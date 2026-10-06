      *================================================================*
      * PROGRAM  : IRAACCR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : IRA CUSTODY - DAILY ACCRUAL. READS THE IRA          *
      *            TRANSACTIONS, UPDATES THE IRA MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. INDIVIDUAL RETIREMENT ACCOUNT   *
      *            CONTRIBUTIONS, DISTRIBUTIONS AND 5498               *
      * INPUT    : IRAMST (KSDS), IRATRN, BANKCTL                      *
      * OUTPUT   : IRAOUT, IRAACRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, IRELG, IRPRC, IRVAL     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-06-14 PQR  ORIGINAL                                       *
      * 1996-02-17 TWH  RMD RERUN PROTECTION PRB0867                   *
      * 2002-05-16 BHS  DISTRIB FEE SCHEDULE UPDATE PRB1712            *
      * 2007-02-08 LMB  DISTRIB LIMIT REVISED PRB7779                  *
      * 2007-11-07 GAF  FMV RERUN PROTECTION INC0306                   *
      * 2011-03-03 SKN  PERFORMANCE - DISTRIB READ AHEAD PRB1041       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IRAACCR.
       AUTHOR. M K LOWE.

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
           SELECT REPORT-FILE ASSIGN TO "IRAACRPT"
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
                                           VALUE 23.000000.
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
      * WS-CONTRIB-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONTRIB-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000646000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000267000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000373500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003050000000160500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003300000000514000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005800000000196000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006300000000575000'.
       01  WS-CONTRIB-TIER-TBL REDEFINES WS-CONTRIB-TIER-VALUES.
           05  WS-CONTRIB-TIER-ROW OCCURS 7 TIMES.
               10  WS-CONTRIB-TIER-LOW     PIC 9(09)V99.
               10  WS-CONTRIB-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ROLLOVER-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ROLLOVER-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000878000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000639000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000683000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000732500'.
       01  WS-ROLLOVER-SCHED-TBL REDEFINES WS-ROLLOVER-SCHED-VALUES.
           05  WS-ROLLOVER-SCHED-ROW OCCURS 4 TIMES.
               10  WS-ROLLOVER-SCHED-LOW   PIC 9(09)V99.
               10  WS-ROLLOVER-SCHED-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  IRAACCR  IRA CUSTODY'.
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
       COPY IRELGP.
       COPY IRPRCP.
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
               PERFORM 3000-EDIT-REJECT
               PERFORM 3010-COUNT-SEQ
               PERFORM 3020-ACCRUE-RMD
               PERFORM 3030-LIMIT-CONTRIB
               PERFORM 3040-AGE-MATURITY
               PERFORM 3050-ACCUM-CHANNEL
               PERFORM 3060-CHECK-MATURITY
               PERFORM 3070-PRORATE-DISTRIB
               PERFORM 3080-SET-REGION
               PERFORM 3090-DESCRIBE-REGION
               PERFORM 3100-SELECT-RATE
               PERFORM 3110-AGE-NEXT-DUE
               PERFORM 3120-DESCRIBE-TRAN
               PERFORM 3130-AGE-LAST-ACT
               PERFORM 3140-SELECT-MARGIN
               PERFORM 3150-CHECK-NEXT-REVIEW
               PERFORM 3160-ASSESS-AUTO-RENEW
               PERFORM 3170-ACCUM-CHANNEL
               PERFORM 3180-ACCUM-CHANNEL
               PERFORM 3190-COUNT-CYCLE
               PERFORM 3200-ACCRUE-ROLLOVER
               PERFORM 3210-CHECK-EFF
               PERFORM 3220-SELECT-RATE
               PERFORM 3230-CHECK-EFFECTIVE
               PERFORM 3240-ACCRUE-FMV
               PERFORM 3250-EDIT-MEMO
               PERFORM 3260-FLAG-ITEM
               PERFORM 3270-SELECT-FEE-PCT
               PERFORM 3280-COUNT-RETRY
               PERFORM 3290-FLAG-RMD
               PERFORM 3300-EDIT-CUST-ID
               PERFORM 3310-ACCUM-CHANNEL
               PERFORM 3320-DESCRIBE-REGION
               PERFORM 3330-SET-REGION
               PERFORM 3340-ACCRUE-RATE
               PERFORM 3350-LIMIT-DISTRIB
               PERFORM 3360-PRORATE-EXCESS
               PERFORM 3370-SET-TRAN
               PERFORM 3380-SELECT-MARGIN
               PERFORM 3390-ACCUM-CHANNEL
               PERFORM 3400-ASSESS-REVIEW
               PERFORM 3410-SET-CHANNEL
               PERFORM 3420-COUNT-SEQ
               PERFORM 3430-SPLIT-ROLLOVER
               PERFORM 3440-DESCRIBE-CHANNEL
               PERFORM 3450-CHECK-NOTICE
               PERFORM 3460-COUNT-ITEM
               PERFORM 3470-AGE-LAST-ACT
               PERFORM 3480-SET-DIST-CODE
               PERFORM 3490-ASSESS-REVIEW
               PERFORM 3500-EDIT-MEMO
               PERFORM 3510-DESCRIBE-TYPE
               PERFORM 3520-FLAG-FMV
               PERFORM 3530-SET-REASON
               PERFORM 3540-ASSESS-REVERSAL
               PERFORM 3550-CHECK-EFF
               PERFORM 3560-ASSESS-HOLD
               PERFORM 3570-ASSESS-HOLD
               PERFORM 3580-ASSESS-STOP
               PERFORM 3590-ACCUM-TRAN
               PERFORM 3600-EDIT-OPERATOR
               PERFORM 3610-DESCRIBE-REGION
               PERFORM 3620-SET-REASON
               PERFORM 3630-ACCUM-DIST-CODE
               PERFORM 3640-EDIT-OPERATOR
               PERFORM 3650-SPLIT-DISTRIB
               PERFORM 3660-DESCRIBE-CHANNEL
               PERFORM 3670-PRORATE-FMV
               PERFORM 3680-SPLIT-ROLLOVER
               PERFORM 3690-SPLIT-WITHHOLD
               PERFORM 3700-EDIT-CUST-ID
               PERFORM 3710-LIMIT-DISTRIB
               PERFORM 3720-PRORATE-EXCESS
               PERFORM 3730-DESCRIBE-CHANNEL
               PERFORM 3740-EDIT-REF-NO
               PERFORM 3750-ASSESS-HOLD
               PERFORM 3760-SET-REASON
               PERFORM 3770-ACCUM-DIST-CODE
               PERFORM 3780-FLAG-DISTRIB
               PERFORM 3790-EDIT-CUST-ID
               PERFORM 3800-ACCUM-TYPE
               PERFORM 3810-ASSESS-SUSPEND
               PERFORM 3820-PRORATE-ROLLOVER
               PERFORM 3830-CHECK-POST
               PERFORM 3840-SET-REGION
               PERFORM 3850-CHECK-OPEN
               PERFORM 3860-DESCRIBE-CHANNEL
               PERFORM 3870-FLAG-ITEM
               PERFORM 3880-LIMIT-FMV
               PERFORM 3890-AGE-NEXT-DUE
               PERFORM 3900-SELECT-FEE-PCT
               PERFORM 3910-LIMIT-ITEM
               PERFORM 3920-CHECK-LAST-ACT
               PERFORM 3930-SPLIT-EXCESS
               PERFORM 3940-EDIT-DESC
               PERFORM 3950-COUNT-PAYMENT
               PERFORM 3960-FLAG-EXCESS
               PERFORM 3970-ASSESS-STOP
               PERFORM 3980-ASSESS-OVERRIDE
               PERFORM 3990-ASSESS-STOP
               PERFORM 4000-PRORATE-ROLLOVER
               PERFORM 4010-PRORATE-DISTRIB
               PERFORM 4020-ACCRUE-EXCESS
               PERFORM 4030-FLAG-CONTRIB
               PERFORM 4040-COUNT-SEQ
               PERFORM 7020-CALL-IRELG
               PERFORM 7000-CALL-IRPRC
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E755 REJECT CD REQUIRED
      * E142 DISTRIB AMT NEGATIVE
      * E294 DISTRIB AMT OVER 10000
      * E579 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3000-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-REJECT-CD = SPACES
                   MOVE 'E755' TO WS-REJECT-CODE
               WHEN IRAM-DISTRIB-AMT < 0
                   MOVE 'E142' TO WS-REJECT-CODE
               WHEN IRAM-DISTRIB-AMT > 10000.00
                   MOVE 'E294' TO WS-REJECT-CODE
               WHEN IRAT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E579' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-SEQ.
           IF IRAT-SEQ < 9999999
               ADD 1 TO IRAT-SEQ
           END-IF
           IF IRAT-SEQ NOT < 10
               MOVE 'Y' TO IRAM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RMD AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3020-ACCRUE-RMD.
           IF IRAM-RMD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-RMD-AMT * IRAM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO IRAT-RMD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3030-LIMIT-CONTRIB.
           IF IRAM-CONTRIB-AMT < 10.00
               MOVE 10.00 TO IRAM-CONTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-CONTRIB-AMT > 5000.00
               MOVE 5000.00 TO IRAM-CONTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAT-ROLLOVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD IRAT-ROLLOVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD IRAT-ROLLOVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAT-ROLLOVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAT-ROLLOVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-CHANNEL-CD
                   ADD IRAM-DISTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-DISTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF IRAM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RMD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISTRIB YTD.
      *----------------------------------------------------------------*
       3070-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-DISTRIB-YTD ROUNDED =
                   IRAM-RMD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-DISTRIB-YTD
           END-IF
           ADD IRAM-DISTRIB-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN LAST STMT DATE IS PAST
      * FL -> GY WHEN LAST STMT DATE IS PAST
      * GY -> HU WHEN LAST STMT DATE IS PAST
      * HU -> KA WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3080-SET-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3090-DESCRIBE-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ROLLOVER SCHED TIER FOR THE WITHHOLD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3100-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ROLLOVER-SCHED-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF IRAM-WITHHOLD-YTD NOT < WS-ROLLOVER-SCHED-LOW(WS-TX)
                   MOVE WS-ROLLOVER-SCHED-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3110-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-DISTRIB-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-DISTRIB-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRAM-DISTRIB-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD IRAM-DISTRIB-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-DISTRIB-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3120-DESCRIBE-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RO'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-LAST-ACT.
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
                   ADD IRAM-EXCESS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-EXCESS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-EXCESS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD IRAM-EXCESS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-EXCESS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3140-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIER-RATE(1) TO IRAM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT-2 NOT < WS-CONTRIB-TIER-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIER-RATE(WS-TX) TO IRAM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO IRAM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCESS FEE ON THE WITHHOLD YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3160-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN IRAM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO IRAT-EXCESS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-STOP-FLAG = 'Y'
                   COMPUTE IRAT-EXCESS-AMT ROUNDED = IRAM-WITHHOLD-YTD *
                       IRAM-RATE / 200
               WHEN IRAM-IRA-TYPE-CD = 'F'
                   MOVE 3.00 TO IRAT-EXCESS-AMT
               WHEN IRAM-IRA-TYPE-CD = 'V'
                   MOVE 12.00 TO IRAT-EXCESS-AMT
               WHEN OTHER
                   COMPUTE IRAT-EXCESS-AMT ROUNDED = IRAM-WITHHOLD-YTD *
                       IRAM-RATE / 100
           END-EVALUATE
           IF IRAT-EXCESS-AMT > 0 AND IRAT-EXCESS-AMT < 1.00
               MOVE 1.00 TO IRAT-EXCESS-AMT
           END-IF
           IF IRAT-EXCESS-AMT > 100.00
               MOVE 100.00 TO IRAT-EXCESS-AMT
           END-IF
           ADD IRAT-EXCESS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE ROLLOVER AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-CHANNEL-CD
                   ADD IRAT-ROLLOVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAT-ROLLOVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ROLLOVER AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-CHANNEL-CD
                   ADD IRAT-ROLLOVER-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAT-ROLLOVER-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-CYCLE.
           IF IRAM-CYCLE-CNT < 9999999
               ADD 1 TO IRAM-CYCLE-CNT
           END-IF
           IF IRAM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO IRAM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROLLOVER AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-ROLLOVER.
           IF IRAM-ROLLOVER-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-ROLLOVER-AMT * IRAM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO IRAM-EXCESS-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF IRAT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO IRAM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIER TIER FOR THE RMD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3220-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIER-RATE(1) TO IRAM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF IRAM-RMD-YTD NOT < WS-CONTRIB-TIER-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIER-RATE(WS-TX) TO IRAM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3230-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FMV YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-FMV.
           IF IRAM-FMV-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-FMV-YTD * IRAM-RATE / 100 / 360
               ADD WS-WORK-AMT TO IRAT-RMD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E043 MEMO REQUIRED
      * E406 EXCESS AMT NEGATIVE
      * E834 EXCESS AMT OVER 5000
      * E060 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-MEMO = SPACES
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN IRAT-EXCESS-AMT < 0
                   MOVE 'E406' TO WS-REJECT-CODE
               WHEN IRAT-EXCESS-AMT > 5000.00
                   MOVE 'E834' TO WS-REJECT-CODE
               WHEN IRAM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E060' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3260-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIER-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT NOT < WS-CONTRIB-TIER-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIER-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-RETRY.
           IF IRAM-RETRY-CNT < 9999999
               ADD 1 TO IRAM-RETRY-CNT
           END-IF
           IF IRAM-RETRY-CNT NOT < 5
               MOVE 'Y' TO IRAM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RMD AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3290-FLAG-RMD.
           IF IRAT-RMD-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAT-RMD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E412 CUST ID REQUIRED
      * E220 CONTRIB AMT NEGATIVE
      * E739 CONTRIB AMT OVER 1000000
      * E288 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3300-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-CUST-ID = SPACES
                   MOVE 'E412' TO WS-REJECT-CODE
               WHEN IRAM-CONTRIB-AMT < 0
                   MOVE 'E220' TO WS-REJECT-CODE
               WHEN IRAM-CONTRIB-AMT > 1000000.00
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN IRAM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E288' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RMD AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3310-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-CHANNEL-CD
                   ADD IRAT-RMD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAT-RMD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3320-DESCRIBE-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'HU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN LAST STMT DATE IS PAST
      * FL -> GY WHEN LAST STMT DATE IS PAST
      * GY -> HU WHEN LAST STMT DATE IS PAST
      * HU -> KA WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3330-SET-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3340-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * IRAM-RATE / 100 / 365
               ADD WS-WORK-AMT TO IRAT-EXCESS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3350-LIMIT-DISTRIB.
           IF IRAM-DISTRIB-YTD < 0.00
               MOVE 0.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-DISTRIB-YTD > 50000.00
               MOVE 50000.00 TO IRAM-DISTRIB-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ROLLOVER YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EXCESS YTD.
      *----------------------------------------------------------------*
       3360-PRORATE-EXCESS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-EXCESS-YTD ROUNDED =
                   IRAM-ROLLOVER-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-EXCESS-YTD
           END-IF
           ADD IRAM-EXCESS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CN -> DS WHEN NOTICE DATE IS PAST
      * DS -> RO WHEN NOTICE DATE IS PAST
      * RO -> RC WHEN NOTICE DATE IS PAST
      * RC -> WH WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3370-SET-TRAN.
           EVALUATE IRAT-TRAN-CD
               WHEN 'CN'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DS' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DS'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RO' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RO'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF IRAM-NOTICE-DATE > 0 AND IRAM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO IRAT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE ROLLOVER SCHED TIER FOR THE DISTRIB YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-ROLLOVER-SCHED-RATE(1) TO IRAM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF IRAM-DISTRIB-YTD NOT < WS-ROLLOVER-SCHED-LOW(WS-TX)
                   MOVE WS-ROLLOVER-SCHED-RATE(WS-TX) TO IRAM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISTRIB AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3390-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-CHANNEL-CD
                   ADD IRAM-DISTRIB-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-DISTRIB-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ROLLOVER FEE ON THE FMV YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3400-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO IRAM-ROLLOVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-STOP-FLAG = 'Y'
                   COMPUTE IRAM-ROLLOVER-AMT ROUNDED = IRAM-FMV-YTD *
                       IRAM-FEE-PCT / 200
               WHEN IRAM-IRA-TYPE-CD = 'F'
                   MOVE 5.00 TO IRAM-ROLLOVER-AMT
               WHEN IRAM-IRA-TYPE-CD = 'V'
                   MOVE 5.00 TO IRAM-ROLLOVER-AMT
               WHEN OTHER
                   COMPUTE IRAM-ROLLOVER-AMT ROUNDED = IRAM-FMV-YTD *
                       IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAM-ROLLOVER-AMT > 0 AND IRAM-ROLLOVER-AMT < 5.00
               MOVE 5.00 TO IRAM-ROLLOVER-AMT
           END-IF
           IF IRAM-ROLLOVER-AMT > 150.00
               MOVE 150.00 TO IRAM-ROLLOVER-AMT
           END-IF
           ADD IRAM-ROLLOVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST STMT DATE IS PAST
      * BAT -> MAL WHEN LAST STMT DATE IS PAST
      * MAL -> TEL WHEN LAST STMT DATE IS PAST
      * TEL -> ONL WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3410-SET-CHANNEL.
           EVALUATE IRAT-CHANNEL-CD
               WHEN 'BRN'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF IRAM-LAST-STMT-DATE > 0 AND IRAM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO IRAT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-SEQ.
           IF IRAT-SEQ < 9999999
               ADD 1 TO IRAT-SEQ
           END-IF
           IF IRAT-SEQ NOT < 5
               MOVE 'Y' TO IRAM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRAM-ROLLOVER-YTD / 2
           COMPUTE WS-WORK-AMT-2 = IRAM-ROLLOVER-YTD - (WS-WORK-AMT * 2)
           COMPUTE IRAM-DISTRIB-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3440-DESCRIBE-CHANNEL.
           EVALUATE IRAT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-NOTICE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO IRAM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3460-COUNT-ITEM.
           IF IRAM-ITEM-CNT < 9999999
               ADD 1 TO IRAM-ITEM-CNT
           END-IF
           IF IRAM-ITEM-CNT NOT < 3
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-LAST-ACT.
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
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-ROLLOVER-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DIST CODE CD:
      * C -> J WHEN MATURITY DATE IS PAST
      * J -> U WHEN MATURITY DATE IS PAST
      * U -> X WHEN MATURITY DATE IS PAST
      * X -> C WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-DIST-CODE.
           EVALUATE IRAM-DIST-CODE-CD
               WHEN 'C'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'C' TO IRAM-DIST-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB FEE ON THE ROLLOVER YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3490-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO IRAM-CONTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-STOP-FLAG = 'Y'
                   COMPUTE IRAM-CONTRIB-AMT ROUNDED = IRAM-ROLLOVER-YTD
                       * IRAM-FEE-PCT / 200
               WHEN IRAM-DIST-CODE-CD = 'C'
                   MOVE 5.00 TO IRAM-CONTRIB-AMT
               WHEN IRAM-DIST-CODE-CD = 'J'
                   MOVE 7.00 TO IRAM-CONTRIB-AMT
               WHEN OTHER
                   COMPUTE IRAM-CONTRIB-AMT ROUNDED = IRAM-ROLLOVER-YTD
                       * IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAM-CONTRIB-AMT > 0 AND IRAM-CONTRIB-AMT < 10.00
               MOVE 10.00 TO IRAM-CONTRIB-AMT
           END-IF
           IF IRAM-CONTRIB-AMT > 25.00
               MOVE 25.00 TO IRAM-CONTRIB-AMT
           END-IF
           ADD IRAM-CONTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E627 MEMO REQUIRED
      * E572 WITHHOLD YTD NEGATIVE
      * E887 WITHHOLD YTD OVER 5000
      * E765 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3500-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-MEMO = SPACES
                   MOVE 'E627' TO WS-REJECT-CODE
               WHEN IRAM-WITHHOLD-YTD < 0
                   MOVE 'E572' TO WS-REJECT-CODE
               WHEN IRAM-WITHHOLD-YTD > 5000.00
                   MOVE 'E887' TO WS-REJECT-CODE
               WHEN IRAM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-TYPE.
           EVALUATE IRAM-TYPE-CD
               WHEN 'EA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FMV AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3520-FLAG-FMV.
           IF IRAM-FMV-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE IRAM-FMV-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * CMR -> LJS WHEN MATURITY DATE IS PAST
      * LJS -> NRT WHEN MATURITY DATE IS PAST
      * NRT -> UGA WHEN MATURITY DATE IS PAST
      * UGA -> WBS WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3530-SET-REASON.
           EVALUATE IRAM-REASON-CD
               WHEN 'CMR'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LJS' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LJS'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NRT' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NRT'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UGA' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGA'
                   IF IRAM-MATURITY-DATE > 0 AND IRAM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WBS' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB FEE ON THE RMD AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3540-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN IRAT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO IRAM-CONTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-STOP-FLAG = 'Y'
                   COMPUTE IRAM-CONTRIB-AMT ROUNDED = IRAT-RMD-AMT *
                       IRAM-FEE-PCT / 200
               WHEN IRAT-TRAN-CD = 'CN'
                   MOVE 5.00 TO IRAM-CONTRIB-AMT
               WHEN IRAT-TRAN-CD = 'DS'
                   MOVE 15.00 TO IRAM-CONTRIB-AMT
               WHEN OTHER
                   COMPUTE IRAM-CONTRIB-AMT ROUNDED = IRAT-RMD-AMT *
                       IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAM-CONTRIB-AMT > 0 AND IRAM-CONTRIB-AMT < 5.00
               MOVE 5.00 TO IRAM-CONTRIB-AMT
           END-IF
           IF IRAM-CONTRIB-AMT > 75.00
               MOVE 75.00 TO IRAM-CONTRIB-AMT
           END-IF
           ADD IRAM-CONTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3550-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF IRAT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RMD FEE ON THE CONTRIB AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3560-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN IRAM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO IRAM-RMD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-STOP-FLAG = 'Y'
                   COMPUTE IRAM-RMD-AMT ROUNDED = IRAM-CONTRIB-AMT *
                       IRAM-FEE-PCT / 200
               WHEN IRAM-IRA-TYPE-CD = 'F'
                   MOVE 10.00 TO IRAM-RMD-AMT
               WHEN IRAM-IRA-TYPE-CD = 'V'
                   MOVE 15.00 TO IRAM-RMD-AMT
               WHEN OTHER
                   COMPUTE IRAM-RMD-AMT ROUNDED = IRAM-CONTRIB-AMT *
                       IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAM-RMD-AMT > 0 AND IRAM-RMD-AMT < 10.00
               MOVE 10.00 TO IRAM-RMD-AMT
           END-IF
           IF IRAM-RMD-AMT > 25.00
               MOVE 25.00 TO IRAM-RMD-AMT
           END-IF
           ADD IRAM-RMD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE DISTRIB FEE ON THE ROLLOVER YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3570-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN IRAM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO IRAM-DISTRIB-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   COMPUTE IRAM-DISTRIB-AMT ROUNDED = IRAM-ROLLOVER-YTD
                       * IRAM-MARGIN / 200
               WHEN IRAM-DIST-CODE-CD = 'C'
                   MOVE 10.00 TO IRAM-DISTRIB-AMT
               WHEN IRAM-DIST-CODE-CD = 'J'
                   MOVE 20.00 TO IRAM-DISTRIB-AMT
               WHEN OTHER
                   COMPUTE IRAM-DISTRIB-AMT ROUNDED = IRAM-ROLLOVER-YTD
                       * IRAM-MARGIN / 100
           END-EVALUATE
           IF IRAM-DISTRIB-AMT > 0 AND IRAM-DISTRIB-AMT < 2.00
               MOVE 2.00 TO IRAM-DISTRIB-AMT
           END-IF
           IF IRAM-DISTRIB-AMT > 150.00
               MOVE 150.00 TO IRAM-DISTRIB-AMT
           END-IF
           ADD IRAM-DISTRIB-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE FMV YTD FEE ON THE CONTRIB YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3580-ASSESS-STOP.
           EVALUATE TRUE
               WHEN IRAM-STOP-FLAG = 'Y'
                   MOVE ZERO TO IRAM-FMV-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-OVERRIDE-FLAG = 'Y'
                   COMPUTE IRAM-FMV-YTD ROUNDED = IRAM-CONTRIB-YTD *
                       IRAM-MARGIN / 200
               WHEN IRAM-TYPE-CD = 'EA'
                   MOVE 7.00 TO IRAM-FMV-YTD
               WHEN IRAM-TYPE-CD = 'HR'
                   MOVE 5.00 TO IRAM-FMV-YTD
               WHEN OTHER
                   COMPUTE IRAM-FMV-YTD ROUNDED = IRAM-CONTRIB-YTD *
                       IRAM-MARGIN / 100
           END-EVALUATE
           IF IRAM-FMV-YTD > 0 AND IRAM-FMV-YTD < 2.00
               MOVE 2.00 TO IRAM-FMV-YTD
           END-IF
           IF IRAM-FMV-YTD > 25.00
               MOVE 25.00 TO IRAM-FMV-YTD
           END-IF
           ADD IRAM-FMV-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3590-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAT-TRAN-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E974 OPERATOR REQUIRED
      * E772 ROLLOVER AMT NEGATIVE
      * E027 ROLLOVER AMT OVER 250000
      * E245 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3600-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-OPERATOR = SPACES
                   MOVE 'E974' TO WS-REJECT-CODE
               WHEN IRAT-ROLLOVER-AMT < 0
                   MOVE 'E772' TO WS-REJECT-CODE
               WHEN IRAT-ROLLOVER-AMT > 250000.00
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN IRAM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E245' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3610-DESCRIBE-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * CMR -> LJS WHEN EFF DATE IS PAST
      * LJS -> NRT WHEN EFF DATE IS PAST
      * NRT -> UGA WHEN EFF DATE IS PAST
      * UGA -> WBS WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-REASON.
           EVALUATE IRAM-REASON-CD
               WHEN 'CMR'
                   IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LJS' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LJS'
                   IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NRT' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NRT'
                   IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UGA' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGA'
                   IF IRAT-EFF-DATE > 0 AND IRAT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WBS' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RMD YTD BY DIST CODE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3630-ACCUM-DIST-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-DIST-CODE-CD
                   ADD IRAM-RMD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-DIST-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-RMD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E321 OPERATOR REQUIRED
      * E030 WITHHOLD YTD NEGATIVE
      * E478 WITHHOLD YTD OVER 50000
      * E144 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAT-OPERATOR = SPACES
                   MOVE 'E321' TO WS-REJECT-CODE
               WHEN IRAM-WITHHOLD-YTD < 0
                   MOVE 'E030' TO WS-REJECT-CODE
               WHEN IRAM-WITHHOLD-YTD > 50000.00
                   MOVE 'E478' TO WS-REJECT-CODE
               WHEN IRAM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISTRIB YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3650-SPLIT-DISTRIB.
           COMPUTE WS-WORK-AMT = IRAM-DISTRIB-YTD / 12
           COMPUTE WS-WORK-AMT-2 = IRAM-DISTRIB-YTD - (WS-WORK-AMT * 12)
           COMPUTE IRAM-ROLLOVER-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3660-DESCRIBE-CHANNEL.
           EVALUATE IRAT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FMV AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-FMV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-FMV-AMT ROUNDED =
                   IRAM-CONTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAM-FMV-AMT
           END-IF
           ADD IRAM-FMV-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT ROLLOVER AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3680-SPLIT-ROLLOVER.
           COMPUTE WS-WORK-AMT = IRAM-ROLLOVER-AMT / 3
           COMPUTE WS-WORK-AMT-2 = IRAM-ROLLOVER-AMT - (WS-WORK-AMT * 3)
           COMPUTE IRAM-FMV-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WITHHOLD YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3690-SPLIT-WITHHOLD.
           COMPUTE WS-WORK-AMT = IRAM-WITHHOLD-YTD / 2
           COMPUTE WS-WORK-AMT-2 = IRAM-WITHHOLD-YTD - (WS-WORK-AMT * 2)
           COMPUTE IRAM-RMD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E406 CUST ID REQUIRED
      * E617 EXCESS YTD NEGATIVE
      * E843 EXCESS YTD OVER 50000
      * E041 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-CUST-ID = SPACES
                   MOVE 'E406' TO WS-REJECT-CODE
               WHEN IRAM-EXCESS-YTD < 0
                   MOVE 'E617' TO WS-REJECT-CODE
               WHEN IRAM-EXCESS-YTD > 50000.00
                   MOVE 'E843' TO WS-REJECT-CODE
               WHEN IRAM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E041' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3710-LIMIT-DISTRIB.
           IF IRAM-DISTRIB-AMT < 100.00
               MOVE 100.00 TO IRAM-DISTRIB-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-DISTRIB-AMT > 500.00
               MOVE 500.00 TO IRAM-DISTRIB-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EXCESS AMT.
      *----------------------------------------------------------------*
       3720-PRORATE-EXCESS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAT-EXCESS-AMT ROUNDED =
                   IRAM-DISTRIB-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAT-EXCESS-AMT
           END-IF
           ADD IRAT-EXCESS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-CHANNEL.
           EVALUATE IRAM-CHANNEL-CD
               WHEN 'AS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'AY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E531 REF NO REQUIRED
      * E948 RMD YTD NEGATIVE
      * E633 RMD YTD OVER 10000
      * E296 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3740-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-REF-NO = SPACES
                   MOVE 'E531' TO WS-REJECT-CODE
               WHEN IRAM-RMD-YTD < 0
                   MOVE 'E948' TO WS-REJECT-CODE
               WHEN IRAM-RMD-YTD > 10000.00
                   MOVE 'E633' TO WS-REJECT-CODE
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
      * ASSESS THE WITHHOLD YTD FEE ON THE RMD YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3750-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN IRAM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO IRAM-WITHHOLD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE IRAM-WITHHOLD-YTD ROUNDED = IRAM-RMD-YTD *
                       IRAM-MARGIN / 200
               WHEN IRAM-TYPE-CD = 'EA'
                   MOVE 3.00 TO IRAM-WITHHOLD-YTD
               WHEN IRAM-TYPE-CD = 'HR'
                   MOVE 5.00 TO IRAM-WITHHOLD-YTD
               WHEN OTHER
                   COMPUTE IRAM-WITHHOLD-YTD ROUNDED = IRAM-RMD-YTD *
                       IRAM-MARGIN / 100
           END-EVALUATE
           IF IRAM-WITHHOLD-YTD > 0 AND IRAM-WITHHOLD-YTD < 1.00
               MOVE 1.00 TO IRAM-WITHHOLD-YTD
           END-IF
           IF IRAM-WITHHOLD-YTD > 150.00
               MOVE 150.00 TO IRAM-WITHHOLD-YTD
           END-IF
           ADD IRAM-WITHHOLD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * CMR -> LJS WHEN EFFECTIVE DATE IS PAST
      * LJS -> NRT WHEN EFFECTIVE DATE IS PAST
      * NRT -> UGA WHEN EFFECTIVE DATE IS PAST
      * UGA -> WBS WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-REASON.
           EVALUATE IRAM-REASON-CD
               WHEN 'CMR'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LJS' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LJS'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NRT' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NRT'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UGA' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UGA'
                   IF IRAM-EFFECTIVE-DATE > 0 AND IRAM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WBS' TO IRAM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FMV AMT BY DIST CODE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3770-ACCUM-DIST-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = IRAM-DIST-CODE-CD
                   ADD IRAM-FMV-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE IRAM-DIST-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE IRAM-FMV-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DISTRIB YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3780-FLAG-DISTRIB.
           IF IRAM-DISTRIB-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE IRAM-DISTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E066 CUST ID REQUIRED
      * E453 CONTRIB YTD NEGATIVE
      * E016 CONTRIB YTD OVER 5000
      * E384 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-CUST-ID = SPACES
                   MOVE 'E066' TO WS-REJECT-CODE
               WHEN IRAM-CONTRIB-YTD < 0
                   MOVE 'E453' TO WS-REJECT-CODE
               WHEN IRAM-CONTRIB-YTD > 5000.00
                   MOVE 'E016' TO WS-REJECT-CODE
               WHEN IRAM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E384' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONTRIB AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3800-ACCUM-TYPE.
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
      * ASSESS THE FMV FEE ON THE EXCESS AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3810-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN IRAM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO IRAM-FMV-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-REVIEW-FLAG = 'Y'
                   COMPUTE IRAM-FMV-AMT ROUNDED = IRAM-EXCESS-AMT *
                       IRAM-MARGIN / 200
               WHEN IRAM-REGION-CD = 'CE'
                   MOVE 3.00 TO IRAM-FMV-AMT
               WHEN IRAM-REGION-CD = 'FL'
                   MOVE 15.00 TO IRAM-FMV-AMT
               WHEN OTHER
                   COMPUTE IRAM-FMV-AMT ROUNDED = IRAM-EXCESS-AMT *
                       IRAM-MARGIN / 100
           END-EVALUATE
           IF IRAM-FMV-AMT > 0 AND IRAM-FMV-AMT < 2.00
               MOVE 2.00 TO IRAM-FMV-AMT
           END-IF
           IF IRAM-FMV-AMT > 25.00
               MOVE 25.00 TO IRAM-FMV-AMT
           END-IF
           ADD IRAM-FMV-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE ROLLOVER AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ROLLOVER YTD.
      *----------------------------------------------------------------*
       3820-PRORATE-ROLLOVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-ROLLOVER-YTD ROUNDED =
                   IRAT-ROLLOVER-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO IRAM-ROLLOVER-YTD
           END-IF
           ADD IRAM-ROLLOVER-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3830-CHECK-POST.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO IRAM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * CE -> FL WHEN NEXT REVIEW DATE IS PAST
      * FL -> GY WHEN NEXT REVIEW DATE IS PAST
      * GY -> HU WHEN NEXT REVIEW DATE IS PAST
      * HU -> KA WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-REGION.
           EVALUATE IRAM-REGION-CD
               WHEN 'CE'
                   IF IRAM-NEXT-REVIEW-DATE > 0 AND
                       IRAM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'FL' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF IRAM-NEXT-REVIEW-DATE > 0 AND
                       IRAM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GY' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GY'
                   IF IRAM-NEXT-REVIEW-DATE > 0 AND
                       IRAM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HU' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HU'
                   IF IRAM-NEXT-REVIEW-DATE > 0 AND
                       IRAM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KA' TO IRAM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF IRAM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO IRAM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3860-DESCRIBE-CHANNEL.
           EVALUATE IRAM-CHANNEL-CD
               WHEN 'AS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'AY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3870-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FMV AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3880-LIMIT-FMV.
           IF IRAM-FMV-AMT < 10.00
               MOVE 10.00 TO IRAM-FMV-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF IRAM-FMV-AMT > 1000.00
               MOVE 1000.00 TO IRAM-FMV-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF IRAM-NEXT-DUE-DATE > 0 AND IRAM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE IRAM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD IRAM-CONTRIB-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD IRAM-CONTRIB-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD IRAM-CONTRIB-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD IRAM-CONTRIB-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD IRAM-CONTRIB-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONTRIB TIER TIER FOR THE RMD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3900-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONTRIB-TIER-RATE(1) TO IRAM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF IRAT-RMD-AMT NOT < WS-CONTRIB-TIER-LOW(WS-TX)
                   MOVE WS-CONTRIB-TIER-RATE(WS-TX) TO IRAM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF IRAM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO IRAM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3910-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-LAST-ACT.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO IRAT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EXCESS AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3930-SPLIT-EXCESS.
           COMPUTE WS-WORK-AMT = IRAT-EXCESS-AMT / 3
           COMPUTE WS-WORK-AMT-2 = IRAT-EXCESS-AMT - (WS-WORK-AMT * 3)
           COMPUTE IRAM-FMV-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E325 DESC REQUIRED
      * E730 RMD AMT NEGATIVE
      * E405 RMD AMT OVER 50000
      * E395 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3940-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN IRAM-DESC = SPACES
                   MOVE 'E325' TO WS-REJECT-CODE
               WHEN IRAM-RMD-AMT < 0
                   MOVE 'E730' TO WS-REJECT-CODE
               WHEN IRAM-RMD-AMT > 50000.00
                   MOVE 'E405' TO WS-REJECT-CODE
               WHEN IRAM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E395' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3950-COUNT-PAYMENT.
           IF IRAM-PAYMENT-CNT < 9999999
               ADD 1 TO IRAM-PAYMENT-CNT
           END-IF
           IF IRAM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO IRAM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCESS AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3960-FLAG-EXCESS.
           IF IRAM-EXCESS-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE IRAM-EXCESS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB YTD FEE ON THE ROLLOVER AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3970-ASSESS-STOP.
           EVALUATE TRUE
               WHEN IRAM-STOP-FLAG = 'Y'
                   MOVE ZERO TO IRAM-CONTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAM-HOLD-FLAG = 'Y'
                   COMPUTE IRAM-CONTRIB-YTD ROUNDED = IRAM-ROLLOVER-AMT
                       * IRAM-FEE-PCT / 200
               WHEN IRAT-TRAN-CD = 'CN'
                   MOVE 15.00 TO IRAM-CONTRIB-YTD
               WHEN IRAT-TRAN-CD = 'DS'
                   MOVE 12.00 TO IRAM-CONTRIB-YTD
               WHEN OTHER
                   COMPUTE IRAM-CONTRIB-YTD ROUNDED = IRAM-ROLLOVER-AMT
                       * IRAM-FEE-PCT / 100
           END-EVALUATE
           IF IRAM-CONTRIB-YTD > 0 AND IRAM-CONTRIB-YTD < 10.00
               MOVE 10.00 TO IRAM-CONTRIB-YTD
           END-IF
           IF IRAM-CONTRIB-YTD > 100.00
               MOVE 100.00 TO IRAM-CONTRIB-YTD
           END-IF
           ADD IRAM-CONTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CONTRIB YTD FEE ON THE ROLLOVER AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3980-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN IRAM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO IRAM-CONTRIB-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAT-REVERSAL-FLAG = 'Y'
                   COMPUTE IRAM-CONTRIB-YTD ROUNDED = IRAT-ROLLOVER-AMT
                       * IRAM-MARGIN / 200
               WHEN IRAT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO IRAM-CONTRIB-YTD
               WHEN IRAT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO IRAM-CONTRIB-YTD
               WHEN OTHER
                   COMPUTE IRAM-CONTRIB-YTD ROUNDED = IRAT-ROLLOVER-AMT
                       * IRAM-MARGIN / 100
           END-EVALUATE
           IF IRAM-CONTRIB-YTD > 0 AND IRAM-CONTRIB-YTD < 1.00
               MOVE 1.00 TO IRAM-CONTRIB-YTD
           END-IF
           IF IRAM-CONTRIB-YTD > 100.00
               MOVE 100.00 TO IRAM-CONTRIB-YTD
           END-IF
           ADD IRAM-CONTRIB-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE FMV YTD FEE ON THE FMV AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3990-ASSESS-STOP.
           EVALUATE TRUE
               WHEN IRAM-STOP-FLAG = 'Y'
                   MOVE ZERO TO IRAM-FMV-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN IRAT-REVERSAL-FLAG = 'Y'
                   COMPUTE IRAM-FMV-YTD ROUNDED = IRAM-FMV-AMT *
                       IRAM-MARGIN / 200
               WHEN IRAM-DIST-CODE-CD = 'C'
                   MOVE 15.00 TO IRAM-FMV-YTD
               WHEN IRAM-DIST-CODE-CD = 'J'
                   MOVE 12.00 TO IRAM-FMV-YTD
               WHEN OTHER
                   COMPUTE IRAM-FMV-YTD ROUNDED = IRAM-FMV-AMT *
                       IRAM-MARGIN / 100
           END-EVALUATE
           IF IRAM-FMV-YTD > 0 AND IRAM-FMV-YTD < 2.00
               MOVE 2.00 TO IRAM-FMV-YTD
           END-IF
           IF IRAM-FMV-YTD > 150.00
               MOVE 150.00 TO IRAM-FMV-YTD
           END-IF
           ADD IRAM-FMV-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE CONTRIB AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ROLLOVER YTD.
      *----------------------------------------------------------------*
       4000-PRORATE-ROLLOVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-ROLLOVER-YTD ROUNDED =
                   IRAM-CONTRIB-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-ROLLOVER-YTD
           END-IF
           ADD IRAM-ROLLOVER-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISTRIB AMT.
      *----------------------------------------------------------------*
       4010-PRORATE-DISTRIB.
           IF WS-PERIOD-DAYS > 0
               COMPUTE IRAM-DISTRIB-AMT ROUNDED =
                   IRAM-WITHHOLD-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO IRAM-DISTRIB-AMT
           END-IF
           ADD IRAM-DISTRIB-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON EXCESS AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4020-ACCRUE-EXCESS.
           IF IRAM-EXCESS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   IRAM-EXCESS-AMT * IRAM-RATE / 100 / 365
               ADD WS-WORK-AMT TO IRAT-ROLLOVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONTRIB YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4030-FLAG-CONTRIB.
           IF IRAM-CONTRIB-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE IRAM-CONTRIB-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4040-COUNT-SEQ.
           IF IRAT-SEQ < 9999999
               ADD 1 TO IRAT-SEQ
           END-IF
           IF IRAT-SEQ NOT < 3
               MOVE 'Y' TO IRAM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE IRA FIELDS TO IRELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-IRELG.
           INITIALIZE IRELG-PARMS
           MOVE IRAM-FMV-AMT
             TO IRELG-FMV-AMT
           MOVE IRAM-EXCESS-AMT
             TO IRELG-EXCESS-AMT
           MOVE IRAM-ROLLOVER-YTD
             TO IRELG-ROLLOVER-YTD
           MOVE IRAM-RMD-AMT
             TO IRELG-RMD-AMT
           MOVE IRAM-ROLLOVER-AMT
             TO IRELG-ROLLOVER-AMT
           MOVE IRAM-FMV-YTD
             TO IRELG-FMV-YTD
           MOVE IRAM-LAST-ACT-DATE
             TO IRELG-LAST-ACT-DATE
           MOVE IRAM-LAST-STMT-DATE
             TO IRELG-LAST-STMT-DATE
           MOVE IRAM-EXPIRY-DATE
             TO IRELG-EXPIRY-DATE
           MOVE IRAM-NEXT-REVIEW-DATE
             TO IRELG-NEXT-REVIEW-DATE
           MOVE IRAM-CHANNEL-CD
             TO IRELG-CHANNEL-CD
           MOVE IRAM-REGION-CD
             TO IRELG-REGION-CD
           MOVE IRAM-REASON-CD
             TO IRELG-REASON-CD
           MOVE IRAM-TYPE-CD
             TO IRELG-TYPE-CD
           MOVE IRAM-STOP-FLAG
             TO IRELG-STOP-FLAG
           MOVE IRAM-REVIEW-FLAG
             TO IRELG-REVIEW-FLAG
           MOVE IRAM-AUTO-RENEW-FLAG
             TO IRELG-AUTO-RENEW-FLAG
           MOVE IRAM-CYCLE-CNT
             TO IRELG-CYCLE-CNT
           MOVE IRAM-RETRY-CNT
             TO IRELG-RETRY-CNT
           MOVE IRAM-MARGIN
             TO IRELG-MARGIN
           MOVE IRAM-FEE-PCT
             TO IRELG-FEE-PCT
           MOVE IRAM-ADDR
             TO IRELG-ADDR
           MOVE IRAM-NAME
             TO IRELG-NAME
           CALL 'IRELG' USING IRELG-PARMS
           EVALUATE IRELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE IRELG-FMV-AMT
                     TO IRAM-FMV-AMT
                   MOVE IRELG-EXCESS-AMT
                     TO IRAM-EXCESS-AMT
                   MOVE IRELG-FMV-YTD
                     TO IRAM-FMV-YTD
                   MOVE IRELG-REVIEW-FLAG
                     TO IRAM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-IRELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE IRA FIELDS TO IRPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-IRPRC.
           INITIALIZE IRPRC-PARMS
           MOVE IRAM-RMD-YTD
             TO IRPRC-RMD-YTD
           MOVE IRAM-CONTRIB-YTD
             TO IRPRC-CONTRIB-YTD
           MOVE IRAM-EXCESS-YTD
             TO IRPRC-EXCESS-YTD
           MOVE IRAM-DISTRIB-YTD
             TO IRPRC-DISTRIB-YTD
           MOVE IRAM-FMV-AMT
             TO IRPRC-FMV-AMT
           MOVE IRAM-ROLLOVER-AMT
             TO IRPRC-ROLLOVER-AMT
           MOVE IRAM-NEXT-REVIEW-DATE
             TO IRPRC-NEXT-REVIEW-DATE
           MOVE IRAM-LAST-STMT-DATE
             TO IRPRC-LAST-STMT-DATE
           MOVE IRAM-LAST-ACT-DATE
             TO IRPRC-LAST-ACT-DATE
           MOVE IRAM-EXPIRY-DATE
             TO IRPRC-EXPIRY-DATE
           MOVE IRAM-IRA-TYPE-CD
             TO IRPRC-IRA-TYPE-CD
           MOVE IRAM-TYPE-CD
             TO IRPRC-TYPE-CD
           MOVE IRAM-CHANNEL-CD
             TO IRPRC-CHANNEL-CD
           MOVE IRAM-REASON-CD
             TO IRPRC-REASON-CD
           MOVE IRAM-OVERRIDE-FLAG
             TO IRPRC-OVERRIDE-FLAG
           MOVE IRAM-SUSPEND-FLAG
             TO IRPRC-SUSPEND-FLAG
           MOVE IRAM-AUTO-RENEW-FLAG
             TO IRPRC-AUTO-RENEW-FLAG
           MOVE IRAM-PAYMENT-CNT
             TO IRPRC-PAYMENT-CNT
           MOVE IRAM-RETRY-CNT
             TO IRPRC-RETRY-CNT
           MOVE IRAM-MARGIN
             TO IRPRC-MARGIN
           MOVE IRAM-FEE-PCT
             TO IRPRC-FEE-PCT
           MOVE IRAM-NAME
             TO IRPRC-NAME
           MOVE IRAM-ADDR
             TO IRPRC-ADDR
           CALL 'IRPRC' USING IRPRC-PARMS
           EVALUATE IRPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE IRPRC-DISTRIB-YTD
                     TO IRAM-DISTRIB-YTD
                   MOVE IRPRC-FMV-AMT
                     TO IRAM-FMV-AMT
                   MOVE IRPRC-ROLLOVER-AMT
                     TO IRAM-ROLLOVER-AMT
                   MOVE IRPRC-OVERRIDE-FLAG
                     TO IRAM-OVERRIDE-FLAG
                   MOVE IRPRC-SUSPEND-FLAG
                     TO IRAM-SUSPEND-FLAG
                   MOVE IRPRC-AUTO-RENEW-FLAG
                     TO IRAM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-IRPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

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
                   MOVE IRVAL-EXCESS-AMT
                     TO IRAM-EXCESS-AMT
                   MOVE IRVAL-ROLLOVER-AMT
                     TO IRAM-ROLLOVER-AMT
                   MOVE IRVAL-RMD-YTD
                     TO IRAM-RMD-YTD
                   MOVE IRVAL-FMV-YTD
                     TO IRAM-FMV-YTD
                   MOVE IRVAL-FMV-AMT
                     TO IRAM-FMV-AMT
                   MOVE IRVAL-HOLD-FLAG
                     TO IRAM-HOLD-FLAG
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
           DISPLAY 'IRAACCR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'IRAACCR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
