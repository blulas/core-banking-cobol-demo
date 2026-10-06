      *================================================================*
      * PROGRAM  : PBKRPT                                              *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PASSBOOK PRINTING - MANAGEMENT REPORTING. READS THE *
      *            PBK TRANSACTIONS, UPDATES THE PBK MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. PASSBOOK SAVINGS UPDATE*
      *            PRINTING AND UNPOSTED ITEM CONTROL                  *
      * INPUT    : PBKMST (KSDS), PBKTRN, BANKCTL                      *
      * OUTPUT   : PBKOUT, PBKRPRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PBSTAT, PBPRC, PBDTE    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-12-19 GAF  ORIGINAL                                       *
      * 1998-05-06 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-05-22 PQR  UNPOSTED EXCEPTIONS TO OPS QUEUE CHG2416       *
      * 2001-07-22 RJT  AUDIT FINDING - CARRYFWD CONTROL TOTALS REQ1437*
      * 2003-06-23 TWH  PERFORMANCE - LINE READ AHEAD INC3914          *
      * 2010-01-26 TWH  INTEREST CALCULATION CORRECTED CHG7771         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBKRPT.
       AUTHOR. D P SHAW.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "PBKMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PBKM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "PBKTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "PBKOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "PBKRPRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY PBKMST.
       FD  TRAN-FILE.
       COPY PBKTRN.
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
                                           VALUE 19.000000.
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
      * WS-INTEREST-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000153000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000142000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000703000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000322000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000267000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000249500'.
       01  WS-INTEREST-SCHED-TBL REDEFINES WS-INTEREST-SCHED-VALUES.
           05  WS-INTEREST-SCHED-ROW OCCURS 6 TIMES.
               10  WS-INTEREST-SCHED-LOW   PIC 9(09)V99.
               10  WS-INTEREST-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PRINTED-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINTED-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000244500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000873500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000447500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000850000000169000'.
       01  WS-PRINTED-SCHED-TBL REDEFINES WS-PRINTED-SCHED-VALUES.
           05  WS-PRINTED-SCHED-ROW OCCURS 4 TIMES.
               10  WS-PRINTED-SCHED-LOW    PIC 9(09)V99.
               10  WS-PRINTED-SCHED-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PBKRPT  PASSBOOK PRINTING'.
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
       COPY PBSTATP.
       COPY PBPRCP.
       COPY PBDTEP.

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
               MOVE 'PBKMST' TO ABND-FILE-NAME
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
           MOVE PBKT-KEY TO PBKM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SPLIT-PRINTED
               PERFORM 3010-ASSESS-HOLD
               PERFORM 3020-ASSESS-HOLD
               PERFORM 3030-SPLIT-PRINTED
               PERFORM 3040-SPLIT-UNPOSTED
               PERFORM 3050-LIMIT-BALANCE
               PERFORM 3060-EDIT-NAME
               PERFORM 3070-ACCUM-CHANNEL
               PERFORM 3080-SELECT-PENALTY-PCT
               PERFORM 3090-COUNT-ITEM
               PERFORM 3100-ACCRUE-INTEREST
               PERFORM 3110-SET-SOURCE
               PERFORM 3120-FLAG-CARRYFWD
               PERFORM 3130-SET-BRANCH
               PERFORM 3140-SPLIT-BALANCE
               PERFORM 3150-EDIT-NAME
               PERFORM 3160-ACCUM-BRANCH
               PERFORM 3170-EDIT-DESC
               PERFORM 3180-CHECK-NEXT-REVIEW
               PERFORM 3190-ACCRUE-CARRYFWD
               PERFORM 3200-SET-SOURCE
               PERFORM 3210-LIMIT-LINE
               PERFORM 3220-LIMIT-CARRYFWD
               PERFORM 3230-DESCRIBE-TRAN
               PERFORM 3240-SET-CHANNEL
               PERFORM 3250-PRORATE-UNPOSTED
               PERFORM 3260-LIMIT-PRINTED
               PERFORM 3270-SELECT-FEE-PCT
               PERFORM 3280-ACCRUE-FEE-PCT
               PERFORM 3290-CHECK-EXPIRY
               PERFORM 3300-FLAG-ITEM
               PERFORM 3310-AGE-NEXT-DUE
               PERFORM 3320-ACCUM-CHANNEL
               PERFORM 3330-SPLIT-LINE
               PERFORM 3340-DESCRIBE-REGION
               PERFORM 3350-PRORATE-PRINTED
               PERFORM 3360-SET-BRANCH
               PERFORM 3370-FLAG-INTEREST
               PERFORM 3380-AGE-MATURITY
               PERFORM 3390-SELECT-MARGIN
               PERFORM 3400-ACCRUE-PRINTED
               PERFORM 3410-ASSESS-SUSPEND
               PERFORM 3420-ACCUM-SOURCE
               PERFORM 3430-EDIT-DESC
               PERFORM 3440-PRORATE-PRINTED
               PERFORM 3450-FLAG-BALANCE
               PERFORM 3460-ACCRUE-LINE
               PERFORM 3470-AGE-OPEN
               PERFORM 3480-SET-CHANNEL
               PERFORM 3490-CHECK-MATURITY
               PERFORM 3500-ASSESS-NOTICE-SENT
               PERFORM 3510-AGE-CLOSE
               PERFORM 3520-ACCRUE-INTEREST
               PERFORM 3530-SPLIT-PRINTED
               PERFORM 3540-AGE-MATURITY
               PERFORM 3550-AGE-NEXT-REVIEW
               PERFORM 3560-EDIT-REJECT
               PERFORM 3570-CHECK-NEXT-DUE
               PERFORM 3580-LIMIT-ITEM
               PERFORM 3590-SELECT-MARGIN
               PERFORM 3600-DESCRIBE-CHANNEL
               PERFORM 3610-SET-TRAN
               PERFORM 3620-SPLIT-PRINTED
               PERFORM 3630-EDIT-CUST-ID
               PERFORM 3640-SPLIT-INTEREST
               PERFORM 3650-SET-PRINTER
               PERFORM 3660-LIMIT-UNPOSTED
               PERFORM 3670-COUNT-PAYMENT
               PERFORM 3680-COUNT-ITEM
               PERFORM 3690-DESCRIBE-BRANCH
               PERFORM 3700-ACCRUE-MARGIN
               PERFORM 3710-CHECK-MATURITY
               PERFORM 3720-SET-BRANCH
               PERFORM 3730-ACCUM-CHANNEL
               PERFORM 3740-ASSESS-SUSPEND
               PERFORM 3750-ACCRUE-PRINTED
               PERFORM 3760-PRORATE-LINE
               PERFORM 3770-AGE-MATURITY
               PERFORM 3780-LIMIT-UNPOSTED
               PERFORM 3790-ACCRUE-PENALTY-PCT
               PERFORM 3800-CHECK-MATURITY
               PERFORM 3810-COUNT-RETRY
               PERFORM 3820-DESCRIBE-BOOK-TYPE
               PERFORM 3830-SELECT-FEE-PCT
               PERFORM 3840-DESCRIBE-REGION
               PERFORM 3850-ACCRUE-INTEREST
               PERFORM 3860-PRORATE-PRINTED
               PERFORM 3870-DESCRIBE-BRANCH
               PERFORM 3880-ACCRUE-CARRYFWD
               PERFORM 3890-DESCRIBE-CHANNEL
               PERFORM 3900-SPLIT-UNPOSTED
               PERFORM 3910-FLAG-LINE
               PERFORM 3920-FLAG-INTEREST
               PERFORM 3930-SPLIT-PRINTED
               PERFORM 3940-PRORATE-PRINTED
               PERFORM 3950-ACCUM-SOURCE
               PERFORM 3960-FLAG-ITEM
               PERFORM 3970-LIMIT-UNPOSTED
               PERFORM 3980-SELECT-FEE-PCT
               PERFORM 3990-COUNT-PAYMENT
               PERFORM 4000-FLAG-INTEREST
               PERFORM 4010-EDIT-MEMO
               PERFORM 4020-ACCUM-PRINTER
               PERFORM 4030-ACCUM-PRINTER
               PERFORM 4040-ACCUM-CHANNEL
               PERFORM 4050-EDIT-REJECT
               PERFORM 4060-PRORATE-BALANCE
               PERFORM 4070-SELECT-MARGIN
               PERFORM 4080-AGE-CLOSE
               PERFORM 4090-SELECT-MARGIN
               PERFORM 4100-FLAG-INTEREST
               PERFORM 4110-SELECT-FEE-PCT
               PERFORM 4120-SET-PRINTER
               PERFORM 4130-CHECK-MATURITY
               PERFORM 4140-SET-SOURCE
               PERFORM 4150-EDIT-MEMO
               PERFORM 4160-SELECT-FEE-PCT
               PERFORM 4170-COUNT-SEQ
               PERFORM 4180-EDIT-CUST-ID
               PERFORM 4190-SELECT-PENALTY-PCT
               PERFORM 4200-COUNT-PAYMENT
               PERFORM 4210-CHECK-NEXT-DUE
               PERFORM 4220-SELECT-MARGIN
               PERFORM 4230-AGE-CLOSE
               PERFORM 4240-AGE-OPEN
               PERFORM 4250-ASSESS-EXEMPT
               PERFORM 4260-ASSESS-AUTO-RENEW
               PERFORM 4270-SELECT-FEE-PCT
               PERFORM 4280-EDIT-DESC
               PERFORM 4290-FLAG-INTEREST
               PERFORM 4300-PRORATE-UNPOSTED
               PERFORM 4310-COUNT-SEQ
               PERFORM 4320-SPLIT-CARRYFWD
               PERFORM 4330-CHECK-OPEN
               PERFORM 4340-LIMIT-BALANCE
               PERFORM 4350-ACCRUE-CARRYFWD
               PERFORM 4360-ACCRUE-PRINTED
               PERFORM 4370-SET-TRAN
               PERFORM 4380-EDIT-NAME
               PERFORM 4390-SPLIT-PRINTED
               PERFORM 4400-SPLIT-PRINTED
               PERFORM 4410-FLAG-INTEREST
               PERFORM 4420-COUNT-RETRY
               PERFORM 4430-EDIT-REJECT
               PERFORM 4440-ACCUM-BRANCH
               PERFORM 4450-FLAG-PRINTED
               PERFORM 4460-ASSESS-EXEMPT
               PERFORM 4470-ACCRUE-CARRYFWD
               PERFORM 4480-PRORATE-LINE
               PERFORM 4490-SELECT-PENALTY-PCT
               PERFORM 4500-PRORATE-LINE
               PERFORM 4510-PRORATE-UNPOSTED
               PERFORM 7020-CALL-PBSTAT
               PERFORM 7000-CALL-PBPRC
               PERFORM 7010-CALL-PBDTE
               REWRITE PBK-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'PBKMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM PBK-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * SPLIT PRINTED YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKM-PRINTED-YTD / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-PRINTED-YTD - (WS-WORK-AMT * 4)
           COMPUTE PBKM-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LINE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3010-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PBKM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PBKM-LINE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   COMPUTE PBKM-LINE-YTD ROUNDED = WS-WORK-AMT *
                       PBKM-FEE-PCT / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 15.00 TO PBKM-LINE-YTD
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 15.00 TO PBKM-LINE-YTD
               WHEN OTHER
                   COMPUTE PBKM-LINE-YTD ROUNDED = WS-WORK-AMT *
                       PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-LINE-YTD > 0 AND PBKM-LINE-YTD < 2.00
               MOVE 2.00 TO PBKM-LINE-YTD
           END-IF
           IF PBKM-LINE-YTD > 150.00
               MOVE 150.00 TO PBKM-LINE-YTD
           END-IF
           ADD PBKM-LINE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE UNPOSTED YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3020-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PBKM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PBKM-UNPOSTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   COMPUTE PBKM-UNPOSTED-YTD ROUNDED = WS-WORK-AMT-2 *
                       PBKM-FEE-PCT / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 3.00 TO PBKM-UNPOSTED-YTD
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 20.00 TO PBKM-UNPOSTED-YTD
               WHEN OTHER
                   COMPUTE PBKM-UNPOSTED-YTD ROUNDED = WS-WORK-AMT-2 *
                       PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-UNPOSTED-YTD > 0 AND PBKM-UNPOSTED-YTD < 1.00
               MOVE 1.00 TO PBKM-UNPOSTED-YTD
           END-IF
           IF PBKM-UNPOSTED-YTD > 100.00
               MOVE 100.00 TO PBKM-UNPOSTED-YTD
           END-IF
           ADD PBKM-UNPOSTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PRINTED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3030-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKM-PRINTED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-PRINTED-YTD - (WS-WORK-AMT * 2)
           COMPUTE PBKM-CARRYFWD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT UNPOSTED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-UNPOSTED.
           COMPUTE WS-WORK-AMT = PBKM-UNPOSTED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBKM-UNPOSTED-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-PRINTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-BALANCE.
           IF PBKM-BALANCE-YTD < 10.00
               MOVE 10.00 TO PBKM-BALANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-BALANCE-YTD > 1000.00
               MOVE 1000.00 TO PBKM-BALANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E556 NAME REQUIRED
      * E637 LINE YTD NEGATIVE
      * E054 LINE YTD OVER 25000
      * E100 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3060-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-NAME = SPACES
                   MOVE 'E556' TO WS-REJECT-CODE
               WHEN PBKM-LINE-YTD < 0
                   MOVE 'E637' TO WS-REJECT-CODE
               WHEN PBKM-LINE-YTD > 25000.00
                   MOVE 'E054' TO WS-REJECT-CODE
               WHEN PBKM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E100' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINTED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKT-CHANNEL-CD
                   ADD PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED SCHED TIER FOR THE LINE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-SCHED-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PBKT-LINE-AMT NOT < WS-PRINTED-SCHED-LOW(WS-TX)
                   MOVE WS-PRINTED-SCHED-RATE(WS-TX) TO PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3090-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 3
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3100-ACCRUE-INTEREST.
           IF PBKM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-INTEREST-YTD * PBKM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * JX -> ZC WHEN EXPIRY DATE IS PAST
      * ZC -> ZY WHEN EXPIRY DATE IS PAST
      * ZY -> JX WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-SOURCE.
           EVALUATE PBKM-SOURCE-CD
               WHEN 'JX'
                   IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZC' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZC'
                   IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZY' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZY'
                   IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JX' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CARRYFWD AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3120-FLAG-CARRYFWD.
           IF PBKM-CARRYFWD-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-CARRYFWD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DF -> NV WHEN OPEN DATE IS PAST
      * NV -> PP WHEN OPEN DATE IS PAST
      * PP -> SH WHEN OPEN DATE IS PAST
      * SH -> ZP WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3130-SET-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NV' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NV'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PP'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BALANCE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3140-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = PBKM-BALANCE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PBKM-BALANCE-YTD - (WS-WORK-AMT * 3)
           COMPUTE PBKT-CARRYFWD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E072 NAME REQUIRED
      * E198 PRINTED AMT NEGATIVE
      * E759 PRINTED AMT OVER 5000
      * E961 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3150-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-NAME = SPACES
                   MOVE 'E072' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-AMT < 0
                   MOVE 'E198' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-AMT > 5000.00
                   MOVE 'E759' TO WS-REJECT-CODE
               WHEN PBKM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E961' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINTED AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BRANCH-CD
                   ADD PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E234 DESC REQUIRED
      * E229 LINE YTD NEGATIVE
      * E966 LINE YTD OVER 5000
      * E499 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3170-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-DESC = SPACES
                   MOVE 'E234' TO WS-REJECT-CODE
               WHEN PBKM-LINE-YTD < 0
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN PBKM-LINE-YTD > 5000.00
                   MOVE 'E966' TO WS-REJECT-CODE
               WHEN PBKM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E499' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3180-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PBKM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CARRYFWD AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3190-ACCRUE-CARRYFWD.
           IF PBKT-CARRYFWD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-CARRYFWD-AMT * PBKM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-LINE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * JX -> ZC WHEN OPEN DATE IS PAST
      * ZC -> ZY WHEN OPEN DATE IS PAST
      * ZY -> JX WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3200-SET-SOURCE.
           EVALUATE PBKM-SOURCE-CD
               WHEN 'JX'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZC' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZC'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZY' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZY'
                   IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JX' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LINE YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3210-LIMIT-LINE.
           IF PBKM-LINE-YTD < 0.00
               MOVE 0.00 TO PBKM-LINE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-LINE-YTD > 50000.00
               MOVE 50000.00 TO PBKM-LINE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3220-LIMIT-CARRYFWD.
           IF PBKM-CARRYFWD-AMT < 25.00
               MOVE 25.00 TO PBKM-CARRYFWD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-CARRYFWD-AMT > 50000.00
               MOVE 50000.00 TO PBKM-CARRYFWD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3230-DESCRIBE-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> D WHEN NEXT REVIEW DATE IS PAST
      * D -> H WHEN NEXT REVIEW DATE IS PAST
      * H -> L WHEN NEXT REVIEW DATE IS PAST
      * L -> M WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3240-SET-CHANNEL.
           EVALUATE PBKM-CHANNEL-CD
               WHEN 'B'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'D' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'H' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'L' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE LINE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO UNPOSTED YTD.
      *----------------------------------------------------------------*
       3250-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-UNPOSTED-YTD ROUNDED =
                   PBKM-LINE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-UNPOSTED-YTD
           END-IF
           ADD PBKM-UNPOSTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRINTED AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-PRINTED.
           IF PBKM-PRINTED-AMT < 25.00
               MOVE 25.00 TO PBKM-PRINTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-PRINTED-AMT > 10000.00
               MOVE 10000.00 TO PBKM-PRINTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED SCHED TIER FOR THE UNPOSTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-SCHED-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PBKM-UNPOSTED-AMT NOT < WS-PRINTED-SCHED-LOW(WS-TX)
                   MOVE WS-PRINTED-SCHED-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * PBKM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3290-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PBKT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3300-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3310-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-LINE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3320-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-CHANNEL-CD
                   ADD PBKM-CARRYFWD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-CARRYFWD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LINE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3330-SPLIT-LINE.
           COMPUTE WS-WORK-AMT = PBKM-LINE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBKM-LINE-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-BALANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3340-DESCRIBE-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE UNPOSTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINTED AMT.
      *----------------------------------------------------------------*
       3350-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-PRINTED-AMT ROUNDED =
                   PBKM-UNPOSTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-PRINTED-AMT
           END-IF
           ADD PBKM-PRINTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DF -> NV WHEN CLOSE DATE IS PAST
      * NV -> PP WHEN CLOSE DATE IS PAST
      * PP -> SH WHEN CLOSE DATE IS PAST
      * SH -> ZP WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3360-SET-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NV' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NV'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PP'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3370-FLAG-INTEREST.
           IF PBKT-INTEREST-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKT-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3380-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE CARRYFWD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3390-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PBKM-CARRYFWD-YTD NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINTED YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3400-ACCRUE-PRINTED.
           IF PBKM-PRINTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-PRINTED-YTD * PBKM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-UNPOSTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE INTEREST YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3410-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PBKT-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE PBKT-INTEREST-AMT ROUNDED = PBKM-INTEREST-YTD
                       * PBKM-PENALTY-PCT / 200
               WHEN PBKM-SOURCE-CD = 'JX'
                   MOVE 15.00 TO PBKT-INTEREST-AMT
               WHEN PBKM-SOURCE-CD = 'ZC'
                   MOVE 5.00 TO PBKT-INTEREST-AMT
               WHEN OTHER
                   COMPUTE PBKT-INTEREST-AMT ROUNDED = PBKM-INTEREST-YTD
                       * PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKT-INTEREST-AMT > 0 AND PBKT-INTEREST-AMT < 5.00
               MOVE 5.00 TO PBKT-INTEREST-AMT
           END-IF
           IF PBKT-INTEREST-AMT > 35.00
               MOVE 35.00 TO PBKT-INTEREST-AMT
           END-IF
           ADD PBKT-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3420-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-SOURCE-CD
                   ADD PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-INTEREST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E354 DESC REQUIRED
      * E296 CARRYFWD AMT NEGATIVE
      * E968 CARRYFWD AMT OVER 50000
      * E908 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3430-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-DESC = SPACES
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN PBKM-CARRYFWD-AMT < 0
                   MOVE 'E296' TO WS-REJECT-CODE
               WHEN PBKM-CARRYFWD-AMT > 50000.00
                   MOVE 'E968' TO WS-REJECT-CODE
               WHEN PBKM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E908' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINTED AMT.
      *----------------------------------------------------------------*
       3440-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-PRINTED-AMT ROUNDED =
                   PBKT-CARRYFWD-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-PRINTED-AMT
           END-IF
           ADD PBKM-PRINTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * BALANCE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3450-FLAG-BALANCE.
           IF PBKM-BALANCE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKM-BALANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LINE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3460-ACCRUE-LINE.
           IF PBKM-LINE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-LINE-YTD * PBKM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBKM-CARRYFWD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3490-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PBKM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE FEE ON THE BALANCE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3500-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PBKM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PBKM-BALANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   COMPUTE PBKM-BALANCE-AMT ROUNDED = PBKM-BALANCE-YTD *
                       PBKM-FEE-PCT / 200
               WHEN PBKM-REGION-CD = 'ER'
                   MOVE 5.00 TO PBKM-BALANCE-AMT
               WHEN PBKM-REGION-CD = 'GL'
                   MOVE 20.00 TO PBKM-BALANCE-AMT
               WHEN OTHER
                   COMPUTE PBKM-BALANCE-AMT ROUNDED = PBKM-BALANCE-YTD *
                       PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-BALANCE-AMT > 0 AND PBKM-BALANCE-AMT < 1.00
               MOVE 1.00 TO PBKM-BALANCE-AMT
           END-IF
           IF PBKM-BALANCE-AMT > 25.00
               MOVE 25.00 TO PBKM-BALANCE-AMT
           END-IF
           ADD PBKM-BALANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3510-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-CARRYFWD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-CARRYFWD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-CARRYFWD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-CARRYFWD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-CARRYFWD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3520-ACCRUE-INTEREST.
           IF PBKM-INTEREST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-INTEREST-AMT * PBKM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINTED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKT-PRINTED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PBKT-PRINTED-AMT - (WS-WORK-AMT * 12)
           COMPUTE PBKM-LINE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3540-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-LINE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKM-LINE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-LINE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-LINE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-LINE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-REVIEW-DATE > 0 AND PBKM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-UNPOSTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKM-UNPOSTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-UNPOSTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-UNPOSTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-UNPOSTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E547 REJECT CD REQUIRED
      * E117 UNPOSTED AMT NEGATIVE
      * E624 UNPOSTED AMT OVER 1000000
      * E860 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3560-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-REJECT-CD = SPACES
                   MOVE 'E547' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-AMT < 0
                   MOVE 'E117' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-AMT > 1000000.00
                   MOVE 'E624' TO WS-REJECT-CODE
               WHEN PBKM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E860' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3570-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3580-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED SCHED TIER FOR THE BALANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-SCHED-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PBKM-BALANCE-YTD NOT < WS-PRINTED-SCHED-LOW(WS-TX)
                   MOVE WS-PRINTED-SCHED-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-CHANNEL.
           EVALUATE PBKM-CHANNEL-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * UP -> PR WHEN NEXT DUE DATE IS PAST
      * PR -> RB WHEN NEXT DUE DATE IS PAST
      * RB -> LS WHEN NEXT DUE DATE IS PAST
      * LS -> CF WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF PBKM-NEXT-DUE-DATE > 0 AND PBKM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CF' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINTED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3620-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKM-PRINTED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-PRINTED-AMT - (WS-WORK-AMT * 4)
           COMPUTE PBKM-LINE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E048 CUST ID REQUIRED
      * E083 INTEREST AMT NEGATIVE
      * E139 INTEREST AMT OVER 5000
      * E341 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CUST-ID = SPACES
                   MOVE 'E048' TO WS-REJECT-CODE
               WHEN PBKM-INTEREST-AMT < 0
                   MOVE 'E083' TO WS-REJECT-CODE
               WHEN PBKM-INTEREST-AMT > 5000.00
                   MOVE 'E139' TO WS-REJECT-CODE
               WHEN PBKT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E341' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3640-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBKM-INTEREST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PBKM-INTEREST-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PRINTER CD:
      * CB -> EP WHEN LAST ACT DATE IS PAST
      * EP -> EX WHEN LAST ACT DATE IS PAST
      * EX -> JE WHEN LAST ACT DATE IS PAST
      * JE -> MR WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3650-SET-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EP' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EP'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EX' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JE' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * UNPOSTED YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3660-LIMIT-UNPOSTED.
           IF PBKM-UNPOSTED-YTD < 50.00
               MOVE 50.00 TO PBKM-UNPOSTED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-UNPOSTED-YTD > 2500.00
               MOVE 2500.00 TO PBKM-UNPOSTED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3680-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 10
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3690-DESCRIBE-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3700-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PBKM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO PBKM-PRINTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3710-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DF -> NV WHEN NEXT REVIEW DATE IS PAST
      * NV -> PP WHEN NEXT REVIEW DATE IS PAST
      * PP -> SH WHEN NEXT REVIEW DATE IS PAST
      * SH -> ZP WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NV' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NV'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PP'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SH' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3730-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE LINE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3740-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PBKT-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   COMPUTE PBKT-INTEREST-AMT ROUNDED = PBKM-LINE-AMT *
                       PBKM-FEE-PCT / 200
               WHEN PBKM-BOOK-TYPE-CD = 'CH'
                   MOVE 10.00 TO PBKT-INTEREST-AMT
               WHEN PBKM-BOOK-TYPE-CD = 'HG'
                   MOVE 20.00 TO PBKT-INTEREST-AMT
               WHEN OTHER
                   COMPUTE PBKT-INTEREST-AMT ROUNDED = PBKM-LINE-AMT *
                       PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKT-INTEREST-AMT > 0 AND PBKT-INTEREST-AMT < 2.00
               MOVE 2.00 TO PBKT-INTEREST-AMT
           END-IF
           IF PBKT-INTEREST-AMT > 100.00
               MOVE 100.00 TO PBKT-INTEREST-AMT
           END-IF
           ADD PBKT-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINTED AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3750-ACCRUE-PRINTED.
           IF PBKT-PRINTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-PRINTED-AMT * PBKM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-CARRYFWD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LINE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3760-PRORATE-LINE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-LINE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3770-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-MATURITY-DATE > 0 AND PBKM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * UNPOSTED AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-UNPOSTED.
           IF PBKM-UNPOSTED-AMT < 50.00
               MOVE 50.00 TO PBKM-UNPOSTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-UNPOSTED-AMT > 10000.00
               MOVE 10000.00 TO PBKM-UNPOSTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3790-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * PBKM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3810-COUNT-RETRY.
           IF PBKM-RETRY-CNT < 9999999
               ADD 1 TO PBKM-RETRY-CNT
           END-IF
           IF PBKM-RETRY-CNT NOT < 5
               MOVE 'Y' TO PBKM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOOK TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-BOOK-TYPE.
           EVALUATE PBKM-BOOK-TYPE-CD
               WHEN 'CH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'HG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED SCHED TIER FOR THE LINE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3830-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-SCHED-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PBKM-LINE-AMT NOT < WS-PRINTED-SCHED-LOW(WS-TX)
                   MOVE WS-PRINTED-SCHED-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3850-ACCRUE-INTEREST.
           IF PBKM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-INTEREST-YTD * PBKM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE UNPOSTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINTED YTD.
      *----------------------------------------------------------------*
       3860-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-PRINTED-YTD ROUNDED =
                   PBKM-UNPOSTED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-PRINTED-YTD
           END-IF
           ADD PBKM-PRINTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CARRYFWD AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3880-ACCRUE-CARRYFWD.
           IF PBKT-CARRYFWD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-CARRYFWD-AMT * PBKM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-PRINTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3890-DESCRIBE-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT UNPOSTED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-UNPOSTED.
           COMPUTE WS-WORK-AMT = PBKM-UNPOSTED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = PBKM-UNPOSTED-YTD - (WS-WORK-AMT * 3)
           COMPUTE PBKT-PRINTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LINE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3910-FLAG-LINE.
           IF PBKT-LINE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKT-LINE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3920-FLAG-INTEREST.
           IF PBKM-INTEREST-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-INTEREST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINTED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3930-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKT-PRINTED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = PBKT-PRINTED-AMT - (WS-WORK-AMT * 12)
           COMPUTE PBKT-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LINE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINTED YTD.
      *----------------------------------------------------------------*
       3940-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-PRINTED-YTD ROUNDED =
                   PBKM-LINE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-PRINTED-YTD
           END-IF
           ADD PBKM-PRINTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3950-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-SOURCE-CD
                   ADD PBKM-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3960-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * UNPOSTED AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3970-LIMIT-UNPOSTED.
           IF PBKM-UNPOSTED-AMT < 25.00
               MOVE 25.00 TO PBKM-UNPOSTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-UNPOSTED-AMT > 5000.00
               MOVE 5000.00 TO PBKM-UNPOSTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED SCHED TIER FOR THE PRINTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3980-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-SCHED-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PBKM-PRINTED-YTD NOT < WS-PRINTED-SCHED-LOW(WS-TX)
                   MOVE WS-PRINTED-SCHED-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3990-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO PBKM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4000-FLAG-INTEREST.
           IF PBKT-INTEREST-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKT-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E231 MEMO REQUIRED
      * E925 LINE YTD NEGATIVE
      * E699 LINE YTD OVER 99999
      * E071 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-MEMO = SPACES
                   MOVE 'E231' TO WS-REJECT-CODE
               WHEN PBKM-LINE-YTD < 0
                   MOVE 'E925' TO WS-REJECT-CODE
               WHEN PBKM-LINE-YTD > 99999.00
                   MOVE 'E699' TO WS-REJECT-CODE
               WHEN PBKT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E071' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE AMT BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4020-ACCUM-PRINTER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-PRINTER-CD
                   ADD PBKM-BALANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-PRINTER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-BALANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE UNPOSTED YTD BY PRINTER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-PRINTER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-PRINTER-CD
                   ADD PBKM-UNPOSTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-PRINTER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-UNPOSTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4040-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKT-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E197 REJECT CD REQUIRED
      * E878 PRINTED AMT NEGATIVE
      * E528 PRINTED AMT OVER 25000
      * E932 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-REJECT-CD = SPACES
                   MOVE 'E197' TO WS-REJECT-CODE
               WHEN PBKT-PRINTED-AMT < 0
                   MOVE 'E878' TO WS-REJECT-CODE
               WHEN PBKT-PRINTED-AMT > 25000.00
                   MOVE 'E528' TO WS-REJECT-CODE
               WHEN PBKM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E932' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LINE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BALANCE AMT.
      *----------------------------------------------------------------*
       4060-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-BALANCE-AMT ROUNDED =
                   PBKT-LINE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-BALANCE-AMT
           END-IF
           ADD PBKM-BALANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-SCHED-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-PRINTED-SCHED-LOW(WS-TX)
                   MOVE WS-PRINTED-SCHED-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4080-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-CARRYFWD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED SCHED TIER FOR THE PRINTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4090-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-SCHED-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PBKM-PRINTED-YTD NOT < WS-PRINTED-SCHED-LOW(WS-TX)
                   MOVE WS-PRINTED-SCHED-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4100-FLAG-INTEREST.
           IF PBKT-INTEREST-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKT-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED SCHED TIER FOR THE LINE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4110-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-SCHED-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PBKT-LINE-AMT NOT < WS-PRINTED-SCHED-LOW(WS-TX)
                   MOVE WS-PRINTED-SCHED-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PRINTER CD:
      * CB -> EP WHEN LAST MAINT DATE IS PAST
      * EP -> EX WHEN LAST MAINT DATE IS PAST
      * EX -> JE WHEN LAST MAINT DATE IS PAST
      * JE -> MR WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4120-SET-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EP' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EP'
                   IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'EX' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EX'
                   IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JE' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MR' TO PBKM-PRINTER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4130-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO PBKM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * JX -> ZC WHEN LAST ACT DATE IS PAST
      * ZC -> ZY WHEN LAST ACT DATE IS PAST
      * ZY -> JX WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4140-SET-SOURCE.
           EVALUATE PBKM-SOURCE-CD
               WHEN 'JX'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZC' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZC'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZY' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZY'
                   IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JX' TO PBKM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E607 MEMO REQUIRED
      * E192 WORK AMT 2 NEGATIVE
      * E367 WORK AMT 2 OVER 250000
      * E049 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4150-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-MEMO = SPACES
                   MOVE 'E607' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E192' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E367' TO WS-REJECT-CODE
               WHEN PBKM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E049' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED SCHED TIER FOR THE CARRYFWD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4160-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-SCHED-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PBKM-CARRYFWD-YTD NOT < WS-PRINTED-SCHED-LOW(WS-TX)
                   MOVE WS-PRINTED-SCHED-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-SEQ.
           IF PBKT-SEQ < 9999999
               ADD 1 TO PBKT-SEQ
           END-IF
           IF PBKT-SEQ NOT < 12
               MOVE 'Y' TO PBKT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E280 CUST ID REQUIRED
      * E192 WORK AMT NEGATIVE
      * E909 WORK AMT OVER 1000000
      * E269 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4180-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CUST-ID = SPACES
                   MOVE 'E280' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E192' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E909' TO WS-REJECT-CODE
               WHEN PBKM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E269' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE LINE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4190-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PBKT-LINE-AMT NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO
                       PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4200-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO PBKM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4210-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED SCHED TIER FOR THE LINE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4220-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-SCHED-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PBKM-LINE-YTD NOT < WS-PRINTED-SCHED-LOW(WS-TX)
                   MOVE WS-PRINTED-SCHED-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4230-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-CLOSE-DATE > 0 AND PBKM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-LINE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKM-LINE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-LINE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-LINE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-LINE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4240-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PBKM-OPEN-DATE > 0 AND PBKM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE LINE FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4250-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PBKM-LINE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   COMPUTE PBKM-LINE-AMT ROUNDED = WS-WORK-AMT *
                       PBKM-MARGIN / 200
               WHEN PBKM-SOURCE-CD = 'JX'
                   MOVE 12.00 TO PBKM-LINE-AMT
               WHEN PBKM-SOURCE-CD = 'ZC'
                   MOVE 20.00 TO PBKM-LINE-AMT
               WHEN OTHER
                   COMPUTE PBKM-LINE-AMT ROUNDED = WS-WORK-AMT *
                       PBKM-MARGIN / 100
           END-EVALUATE
           IF PBKM-LINE-AMT > 0 AND PBKM-LINE-AMT < 1.00
               MOVE 1.00 TO PBKM-LINE-AMT
           END-IF
           IF PBKM-LINE-AMT > 250.00
               MOVE 250.00 TO PBKM-LINE-AMT
           END-IF
           ADD PBKM-LINE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE FEE ON THE WORK AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4260-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-BALANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   COMPUTE PBKM-BALANCE-AMT ROUNDED = WS-WORK-AMT *
                       PBKM-MARGIN / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 15.00 TO PBKM-BALANCE-AMT
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 3.00 TO PBKM-BALANCE-AMT
               WHEN OTHER
                   COMPUTE PBKM-BALANCE-AMT ROUNDED = WS-WORK-AMT *
                       PBKM-MARGIN / 100
           END-EVALUATE
           IF PBKM-BALANCE-AMT > 0 AND PBKM-BALANCE-AMT < 10.00
               MOVE 10.00 TO PBKM-BALANCE-AMT
           END-IF
           IF PBKM-BALANCE-AMT > 250.00
               MOVE 250.00 TO PBKM-BALANCE-AMT
           END-IF
           ADD PBKM-BALANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST SCHED TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4270-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-SCHED-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF PBKT-INTEREST-AMT NOT < WS-INTEREST-SCHED-LOW(WS-TX)
                   MOVE WS-INTEREST-SCHED-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E016 DESC REQUIRED
      * E280 PRINTED YTD NEGATIVE
      * E164 PRINTED YTD OVER 50000
      * E604 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4280-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-DESC = SPACES
                   MOVE 'E016' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-YTD < 0
                   MOVE 'E280' TO WS-REJECT-CODE
               WHEN PBKM-PRINTED-YTD > 50000.00
                   MOVE 'E164' TO WS-REJECT-CODE
               WHEN PBKM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4290-FLAG-INTEREST.
           IF PBKT-INTEREST-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKT-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO UNPOSTED AMT.
      *----------------------------------------------------------------*
       4300-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-UNPOSTED-AMT ROUNDED =
                   PBKM-PRINTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-UNPOSTED-AMT
           END-IF
           ADD PBKM-UNPOSTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4310-COUNT-SEQ.
           IF PBKT-SEQ < 9999999
               ADD 1 TO PBKT-SEQ
           END-IF
           IF PBKT-SEQ NOT < 10
               MOVE 'Y' TO PBKM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CARRYFWD AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4320-SPLIT-CARRYFWD.
           COMPUTE WS-WORK-AMT = PBKT-CARRYFWD-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBKT-CARRYFWD-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-CARRYFWD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4330-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF PBKM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO PBKM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4340-LIMIT-BALANCE.
           IF PBKM-BALANCE-AMT < 100.00
               MOVE 100.00 TO PBKM-BALANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-BALANCE-AMT > 5000.00
               MOVE 5000.00 TO PBKM-BALANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CARRYFWD AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4350-ACCRUE-CARRYFWD.
           IF PBKM-CARRYFWD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-CARRYFWD-AMT * PBKM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINTED AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4360-ACCRUE-PRINTED.
           IF PBKM-PRINTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-PRINTED-AMT * PBKM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-UNPOSTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * UP -> PR WHEN EFF DATE IS PAST
      * PR -> RB WHEN EFF DATE IS PAST
      * RB -> LS WHEN EFF DATE IS PAST
      * LS -> CF WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4370-SET-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CF' TO PBKT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E820 NAME REQUIRED
      * E602 UNPOSTED AMT NEGATIVE
      * E365 UNPOSTED AMT OVER 99999
      * E778 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4380-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-NAME = SPACES
                   MOVE 'E820' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-AMT < 0
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-AMT > 99999.00
                   MOVE 'E365' TO WS-REJECT-CODE
               WHEN PBKM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E778' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINTED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4390-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKM-PRINTED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-PRINTED-YTD - (WS-WORK-AMT * 2)
           COMPUTE PBKM-BALANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINTED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4400-SPLIT-PRINTED.
           COMPUTE WS-WORK-AMT = PBKT-PRINTED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = PBKT-PRINTED-AMT - (WS-WORK-AMT * 3)
           COMPUTE PBKM-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4410-FLAG-INTEREST.
           IF PBKT-INTEREST-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKT-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4420-COUNT-RETRY.
           IF PBKM-RETRY-CNT < 9999999
               ADD 1 TO PBKM-RETRY-CNT
           END-IF
           IF PBKM-RETRY-CNT NOT < 3
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E807 REJECT CD REQUIRED
      * E066 WORK AMT NEGATIVE
      * E605 WORK AMT OVER 250000
      * E314 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4430-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-REJECT-CD = SPACES
                   MOVE 'E807' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E066' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN PBKM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E314' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINTED AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4440-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BRANCH-CD
                   ADD PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKT-PRINTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4450-FLAG-PRINTED.
           IF PBKM-PRINTED-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-PRINTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE UNPOSTED YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4460-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PBKM-UNPOSTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   COMPUTE PBKM-UNPOSTED-YTD ROUNDED = WS-WORK-AMT *
                       PBKM-PENALTY-PCT / 200
               WHEN PBKM-PRINTER-CD = 'CB'
                   MOVE 3.00 TO PBKM-UNPOSTED-YTD
               WHEN PBKM-PRINTER-CD = 'EP'
                   MOVE 12.00 TO PBKM-UNPOSTED-YTD
               WHEN OTHER
                   COMPUTE PBKM-UNPOSTED-YTD ROUNDED = WS-WORK-AMT *
                       PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-UNPOSTED-YTD > 0 AND PBKM-UNPOSTED-YTD < 1.00
               MOVE 1.00 TO PBKM-UNPOSTED-YTD
           END-IF
           IF PBKM-UNPOSTED-YTD > 100.00
               MOVE 100.00 TO PBKM-UNPOSTED-YTD
           END-IF
           ADD PBKM-UNPOSTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CARRYFWD AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4470-ACCRUE-CARRYFWD.
           IF PBKT-CARRYFWD-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-CARRYFWD-AMT * PBKM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LINE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LINE YTD.
      *----------------------------------------------------------------*
       4480-PRORATE-LINE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-LINE-YTD ROUNDED =
                   PBKM-LINE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-LINE-YTD
           END-IF
           ADD PBKM-LINE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED SCHED TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4490-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-SCHED-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF PBKT-INTEREST-AMT NOT < WS-PRINTED-SCHED-LOW(WS-TX)
                   MOVE WS-PRINTED-SCHED-RATE(WS-TX) TO PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LINE YTD.
      *----------------------------------------------------------------*
       4500-PRORATE-LINE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-LINE-YTD ROUNDED =
                   PBKM-INTEREST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-LINE-YTD
           END-IF
           ADD PBKM-LINE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE LINE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO UNPOSTED AMT.
      *----------------------------------------------------------------*
       4510-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-UNPOSTED-AMT ROUNDED =
                   PBKT-LINE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-UNPOSTED-AMT
           END-IF
           ADD PBKM-UNPOSTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PASS THE PBK FIELDS TO PBSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-PBSTAT.
           INITIALIZE PBSTAT-PARMS
           MOVE PBKM-UNPOSTED-YTD
             TO PBSTAT-UNPOSTED-YTD
           MOVE PBKM-BALANCE-AMT
             TO PBSTAT-BALANCE-AMT
           MOVE PBKM-LINE-YTD
             TO PBSTAT-LINE-YTD
           MOVE PBKM-CARRYFWD-YTD
             TO PBSTAT-CARRYFWD-YTD
           MOVE PBKM-UNPOSTED-AMT
             TO PBSTAT-UNPOSTED-AMT
           MOVE PBKM-LINE-AMT
             TO PBSTAT-LINE-AMT
           MOVE PBKM-CLOSE-DATE
             TO PBSTAT-CLOSE-DATE
           MOVE PBKM-EFFECTIVE-DATE
             TO PBSTAT-EFFECTIVE-DATE
           MOVE PBKM-NEXT-DUE-DATE
             TO PBSTAT-NEXT-DUE-DATE
           MOVE PBKM-NEXT-REVIEW-DATE
             TO PBSTAT-NEXT-REVIEW-DATE
           MOVE PBKM-BOOK-TYPE-CD
             TO PBSTAT-BOOK-TYPE-CD
           MOVE PBKM-PRINTER-CD
             TO PBSTAT-PRINTER-CD
           MOVE PBKM-REGION-CD
             TO PBSTAT-REGION-CD
           MOVE PBKM-SOURCE-CD
             TO PBSTAT-SOURCE-CD
           MOVE PBKM-NOTICE-SENT-FLAG
             TO PBSTAT-NOTICE-SENT-FLAG
           MOVE PBKM-REVIEW-FLAG
             TO PBSTAT-REVIEW-FLAG
           MOVE PBKM-AUTO-RENEW-FLAG
             TO PBSTAT-AUTO-RENEW-FLAG
           MOVE PBKM-RETRY-CNT
             TO PBSTAT-RETRY-CNT
           MOVE PBKM-CYCLE-CNT
             TO PBSTAT-CYCLE-CNT
           MOVE PBKM-PENALTY-PCT
             TO PBSTAT-PENALTY-PCT
           MOVE PBKM-MARGIN
             TO PBSTAT-MARGIN
           MOVE PBKM-CONTACT
             TO PBSTAT-CONTACT
           MOVE PBKM-CUST-ID
             TO PBSTAT-CUST-ID
           CALL 'PBSTAT' USING PBSTAT-PARMS
           EVALUATE PBSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PBSTAT-UNPOSTED-YTD
                     TO PBKM-UNPOSTED-YTD
                   MOVE PBSTAT-UNPOSTED-AMT
                     TO PBKM-UNPOSTED-AMT
                   MOVE PBSTAT-NOTICE-SENT-FLAG
                     TO PBKM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-PBSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE PBK FIELDS TO PBPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-PBPRC.
           INITIALIZE PBPRC-PARMS
           MOVE PBKM-PRINTED-AMT
             TO PBPRC-PRINTED-AMT
           MOVE PBKM-LINE-YTD
             TO PBPRC-LINE-YTD
           MOVE PBKM-UNPOSTED-AMT
             TO PBPRC-UNPOSTED-AMT
           MOVE PBKM-CARRYFWD-YTD
             TO PBPRC-CARRYFWD-YTD
           MOVE PBKM-INTEREST-AMT
             TO PBPRC-INTEREST-AMT
           MOVE PBKM-INTEREST-YTD
             TO PBPRC-INTEREST-YTD
           MOVE PBKM-MATURITY-DATE
             TO PBPRC-MATURITY-DATE
           MOVE PBKM-EXPIRY-DATE
             TO PBPRC-EXPIRY-DATE
           MOVE PBKM-EFFECTIVE-DATE
             TO PBPRC-EFFECTIVE-DATE
           MOVE PBKM-NEXT-REVIEW-DATE
             TO PBPRC-NEXT-REVIEW-DATE
           MOVE PBKM-CHANNEL-CD
             TO PBPRC-CHANNEL-CD
           MOVE PBKM-REGION-CD
             TO PBPRC-REGION-CD
           MOVE PBKM-SOURCE-CD
             TO PBPRC-SOURCE-CD
           MOVE PBKM-PRINTER-CD
             TO PBPRC-PRINTER-CD
           MOVE PBKM-AUTO-RENEW-FLAG
             TO PBPRC-AUTO-RENEW-FLAG
           MOVE PBKM-NOTICE-SENT-FLAG
             TO PBPRC-NOTICE-SENT-FLAG
           MOVE PBKM-HOLD-FLAG
             TO PBPRC-HOLD-FLAG
           MOVE PBKM-PAYMENT-CNT
             TO PBPRC-PAYMENT-CNT
           MOVE PBKM-ITEM-CNT
             TO PBPRC-ITEM-CNT
           MOVE PBKM-PENALTY-PCT
             TO PBPRC-PENALTY-PCT
           MOVE PBKM-MARGIN
             TO PBPRC-MARGIN
           MOVE PBKM-CUST-ID
             TO PBPRC-CUST-ID
           MOVE PBKM-ADDR
             TO PBPRC-ADDR
           CALL 'PBPRC' USING PBPRC-PARMS
           EVALUATE PBPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PBPRC-LINE-YTD
                     TO PBKM-LINE-YTD
                   MOVE PBPRC-UNPOSTED-AMT
                     TO PBKM-UNPOSTED-AMT
                   MOVE PBPRC-CARRYFWD-YTD
                     TO PBKM-CARRYFWD-YTD
                   MOVE PBPRC-INTEREST-AMT
                     TO PBKM-INTEREST-AMT
                   MOVE PBPRC-INTEREST-YTD
                     TO PBKM-INTEREST-YTD
                   MOVE PBPRC-NOTICE-SENT-FLAG
                     TO PBKM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-PBPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE PBK FIELDS TO PBDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-PBDTE.
           INITIALIZE PBDTE-PARMS
           MOVE PBKM-PRINTED-YTD
             TO PBDTE-PRINTED-YTD
           MOVE PBKM-INTEREST-YTD
             TO PBDTE-INTEREST-YTD
           MOVE PBKM-UNPOSTED-YTD
             TO PBDTE-UNPOSTED-YTD
           MOVE PBKM-UNPOSTED-AMT
             TO PBDTE-UNPOSTED-AMT
           MOVE PBKM-CARRYFWD-AMT
             TO PBDTE-CARRYFWD-AMT
           MOVE PBKM-CARRYFWD-YTD
             TO PBDTE-CARRYFWD-YTD
           MOVE PBKM-NEXT-DUE-DATE
             TO PBDTE-NEXT-DUE-DATE
           MOVE PBKM-MATURITY-DATE
             TO PBDTE-MATURITY-DATE
           MOVE PBKM-OPEN-DATE
             TO PBDTE-OPEN-DATE
           MOVE PBKM-CLOSE-DATE
             TO PBDTE-CLOSE-DATE
           MOVE PBKM-REGION-CD
             TO PBDTE-REGION-CD
           MOVE PBKM-SOURCE-CD
             TO PBDTE-SOURCE-CD
           MOVE PBKM-BRANCH-CD
             TO PBDTE-BRANCH-CD
           MOVE PBKM-PRINTER-CD
             TO PBDTE-PRINTER-CD
           MOVE PBKM-REVIEW-FLAG
             TO PBDTE-REVIEW-FLAG
           MOVE PBKM-EXEMPT-FLAG
             TO PBDTE-EXEMPT-FLAG
           MOVE PBKM-SUSPEND-FLAG
             TO PBDTE-SUSPEND-FLAG
           MOVE PBKM-CYCLE-CNT
             TO PBDTE-CYCLE-CNT
           MOVE PBKM-RETRY-CNT
             TO PBDTE-RETRY-CNT
           MOVE PBKM-MARGIN
             TO PBDTE-MARGIN
           MOVE PBKM-FEE-PCT
             TO PBDTE-FEE-PCT
           MOVE PBKM-CUST-ID
             TO PBDTE-CUST-ID
           MOVE PBKM-NAME
             TO PBDTE-NAME
           CALL 'PBDTE' USING PBDTE-PARMS
           EVALUATE PBDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE PBDTE-PRINTED-YTD
                     TO PBKM-PRINTED-YTD
                   MOVE PBDTE-UNPOSTED-YTD
                     TO PBKM-UNPOSTED-YTD
                   MOVE PBDTE-UNPOSTED-AMT
                     TO PBKM-UNPOSTED-AMT
                   MOVE PBDTE-EXEMPT-FLAG
                     TO PBKM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-PBDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE PBKT-KEY TO RD-KEY
           MOVE PBKT-TRAN-CD TO RD-CODE
           MOVE PBKT-PRINTED-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE PBKT-KEY TO RD-KEY
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
           DISPLAY 'PBKRPT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PBKRPT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
