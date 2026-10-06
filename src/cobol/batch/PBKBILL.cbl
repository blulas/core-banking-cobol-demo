      *================================================================*
      * PROGRAM  : PBKBILL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PASSBOOK PRINTING - CYCLE BILLING. READS THE PBK    *
      *            TRANSACTIONS, UPDATES THE PBK MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. PASSBOOK SAVINGS UPDATE PRINTING*
      *            AND UNPOSTED ITEM CONTROL                           *
      * INPUT    : PBKMST (KSDS), PBKTRN, BANKCTL                      *
      * OUTPUT   : PBKOUT, PBKBIRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, PBDTE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-08-07 SKN  ORIGINAL                                       *
      * 2003-02-05 EWS  UNPOSTED FEE SCHEDULE UPDATE REQ3132           *
      * 2004-06-23 BHS  AUDIT FINDING - LINE CONTROL TOTALS INC8252    *
      * 2007-05-20 GAF  NEW CARRYFWD REPORT COLUMNS CHG1918            *
      * 2008-10-24 CRP  UNPOSTED ROUNDING - HALF UP REQ1198            *
      * 2008-09-08 MKL  LINE LIMIT REVISED PRB2719                     *
      * 2011-07-24 BHS  CARRYFWD RERUN PROTECTION CHG9114              *
      * 2012-07-05 SKN  BALANCE EXCEPTIONS TO OPS QUEUE CHG0372        *
      * 2013-09-25 SKN  CARRYFWD EXCEPTIONS TO OPS QUEUE PRB6138       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBKBILL.
       AUTHOR. M K LOWE.

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
           SELECT REPORT-FILE ASSIGN TO "PBKBIRPT"
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
                                           VALUE 20.000000.
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
      * WS-UNPOSTED-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-UNPOSTED-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000864000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000846500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000167000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000800000000017000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000900000000318000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010900000000034000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010950000000083000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020950000000111500'.
       01  WS-UNPOSTED-BAND-TBL REDEFINES WS-UNPOSTED-BAND-VALUES.
           05  WS-UNPOSTED-BAND-ROW OCCURS 8 TIMES.
               10  WS-UNPOSTED-BAND-LOW    PIC 9(09)V99.
               10  WS-UNPOSTED-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INTEREST-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000615000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000210500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000216000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000860500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001300000000842000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011300000000488000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021300000000662500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021550000000745500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021650000000461500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021750000000169000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026750000000327000'.
       01  WS-INTEREST-BAND-TBL REDEFINES WS-INTEREST-BAND-VALUES.
           05  WS-INTEREST-BAND-ROW OCCURS 11 TIMES.
               10  WS-INTEREST-BAND-LOW    PIC 9(09)V99.
               10  WS-INTEREST-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PRINTED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINTED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000593500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000174500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000579500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022500000000694000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027500000000511500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028000000000765000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028500000000524500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033500000000377500'.
       01  WS-PRINTED-GRID-TBL REDEFINES WS-PRINTED-GRID-VALUES.
           05  WS-PRINTED-GRID-ROW OCCURS 8 TIMES.
               10  WS-PRINTED-GRID-LOW     PIC 9(09)V99.
               10  WS-PRINTED-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-LINE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LINE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000683000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000763000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000845000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000893500'.
       01  WS-LINE-SCHED-TBL REDEFINES WS-LINE-SCHED-VALUES.
           05  WS-LINE-SCHED-ROW OCCURS 4 TIMES.
               10  WS-LINE-SCHED-LOW       PIC 9(09)V99.
               10  WS-LINE-SCHED-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  PBKBILL  PASSBOOK PRINTING'.
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
               PERFORM 3000-ACCRUE-BALANCE
               PERFORM 3010-SET-REGION
               PERFORM 3020-EDIT-NAME
               PERFORM 3030-ASSESS-AUTO-RENEW
               PERFORM 3040-AGE-EFFECTIVE
               PERFORM 3050-EDIT-REJECT
               PERFORM 3060-COUNT-RETRY
               PERFORM 3070-SPLIT-CARRYFWD
               PERFORM 3080-LIMIT-ITEM
               PERFORM 3090-ACCRUE-INTEREST
               PERFORM 3100-ASSESS-REVERSAL
               PERFORM 3110-SET-BRANCH
               PERFORM 3120-ACCRUE-UNPOSTED
               PERFORM 3130-FLAG-ITEM
               PERFORM 3140-DESCRIBE-TRAN
               PERFORM 3150-FLAG-LINE
               PERFORM 3160-DESCRIBE-BOOK-TYPE
               PERFORM 3170-SPLIT-BALANCE
               PERFORM 3180-EDIT-CUST-ID
               PERFORM 3190-CHECK-CLOSE
               PERFORM 3200-DESCRIBE-REGION
               PERFORM 3210-SPLIT-CARRYFWD
               PERFORM 3220-PRORATE-BALANCE
               PERFORM 3230-PRORATE-LINE
               PERFORM 3240-EDIT-NAME
               PERFORM 3250-LIMIT-PRINTED
               PERFORM 3260-SPLIT-INTEREST
               PERFORM 3270-ACCRUE-LINE
               PERFORM 3280-PRORATE-CARRYFWD
               PERFORM 3290-SET-CHANNEL
               PERFORM 3300-AGE-LAST-ACT
               PERFORM 3310-LIMIT-CARRYFWD
               PERFORM 3320-ASSESS-AUTO-RENEW
               PERFORM 3330-AGE-EXPIRY
               PERFORM 3340-ASSESS-REVIEW
               PERFORM 3350-LIMIT-UNPOSTED
               PERFORM 3360-FLAG-UNPOSTED
               PERFORM 3370-ASSESS-AUTO-RENEW
               PERFORM 3380-SPLIT-UNPOSTED
               PERFORM 3390-CHECK-LAST-ACT
               PERFORM 3400-CHECK-NEXT-REVIEW
               PERFORM 3410-LIMIT-LINE
               PERFORM 3420-ACCUM-SOURCE
               PERFORM 3430-LIMIT-ITEM
               PERFORM 3440-SELECT-MARGIN
               PERFORM 3450-SELECT-MARGIN
               PERFORM 3460-AGE-LAST-MAINT
               PERFORM 3470-SPLIT-BALANCE
               PERFORM 3480-SELECT-MARGIN
               PERFORM 3490-CHECK-LAST-MAINT
               PERFORM 3500-DESCRIBE-REGION
               PERFORM 3510-CHECK-LAST-ACT
               PERFORM 3520-AGE-MATURITY
               PERFORM 3530-SET-CHANNEL
               PERFORM 3540-ASSESS-REVERSAL
               PERFORM 3550-DESCRIBE-PRINTER
               PERFORM 3560-PRORATE-INTEREST
               PERFORM 3570-ACCUM-SOURCE
               PERFORM 3580-LIMIT-CARRYFWD
               PERFORM 3590-AGE-CLOSE
               PERFORM 3600-ACCUM-BRANCH
               PERFORM 3610-SPLIT-INTEREST
               PERFORM 3620-ASSESS-EXEMPT
               PERFORM 3630-FLAG-BALANCE
               PERFORM 3640-SET-BOOK-TYPE
               PERFORM 3650-COUNT-SEQ
               PERFORM 3660-EDIT-MEMO
               PERFORM 3670-ACCRUE-CARRYFWD
               PERFORM 3680-FLAG-ITEM
               PERFORM 3690-ACCUM-CHANNEL
               PERFORM 3700-SELECT-MARGIN
               PERFORM 3710-ASSESS-NOTICE-SENT
               PERFORM 3720-ASSESS-AUTO-RENEW
               PERFORM 3730-COUNT-PAYMENT
               PERFORM 3740-LIMIT-ITEM
               PERFORM 3750-SET-CHANNEL
               PERFORM 3760-PRORATE-UNPOSTED
               PERFORM 3770-AGE-OPEN
               PERFORM 3780-PRORATE-LINE
               PERFORM 3790-ACCUM-BRANCH
               PERFORM 3800-ACCRUE-MARGIN
               PERFORM 3810-FLAG-CARRYFWD
               PERFORM 3820-SPLIT-BALANCE
               PERFORM 3830-PRORATE-CARRYFWD
               PERFORM 3840-EDIT-NAME
               PERFORM 3850-PRORATE-BALANCE
               PERFORM 3860-ASSESS-SUSPEND
               PERFORM 3870-SPLIT-BALANCE
               PERFORM 3880-SPLIT-INTEREST
               PERFORM 3890-DESCRIBE-TRAN
               PERFORM 3900-ACCRUE-CARRYFWD
               PERFORM 3910-SPLIT-UNPOSTED
               PERFORM 3920-COUNT-PAYMENT
               PERFORM 3930-CHECK-LAST-MAINT
               PERFORM 3940-FLAG-BALANCE
               PERFORM 3950-SELECT-MARGIN
               PERFORM 3960-FLAG-INTEREST
               PERFORM 3970-PRORATE-UNPOSTED
               PERFORM 3980-SELECT-PENALTY-PCT
               PERFORM 3990-COUNT-RETRY
               PERFORM 4000-ASSESS-SUSPEND
               PERFORM 4010-EDIT-MEMO
               PERFORM 4020-DESCRIBE-REGION
               PERFORM 4030-CHECK-OPEN
               PERFORM 4040-SELECT-FEE-PCT
               PERFORM 4050-FLAG-UNPOSTED
               PERFORM 4060-PRORATE-PRINTED
               PERFORM 4070-SELECT-MARGIN
               PERFORM 4080-ACCUM-SOURCE
               PERFORM 4090-PRORATE-CARRYFWD
               PERFORM 4100-ACCUM-CHANNEL
               PERFORM 4110-ASSESS-HOLD
               PERFORM 4120-COUNT-ITEM
               PERFORM 4130-DESCRIBE-PRINTER
               PERFORM 4140-SET-CHANNEL
               PERFORM 4150-ACCUM-BOOK-TYPE
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
      * DAILY INTEREST ON BALANCE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3000-ACCRUE-BALANCE.
           IF PBKM-BALANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-BALANCE-YTD * PBKM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO PBKM-CARRYFWD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * ER -> GL WHEN NEXT REVIEW DATE IS PAST
      * GL -> PE WHEN NEXT REVIEW DATE IS PAST
      * PE -> RJ WHEN NEXT REVIEW DATE IS PAST
      * RJ -> VB WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GL' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PE' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PE'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RJ' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VB' TO PBKM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E787 NAME REQUIRED
      * E796 WORK AMT 2 NEGATIVE
      * E579 WORK AMT 2 OVER 1000000
      * E465 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-NAME = SPACES
                   MOVE 'E787' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E796' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E579' TO WS-REJECT-CODE
               WHEN PBKM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E465' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE UNPOSTED FEE ON THE LINE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3030-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-UNPOSTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   COMPUTE PBKM-UNPOSTED-AMT ROUNDED = PBKM-LINE-YTD *
                       PBKM-FEE-PCT / 200
               WHEN PBKM-REGION-CD = 'ER'
                   MOVE 3.00 TO PBKM-UNPOSTED-AMT
               WHEN PBKM-REGION-CD = 'GL'
                   MOVE 10.00 TO PBKM-UNPOSTED-AMT
               WHEN OTHER
                   COMPUTE PBKM-UNPOSTED-AMT ROUNDED = PBKM-LINE-YTD *
                       PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-UNPOSTED-AMT > 0 AND PBKM-UNPOSTED-AMT < 10.00
               MOVE 10.00 TO PBKM-UNPOSTED-AMT
           END-IF
           IF PBKM-UNPOSTED-AMT > 25.00
               MOVE 25.00 TO PBKM-UNPOSTED-AMT
           END-IF
           ADD PBKM-UNPOSTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3040-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E055 REJECT CD REQUIRED
      * E865 LINE AMT NEGATIVE
      * E421 LINE AMT OVER 25000
      * E495 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3050-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-REJECT-CD = SPACES
                   MOVE 'E055' TO WS-REJECT-CODE
               WHEN PBKT-LINE-AMT < 0
                   MOVE 'E865' TO WS-REJECT-CODE
               WHEN PBKT-LINE-AMT > 25000.00
                   MOVE 'E421' TO WS-REJECT-CODE
               WHEN PBKT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E495' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3060-COUNT-RETRY.
           IF PBKM-RETRY-CNT < 9999999
               ADD 1 TO PBKM-RETRY-CNT
           END-IF
           IF PBKM-RETRY-CNT NOT < 12
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CARRYFWD AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3070-SPLIT-CARRYFWD.
           COMPUTE WS-WORK-AMT = PBKM-CARRYFWD-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-CARRYFWD-AMT - (WS-WORK-AMT * 2)
           COMPUTE PBKT-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3080-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3090-ACCRUE-INTEREST.
           IF PBKM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-INTEREST-YTD * PBKM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBKM-LINE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE UNPOSTED YTD FEE ON THE INTEREST AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3100-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PBKM-UNPOSTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-HOLD-FLAG = 'Y'
                   COMPUTE PBKM-UNPOSTED-YTD ROUNDED = PBKT-INTEREST-AMT
                       * PBKM-MARGIN / 200
               WHEN PBKT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO PBKM-UNPOSTED-YTD
               WHEN PBKT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO PBKM-UNPOSTED-YTD
               WHEN OTHER
                   COMPUTE PBKM-UNPOSTED-YTD ROUNDED = PBKT-INTEREST-AMT
                       * PBKM-MARGIN / 100
           END-EVALUATE
           IF PBKM-UNPOSTED-YTD > 0 AND PBKM-UNPOSTED-YTD < 1.00
               MOVE 1.00 TO PBKM-UNPOSTED-YTD
           END-IF
           IF PBKM-UNPOSTED-YTD > 35.00
               MOVE 35.00 TO PBKM-UNPOSTED-YTD
           END-IF
           ADD PBKM-UNPOSTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * DF -> NV WHEN EFF DATE IS PAST
      * NV -> PP WHEN EFF DATE IS PAST
      * PP -> SH WHEN EFF DATE IS PAST
      * SH -> ZP WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-BRANCH.
           EVALUATE PBKM-BRANCH-CD
               WHEN 'DF'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NV' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NV'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PP'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZP' TO PBKM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON UNPOSTED AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3120-ACCRUE-UNPOSTED.
           IF PBKM-UNPOSTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-UNPOSTED-AMT * PBKM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO PBKM-CARRYFWD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3130-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LINE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3150-FLAG-LINE.
           IF PBKM-LINE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-LINE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BOOK TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3160-DESCRIBE-BOOK-TYPE.
           EVALUATE PBKM-BOOK-TYPE-CD
               WHEN 'CH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BALANCE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = PBKM-BALANCE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-BALANCE-AMT - (WS-WORK-AMT * 2)
           COMPUTE PBKT-PRINTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E371 CUST ID REQUIRED
      * E701 INTEREST YTD NEGATIVE
      * E595 INTEREST YTD OVER 25000
      * E843 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3180-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-CUST-ID = SPACES
                   MOVE 'E371' TO WS-REJECT-CODE
               WHEN PBKM-INTEREST-YTD < 0
                   MOVE 'E701' TO WS-REJECT-CODE
               WHEN PBKM-INTEREST-YTD > 25000.00
                   MOVE 'E595' TO WS-REJECT-CODE
               WHEN PBKM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E843' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF PBKM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-CLOSE-DATE TO DTU-DATE-1
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
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3200-DESCRIBE-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CARRYFWD AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-CARRYFWD.
           COMPUTE WS-WORK-AMT = PBKM-CARRYFWD-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-CARRYFWD-AMT - (WS-WORK-AMT * 2)
           COMPUTE PBKM-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BALANCE AMT.
      *----------------------------------------------------------------*
       3220-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-BALANCE-AMT ROUNDED =
                   PBKT-CARRYFWD-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-BALANCE-AMT
           END-IF
           ADD PBKM-BALANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LINE AMT.
      *----------------------------------------------------------------*
       3230-PRORATE-LINE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKT-LINE-AMT ROUNDED =
                   PBKM-CARRYFWD-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKT-LINE-AMT
           END-IF
           ADD PBKT-LINE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E482 NAME REQUIRED
      * E877 CARRYFWD AMT NEGATIVE
      * E867 CARRYFWD AMT OVER 1000000
      * E755 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-NAME = SPACES
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN PBKT-CARRYFWD-AMT < 0
                   MOVE 'E877' TO WS-REJECT-CODE
               WHEN PBKT-CARRYFWD-AMT > 1000000.00
                   MOVE 'E867' TO WS-REJECT-CODE
               WHEN PBKM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E755' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRINTED AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-PRINTED.
           IF PBKT-PRINTED-AMT < 0.00
               MOVE 0.00 TO PBKT-PRINTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKT-PRINTED-AMT > 5000.00
               MOVE 5000.00 TO PBKT-PRINTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3260-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBKT-INTEREST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBKT-INTEREST-AMT - (WS-WORK-AMT * 4)
           COMPUTE PBKM-CARRYFWD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LINE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3270-ACCRUE-LINE.
           IF PBKT-LINE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-LINE-AMT * PBKM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBKT-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CARRYFWD AMT.
      *----------------------------------------------------------------*
       3280-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-CARRYFWD-AMT ROUNDED =
                   PBKM-PRINTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-CARRYFWD-AMT
           END-IF
           ADD PBKM-CARRYFWD-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> D WHEN EFFECTIVE DATE IS PAST
      * D -> H WHEN EFFECTIVE DATE IS PAST
      * H -> L WHEN EFFECTIVE DATE IS PAST
      * L -> M WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3290-SET-CHANNEL.
           EVALUATE PBKM-CHANNEL-CD
               WHEN 'B'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF PBKM-EFFECTIVE-DATE > 0 AND PBKM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-ACT-DATE > 0 AND PBKM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CARRYFWD AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3310-LIMIT-CARRYFWD.
           IF PBKM-CARRYFWD-AMT < 25.00
               MOVE 25.00 TO PBKM-CARRYFWD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-CARRYFWD-AMT > 5000.00
               MOVE 5000.00 TO PBKM-CARRYFWD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST YTD FEE ON THE CARRYFWD AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3320-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-INTEREST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   COMPUTE PBKM-INTEREST-YTD ROUNDED = PBKT-CARRYFWD-AMT
                       * PBKM-PENALTY-PCT / 200
               WHEN PBKM-REGION-CD = 'ER'
                   MOVE 10.00 TO PBKM-INTEREST-YTD
               WHEN PBKM-REGION-CD = 'GL'
                   MOVE 15.00 TO PBKM-INTEREST-YTD
               WHEN OTHER
                   COMPUTE PBKM-INTEREST-YTD ROUNDED = PBKT-CARRYFWD-AMT
                       * PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-INTEREST-YTD > 0 AND PBKM-INTEREST-YTD < 5.00
               MOVE 5.00 TO PBKM-INTEREST-YTD
           END-IF
           IF PBKM-INTEREST-YTD > 50.00
               MOVE 50.00 TO PBKM-INTEREST-YTD
           END-IF
           ADD PBKM-INTEREST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3330-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF PBKM-EXPIRY-DATE > 0 AND PBKM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE UNPOSTED YTD FEE ON THE UNPOSTED AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3340-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-UNPOSTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-HOLD-FLAG = 'Y'
                   COMPUTE PBKM-UNPOSTED-YTD ROUNDED = PBKM-UNPOSTED-AMT
                       * PBKM-FEE-PCT / 200
               WHEN PBKM-REGION-CD = 'ER'
                   MOVE 3.00 TO PBKM-UNPOSTED-YTD
               WHEN PBKM-REGION-CD = 'GL'
                   MOVE 5.00 TO PBKM-UNPOSTED-YTD
               WHEN OTHER
                   COMPUTE PBKM-UNPOSTED-YTD ROUNDED = PBKM-UNPOSTED-AMT
                       * PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKM-UNPOSTED-YTD > 0 AND PBKM-UNPOSTED-YTD < 1.00
               MOVE 1.00 TO PBKM-UNPOSTED-YTD
           END-IF
           IF PBKM-UNPOSTED-YTD > 75.00
               MOVE 75.00 TO PBKM-UNPOSTED-YTD
           END-IF
           ADD PBKM-UNPOSTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * UNPOSTED YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3350-LIMIT-UNPOSTED.
           IF PBKM-UNPOSTED-YTD < 50.00
               MOVE 50.00 TO PBKM-UNPOSTED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-UNPOSTED-YTD > 10000.00
               MOVE 10000.00 TO PBKM-UNPOSTED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * UNPOSTED YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3360-FLAG-UNPOSTED.
           IF PBKM-UNPOSTED-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKM-UNPOSTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINTED FEE ON THE UNPOSTED AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3370-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-PRINTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-HOLD-FLAG = 'Y'
                   COMPUTE PBKM-PRINTED-AMT ROUNDED = PBKM-UNPOSTED-AMT
                       * PBKM-MARGIN / 200
               WHEN PBKT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO PBKM-PRINTED-AMT
               WHEN PBKT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO PBKM-PRINTED-AMT
               WHEN OTHER
                   COMPUTE PBKM-PRINTED-AMT ROUNDED = PBKM-UNPOSTED-AMT
                       * PBKM-MARGIN / 100
           END-EVALUATE
           IF PBKM-PRINTED-AMT > 0 AND PBKM-PRINTED-AMT < 10.00
               MOVE 10.00 TO PBKM-PRINTED-AMT
           END-IF
           IF PBKM-PRINTED-AMT > 150.00
               MOVE 150.00 TO PBKM-PRINTED-AMT
           END-IF
           ADD PBKM-PRINTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT UNPOSTED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3380-SPLIT-UNPOSTED.
           COMPUTE WS-WORK-AMT = PBKM-UNPOSTED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-UNPOSTED-AMT - (WS-WORK-AMT * 4)
           COMPUTE PBKM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO PBKM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3400-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LINE AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3410-LIMIT-LINE.
           IF PBKM-LINE-AMT < 25.00
               MOVE 25.00 TO PBKM-LINE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-LINE-AMT > 50000.00
               MOVE 50000.00 TO PBKM-LINE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3420-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-SOURCE-CD
                   ADD PBKM-LINE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-LINE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3430-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED GRID TIER FOR THE UNPOSTED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3440-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-GRID-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PBKM-UNPOSTED-YTD NOT < WS-PRINTED-GRID-LOW(WS-TX)
                   MOVE WS-PRINTED-GRID-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST BAND TIER FOR THE LINE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-BAND-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PBKM-LINE-AMT NOT < WS-INTEREST-BAND-LOW(WS-TX)
                   MOVE WS-INTEREST-BAND-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-MAINT-DATE > 0 AND PBKM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BALANCE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3470-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = PBKM-BALANCE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PBKM-BALANCE-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE UNPOSTED BAND TIER FOR THE INTEREST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3480-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-UNPOSTED-BAND-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PBKT-INTEREST-AMT NOT < WS-UNPOSTED-BAND-LOW(WS-TX)
                   MOVE WS-UNPOSTED-BAND-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3490-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3510-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-MATURITY.
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
               WHEN WS-DAYS NOT > 15
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-PRINTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> D WHEN NEXT REVIEW DATE IS PAST
      * D -> H WHEN NEXT REVIEW DATE IS PAST
      * H -> L WHEN NEXT REVIEW DATE IS PAST
      * L -> M WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3530-SET-CHANNEL.
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
      * ASSESS THE CARRYFWD FEE ON THE BALANCE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3540-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO PBKM-CARRYFWD-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-HOLD-FLAG = 'Y'
                   COMPUTE PBKM-CARRYFWD-AMT ROUNDED = PBKM-BALANCE-AMT
                       * PBKM-PENALTY-PCT / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 7.00 TO PBKM-CARRYFWD-AMT
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 3.00 TO PBKM-CARRYFWD-AMT
               WHEN OTHER
                   COMPUTE PBKM-CARRYFWD-AMT ROUNDED = PBKM-BALANCE-AMT
                       * PBKM-PENALTY-PCT / 100
           END-EVALUATE
           IF PBKM-CARRYFWD-AMT > 0 AND PBKM-CARRYFWD-AMT < 10.00
               MOVE 10.00 TO PBKM-CARRYFWD-AMT
           END-IF
           IF PBKM-CARRYFWD-AMT > 150.00
               MOVE 150.00 TO PBKM-CARRYFWD-AMT
           END-IF
           ADD PBKM-CARRYFWD-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3550-DESCRIBE-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'EP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'EX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3560-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKT-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CARRYFWD YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-SOURCE-CD
                   ADD PBKM-CARRYFWD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-CARRYFWD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3580-LIMIT-CARRYFWD.
           IF PBKM-CARRYFWD-AMT < 10.00
               MOVE 10.00 TO PBKM-CARRYFWD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF PBKM-CARRYFWD-AMT > 2500.00
               MOVE 2500.00 TO PBKM-CARRYFWD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-CLOSE.
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
                   ADD PBKM-UNPOSTED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKM-UNPOSTED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PBKM-UNPOSTED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKM-UNPOSTED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKM-UNPOSTED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INTEREST AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3600-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BRANCH-CD
                   ADD PBKM-INTEREST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-INTEREST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3610-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBKM-INTEREST-YTD / 12
           COMPUTE WS-WORK-AMT-2 = PBKM-INTEREST-YTD - (WS-WORK-AMT *
               12)
           COMPUTE PBKT-CARRYFWD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3620-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO PBKM-BALANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   COMPUTE PBKM-BALANCE-YTD ROUNDED = WS-WORK-AMT *
                       PBKM-MARGIN / 200
               WHEN PBKM-SOURCE-CD = 'JX'
                   MOVE 12.00 TO PBKM-BALANCE-YTD
               WHEN PBKM-SOURCE-CD = 'ZC'
                   MOVE 7.00 TO PBKM-BALANCE-YTD
               WHEN OTHER
                   COMPUTE PBKM-BALANCE-YTD ROUNDED = WS-WORK-AMT *
                       PBKM-MARGIN / 100
           END-EVALUATE
           IF PBKM-BALANCE-YTD > 0 AND PBKM-BALANCE-YTD < 2.00
               MOVE 2.00 TO PBKM-BALANCE-YTD
           END-IF
           IF PBKM-BALANCE-YTD > 150.00
               MOVE 150.00 TO PBKM-BALANCE-YTD
           END-IF
           ADD PBKM-BALANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BALANCE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3630-FLAG-BALANCE.
           IF PBKM-BALANCE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-BALANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BOOK TYPE CD:
      * CH -> HG WHEN EFF DATE IS PAST
      * HG -> KB WHEN EFF DATE IS PAST
      * KB -> KW WHEN EFF DATE IS PAST
      * KW -> PA WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3640-SET-BOOK-TYPE.
           EVALUATE PBKM-BOOK-TYPE-CD
               WHEN 'CH'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HG' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HG'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KB' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KB'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KW' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KW'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PA' TO PBKM-BOOK-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-SEQ.
           IF PBKT-SEQ < 9999999
               ADD 1 TO PBKT-SEQ
           END-IF
           IF PBKT-SEQ NOT < 6
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E158 MEMO REQUIRED
      * E897 UNPOSTED YTD NEGATIVE
      * E572 UNPOSTED YTD OVER 10000
      * E065 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-MEMO = SPACES
                   MOVE 'E158' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-YTD < 0
                   MOVE 'E897' TO WS-REJECT-CODE
               WHEN PBKM-UNPOSTED-YTD > 10000.00
                   MOVE 'E572' TO WS-REJECT-CODE
               WHEN PBKM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E065' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CARRYFWD YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3670-ACCRUE-CARRYFWD.
           IF PBKM-CARRYFWD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-CARRYFWD-YTD * PBKM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3680-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE UNPOSTED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3690-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-CHANNEL-CD
                   ADD PBKM-UNPOSTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-UNPOSTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED GRID TIER FOR THE CARRYFWD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3700-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-GRID-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PBKM-CARRYFWD-AMT NOT < WS-PRINTED-GRID-LOW(WS-TX)
                   MOVE WS-PRINTED-GRID-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE UNPOSTED FEE ON THE CARRYFWD AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3710-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN PBKM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO PBKM-UNPOSTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   COMPUTE PBKM-UNPOSTED-AMT ROUNDED = PBKM-CARRYFWD-AMT
                       * PBKM-MARGIN / 200
               WHEN PBKT-TRAN-CD = 'UP'
                   MOVE 12.00 TO PBKM-UNPOSTED-AMT
               WHEN PBKT-TRAN-CD = 'PR'
                   MOVE 20.00 TO PBKM-UNPOSTED-AMT
               WHEN OTHER
                   COMPUTE PBKM-UNPOSTED-AMT ROUNDED = PBKM-CARRYFWD-AMT
                       * PBKM-MARGIN / 100
           END-EVALUATE
           IF PBKM-UNPOSTED-AMT > 0 AND PBKM-UNPOSTED-AMT < 5.00
               MOVE 5.00 TO PBKM-UNPOSTED-AMT
           END-IF
           IF PBKM-UNPOSTED-AMT > 150.00
               MOVE 150.00 TO PBKM-UNPOSTED-AMT
           END-IF
           ADD PBKM-UNPOSTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE LINE YTD FEE ON THE INTEREST YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3720-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN PBKM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO PBKM-LINE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKT-REVERSAL-FLAG = 'Y'
                   COMPUTE PBKM-LINE-YTD ROUNDED = PBKM-INTEREST-YTD *
                       PBKM-MARGIN / 200
               WHEN PBKM-REGION-CD = 'ER'
                   MOVE 12.00 TO PBKM-LINE-YTD
               WHEN PBKM-REGION-CD = 'GL'
                   MOVE 5.00 TO PBKM-LINE-YTD
               WHEN OTHER
                   COMPUTE PBKM-LINE-YTD ROUNDED = PBKM-INTEREST-YTD *
                       PBKM-MARGIN / 100
           END-EVALUATE
           IF PBKM-LINE-YTD > 0 AND PBKM-LINE-YTD < 2.00
               MOVE 2.00 TO PBKM-LINE-YTD
           END-IF
           IF PBKM-LINE-YTD > 25.00
               MOVE 25.00 TO PBKM-LINE-YTD
           END-IF
           ADD PBKM-LINE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO PBKM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3740-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-CHANNEL.
           EVALUATE PBKT-CHANNEL-CD
               WHEN 'BRN'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF PBKM-NEXT-REVIEW-DATE > 0 AND
                       PBKM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO PBKT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE LINE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO UNPOSTED AMT.
      *----------------------------------------------------------------*
       3760-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-UNPOSTED-AMT ROUNDED =
                   PBKT-LINE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-UNPOSTED-AMT
           END-IF
           ADD PBKM-UNPOSTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3770-AGE-OPEN.
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
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD PBKT-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PRINTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LINE AMT.
      *----------------------------------------------------------------*
       3780-PRORATE-LINE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-LINE-AMT ROUNDED =
                   PBKM-PRINTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-LINE-AMT
           END-IF
           ADD PBKM-LINE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE LINE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BRANCH-CD
                   ADD PBKM-LINE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-LINE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3800-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * PBKM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBKT-CARRYFWD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CARRYFWD AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3810-FLAG-CARRYFWD.
           IF PBKM-CARRYFWD-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-CARRYFWD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BALANCE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3820-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = PBKM-BALANCE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = PBKM-BALANCE-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CARRYFWD YTD.
      *----------------------------------------------------------------*
       3830-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-CARRYFWD-YTD ROUNDED =
                   PBKT-PRINTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-CARRYFWD-YTD
           END-IF
           ADD PBKM-CARRYFWD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E726 NAME REQUIRED
      * E850 CARRYFWD AMT NEGATIVE
      * E051 CARRYFWD AMT OVER 250000
      * E503 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKM-NAME = SPACES
                   MOVE 'E726' TO WS-REJECT-CODE
               WHEN PBKM-CARRYFWD-AMT < 0
                   MOVE 'E850' TO WS-REJECT-CODE
               WHEN PBKM-CARRYFWD-AMT > 250000.00
                   MOVE 'E051' TO WS-REJECT-CODE
               WHEN PBKT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E503' TO WS-REJECT-CODE
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
       3850-PRORATE-BALANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-BALANCE-AMT ROUNDED =
                   PBKT-LINE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-BALANCE-AMT
           END-IF
           ADD PBKM-BALANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE INTEREST AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3860-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PBKT-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-REVIEW-FLAG = 'Y'
                   COMPUTE PBKT-INTEREST-AMT ROUNDED = PBKM-INTEREST-AMT
                       * PBKM-FEE-PCT / 200
               WHEN PBKM-CHANNEL-CD = 'B'
                   MOVE 10.00 TO PBKT-INTEREST-AMT
               WHEN PBKM-CHANNEL-CD = 'D'
                   MOVE 7.00 TO PBKT-INTEREST-AMT
               WHEN OTHER
                   COMPUTE PBKT-INTEREST-AMT ROUNDED = PBKM-INTEREST-AMT
                       * PBKM-FEE-PCT / 100
           END-EVALUATE
           IF PBKT-INTEREST-AMT > 0 AND PBKT-INTEREST-AMT < 1.00
               MOVE 1.00 TO PBKT-INTEREST-AMT
           END-IF
           IF PBKT-INTEREST-AMT > 25.00
               MOVE 25.00 TO PBKT-INTEREST-AMT
           END-IF
           ADD PBKT-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3870-SPLIT-BALANCE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE PBKM-BALANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3880-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = PBKM-INTEREST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = PBKM-INTEREST-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3890-DESCRIBE-TRAN.
           EVALUATE PBKT-TRAN-CD
               WHEN 'UP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'CF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CARRYFWD YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-CARRYFWD.
           IF PBKM-CARRYFWD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-CARRYFWD-YTD * PBKM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO PBKM-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3910-SPLIT-UNPOSTED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE PBKM-UNPOSTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3920-COUNT-PAYMENT.
           IF PBKM-PAYMENT-CNT < 9999999
               ADD 1 TO PBKM-PAYMENT-CNT
           END-IF
           IF PBKM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO PBKM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3930-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF PBKM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE PBKM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO PBKM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BALANCE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3940-FLAG-BALANCE.
           IF PBKM-BALANCE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE PBKM-BALANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3950-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-BAND-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-INTEREST-BAND-LOW(WS-TX)
                   MOVE WS-INTEREST-BAND-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3960-FLAG-INTEREST.
           IF PBKT-INTEREST-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE PBKT-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE UNPOSTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO UNPOSTED AMT.
      *----------------------------------------------------------------*
       3970-PRORATE-UNPOSTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-UNPOSTED-AMT ROUNDED =
                   PBKM-UNPOSTED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO PBKM-UNPOSTED-AMT
           END-IF
           ADD PBKM-UNPOSTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PRINTED GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3980-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINTED-GRID-RATE(1) TO PBKM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-PRINTED-GRID-LOW(WS-TX)
                   MOVE WS-PRINTED-GRID-RATE(WS-TX) TO PBKM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3990-COUNT-RETRY.
           IF PBKM-RETRY-CNT < 9999999
               ADD 1 TO PBKM-RETRY-CNT
           END-IF
           IF PBKM-RETRY-CNT NOT < 6
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BALANCE YTD FEE ON THE UNPOSTED AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4000-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN PBKM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO PBKM-BALANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE PBKM-BALANCE-YTD ROUNDED = PBKM-UNPOSTED-AMT
                       * PBKM-MARGIN / 200
               WHEN PBKM-BRANCH-CD = 'DF'
                   MOVE 3.00 TO PBKM-BALANCE-YTD
               WHEN PBKM-BRANCH-CD = 'NV'
                   MOVE 7.00 TO PBKM-BALANCE-YTD
               WHEN OTHER
                   COMPUTE PBKM-BALANCE-YTD ROUNDED = PBKM-UNPOSTED-AMT
                       * PBKM-MARGIN / 100
           END-EVALUATE
           IF PBKM-BALANCE-YTD > 0 AND PBKM-BALANCE-YTD < 10.00
               MOVE 10.00 TO PBKM-BALANCE-YTD
           END-IF
           IF PBKM-BALANCE-YTD > 150.00
               MOVE 150.00 TO PBKM-BALANCE-YTD
           END-IF
           ADD PBKM-BALANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E618 MEMO REQUIRED
      * E607 LINE AMT NEGATIVE
      * E893 LINE AMT OVER 5000
      * E366 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN PBKT-MEMO = SPACES
                   MOVE 'E618' TO WS-REJECT-CODE
               WHEN PBKM-LINE-AMT < 0
                   MOVE 'E607' TO WS-REJECT-CODE
               WHEN PBKM-LINE-AMT > 5000.00
                   MOVE 'E893' TO WS-REJECT-CODE
               WHEN PBKM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E366' TO WS-REJECT-CODE
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
       4020-DESCRIBE-REGION.
           EVALUATE PBKM-REGION-CD
               WHEN 'ER'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'VB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4030-CHECK-OPEN.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO PBKM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE UNPOSTED BAND TIER FOR THE PRINTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4040-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-UNPOSTED-BAND-RATE(1) TO PBKM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF PBKT-PRINTED-AMT NOT < WS-UNPOSTED-BAND-LOW(WS-TX)
                   MOVE WS-UNPOSTED-BAND-RATE(WS-TX) TO PBKM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * UNPOSTED YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4050-FLAG-UNPOSTED.
           IF PBKM-UNPOSTED-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE PBKM-UNPOSTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINTED YTD.
      *----------------------------------------------------------------*
       4060-PRORATE-PRINTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE PBKM-PRINTED-YTD ROUNDED =
                   PBKT-PRINTED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO PBKM-PRINTED-YTD
           END-IF
           ADD PBKM-PRINTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST BAND TIER FOR THE BALANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-BAND-RATE(1) TO PBKM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF PBKM-BALANCE-YTD NOT < WS-INTEREST-BAND-LOW(WS-TX)
                   MOVE WS-INTEREST-BAND-RATE(WS-TX) TO PBKM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF PBKM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO PBKM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-SOURCE-CD
                   ADD PBKM-BALANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-BALANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CARRYFWD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4090-PRORATE-CARRYFWD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   PBKM-CARRYFWD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4100-ACCUM-CHANNEL.
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
      * ASSESS THE PRINTED FEE ON THE CARRYFWD AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4110-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN PBKM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO PBKM-PRINTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN PBKM-EXEMPT-FLAG = 'Y'
                   COMPUTE PBKM-PRINTED-AMT ROUNDED = PBKM-CARRYFWD-AMT
                       * PBKM-MARGIN / 200
               WHEN PBKT-TRAN-CD = 'UP'
                   MOVE 12.00 TO PBKM-PRINTED-AMT
               WHEN PBKT-TRAN-CD = 'PR'
                   MOVE 10.00 TO PBKM-PRINTED-AMT
               WHEN OTHER
                   COMPUTE PBKM-PRINTED-AMT ROUNDED = PBKM-CARRYFWD-AMT
                       * PBKM-MARGIN / 100
           END-EVALUATE
           IF PBKM-PRINTED-AMT > 0 AND PBKM-PRINTED-AMT < 10.00
               MOVE 10.00 TO PBKM-PRINTED-AMT
           END-IF
           IF PBKM-PRINTED-AMT > 250.00
               MOVE 250.00 TO PBKM-PRINTED-AMT
           END-IF
           ADD PBKM-PRINTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4120-COUNT-ITEM.
           IF PBKM-ITEM-CNT < 9999999
               ADD 1 TO PBKM-ITEM-CNT
           END-IF
           IF PBKM-ITEM-CNT NOT < 3
               MOVE 'Y' TO PBKM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PRINTER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4130-DESCRIBE-PRINTER.
           EVALUATE PBKM-PRINTER-CD
               WHEN 'CB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'EX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> D WHEN EFF DATE IS PAST
      * D -> H WHEN EFF DATE IS PAST
      * H -> L WHEN EFF DATE IS PAST
      * L -> M WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4140-SET-CHANNEL.
           EVALUATE PBKM-CHANNEL-CD
               WHEN 'B'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF PBKT-EFF-DATE > 0 AND PBKT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO PBKM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BALANCE YTD BY BOOK TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4150-ACCUM-BOOK-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = PBKM-BOOK-TYPE-CD
                   ADD PBKM-BALANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE PBKM-BOOK-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE PBKM-BALANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

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
                   MOVE PBDTE-CARRYFWD-AMT
                     TO PBKM-CARRYFWD-AMT
                   MOVE PBDTE-REVIEW-FLAG
                     TO PBKM-REVIEW-FLAG
                   MOVE PBDTE-SUSPEND-FLAG
                     TO PBKM-SUSPEND-FLAG
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
           DISPLAY 'PBKBILL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'PBKBILL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
