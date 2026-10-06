      *================================================================*
      * PROGRAM  : DRPXMIT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DIVIDEND REINVESTMENT - OUTBOUND TRANSMISSION. READS*
      *            THE DRP TRANSACTIONS, UPDATES THE DRP MASTER AND    *
      *            PRODUCES THE CONTROL REPORT. BANK HOLDING COMPANY   *
      *            STOCK DIVIDEND REINVESTMENT PLAN                    *
      * INPUT    : DRPMST (KSDS), DRPTRN, BANKCTL                      *
      * OUTPUT   : DRPOUT, DRPXMRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, DRSTAT                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-05-24 DPS  ORIGINAL                                       *
      * 1987-09-14 CRP  CASH-PAY ROUNDING - HALF UP INC4639            *
      * 1987-05-03 JLM  REINVEST EXCEPTIONS TO OPS QUEUE PRB8059       *
      * 1992-08-15 DPS  WITHHOLD ROUNDING - HALF UP INC1208            *
      * 1993-09-23 EWS  FEE LIMIT REVISED PRB6630                      *
      * 1994-11-25 JLM  WITHHOLD CUTOFF MOVED INC4482                  *
      * 1999-10-11 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DRPXMIT.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "DRPMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS DRPM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "DRPTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "DRPOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "DRPXMRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY DRPMST.
       FD  TRAN-FILE.
       COPY DRPTRN.
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
      * WS-WITHHOLD-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WITHHOLD-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000464500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000111000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000305500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000266500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005850000000013500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005950000000368000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006450000000221500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008950000000563000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013950000000149500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014200000000047500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014300000000376000'.
       01  WS-WITHHOLD-GRID-TBL REDEFINES WS-WITHHOLD-GRID-VALUES.
           05  WS-WITHHOLD-GRID-ROW OCCURS 11 TIMES.
               10  WS-WITHHOLD-GRID-LOW    PIC 9(09)V99.
               10  WS-WITHHOLD-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SHARES-A-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHARES-A-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000616000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000456500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000888500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000646500'.
       01  WS-SHARES-A-SCHED-TBL REDEFINES WS-SHARES-A-SCHED-VALUES.
           05  WS-SHARES-A-SCHED-ROW OCCURS 4 TIMES.
               10  WS-SHARES-A-SCHED-LOW   PIC 9(09)V99.
               10  WS-SHARES-A-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-PAY-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-PAY-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000473000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000900000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000679500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000467000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000869000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000025500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000849000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012250000000675000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012350000000424500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012850000000270500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013350000000612000'.
       01  WS-CASH-PAY-TIER-TBL REDEFINES WS-CASH-PAY-TIER-VALUES.
           05  WS-CASH-PAY-TIER-ROW OCCURS 11 TIMES.
               10  WS-CASH-PAY-TIER-LOW    PIC 9(09)V99.
               10  WS-CASH-PAY-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000670000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000028000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000570000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000666500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000333500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000753500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016100000000425000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021100000000423000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021200000000332500'.
       01  WS-FEE-GRID-TBL REDEFINES WS-FEE-GRID-VALUES.
           05  WS-FEE-GRID-ROW OCCURS 9 TIMES.
               10  WS-FEE-GRID-LOW         PIC 9(09)V99.
               10  WS-FEE-GRID-RATE        PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  DRPXMIT  DIVIDEND REINVESTMENT'.
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
       COPY DRSTATP.

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
               MOVE 'DRPMST' TO ABND-FILE-NAME
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
           MOVE DRPT-KEY TO DRPM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-CHECK-LAST-PAY
               PERFORM 3010-SET-HOLDER-TYPE
               PERFORM 3020-DESCRIBE-REASON
               PERFORM 3030-AGE-EFF
               PERFORM 3040-COUNT-PAYMENT
               PERFORM 3050-CHECK-LAST-MAINT
               PERFORM 3060-FLAG-REINVEST
               PERFORM 3070-ACCRUE-FEE
               PERFORM 3080-COUNT-PAYMENT
               PERFORM 3090-SET-HOLDER-TYPE
               PERFORM 3100-COUNT-NOTICE
               PERFORM 3110-SPLIT-REINVEST
               PERFORM 3120-SPLIT-DIVIDEND
               PERFORM 3130-LIMIT-WITHHOLD
               PERFORM 3140-SPLIT-SHARES-AMT
               PERFORM 3150-ACCUM-TRAN
               PERFORM 3160-ASSESS-REVERSAL
               PERFORM 3170-CHECK-POST
               PERFORM 3180-ACCRUE-DIVIDEND
               PERFORM 3190-LIMIT-REINVEST
               PERFORM 3200-CHECK-LAST-MAINT
               PERFORM 3210-SELECT-DISCOUNT
               PERFORM 3220-SET-REASON
               PERFORM 3230-AGE-MATURITY
               PERFORM 3240-CHECK-LAST-ACT
               PERFORM 3250-ASSESS-STOP
               PERFORM 3260-SET-PLAN-OPTION
               PERFORM 3270-PRORATE-WITHHOLD
               PERFORM 3280-ACCUM-SOURCE
               PERFORM 3290-PRORATE-DIVIDEND
               PERFORM 3300-SPLIT-REINVEST
               PERFORM 3310-ASSESS-SUSPEND
               PERFORM 3320-CHECK-EFF
               PERFORM 3330-DESCRIBE-HOLDER-TYPE
               PERFORM 3340-ASSESS-REVERSAL
               PERFORM 3350-AGE-EFFECTIVE
               PERFORM 3360-SELECT-PENALTY-PCT
               PERFORM 3370-DESCRIBE-HOLDER-TYPE
               PERFORM 3380-CHECK-EFFECTIVE
               PERFORM 3390-EDIT-CUST-ID
               PERFORM 3400-EDIT-OPERATOR
               PERFORM 3410-SET-CHANNEL
               PERFORM 3420-LIMIT-REINVEST
               PERFORM 3430-DESCRIBE-CYCLE
               PERFORM 3440-EDIT-CUST-ID
               PERFORM 3450-SELECT-DISCOUNT
               PERFORM 3460-CHECK-LAST-ACT
               PERFORM 3470-PRORATE-DIVIDEND
               PERFORM 3480-SET-TRAN
               PERFORM 3490-PRORATE-SHARES-AMT
               PERFORM 3500-FLAG-SHARES-AMT
               PERFORM 3510-PRORATE-CASH-PAY
               PERFORM 3520-ASSESS-SUSPEND
               PERFORM 3530-ACCUM-REASON
               PERFORM 3540-COUNT-EXCEPT
               PERFORM 3550-LIMIT-REINVEST
               PERFORM 3560-SET-CYCLE
               PERFORM 3570-ACCUM-CHANNEL
               PERFORM 3580-FLAG-FEE
               PERFORM 3590-ACCUM-CHANNEL
               PERFORM 3600-PRORATE-FEE
               PERFORM 3610-FLAG-FEE
               PERFORM 3620-SELECT-DISCOUNT
               PERFORM 3630-EDIT-ADDR
               PERFORM 3640-ACCRUE-CASH-PAY
               PERFORM 3650-CHECK-LAST-PAY
               PERFORM 3660-PRORATE-FEE
               PERFORM 3670-ACCRUE-DISCOUNT
               PERFORM 3680-CHECK-LAST-PAY
               PERFORM 3690-ASSESS-REVERSAL
               PERFORM 3700-CHECK-OPEN
               PERFORM 3710-SPLIT-REINVEST
               PERFORM 3720-AGE-LAST-PAY
               PERFORM 3730-FLAG-WITHHOLD
               PERFORM 3740-ASSESS-SUSPEND
               PERFORM 3750-COUNT-EXCEPT
               PERFORM 3760-SPLIT-REINVEST
               PERFORM 3770-SET-SOURCE
               PERFORM 3780-ACCUM-CHANNEL
               PERFORM 3790-DESCRIBE-CHANNEL
               PERFORM 3800-CHECK-NEXT-DUE
               PERFORM 3810-ACCUM-CHANNEL
               PERFORM 3820-SPLIT-CASH-PAY
               PERFORM 3830-AGE-OPEN
               PERFORM 3840-SET-CYCLE
               PERFORM 3850-ACCUM-CYCLE
               PERFORM 3860-EDIT-ADDR
               PERFORM 3870-DESCRIBE-HOLDER-TYPE
               PERFORM 3880-EDIT-REF-NO
               PERFORM 3890-PRORATE-WITHHOLD
               PERFORM 3900-DESCRIBE-TRAN
               PERFORM 3910-DESCRIBE-SOURCE
               PERFORM 3920-SELECT-RATE
               PERFORM 3930-FLAG-SHARES-AMT
               PERFORM 3940-ACCUM-CHANNEL
               PERFORM 3950-DESCRIBE-HOLDER-TYPE
               PERFORM 3960-EDIT-DESC
               PERFORM 3970-ASSESS-EXEMPT
               PERFORM 3980-SELECT-PENALTY-PCT
               PERFORM 3990-FLAG-ITEM
               PERFORM 4000-SPLIT-CASH-PAY
               PERFORM 4010-FLAG-DIVIDEND
               PERFORM 4020-DESCRIBE-CHANNEL
               PERFORM 4030-DESCRIBE-CYCLE
               PERFORM 4040-SET-HOLDER-TYPE
               PERFORM 4050-ACCUM-TRAN
               PERFORM 4060-SPLIT-SHARES-AMT
               PERFORM 4070-ACCRUE-CASH-PAY
               PERFORM 4080-ACCRUE-DISCOUNT
               PERFORM 4090-ASSESS-AUTO-RENEW
               PERFORM 4100-LIMIT-FEE
               PERFORM 4110-SPLIT-SHARES-AMT
               PERFORM 4120-EDIT-ADDR
               PERFORM 4130-DESCRIBE-CYCLE
               PERFORM 4140-DESCRIBE-HOLDER-TYPE
               PERFORM 4150-FLAG-REINVEST
               PERFORM 4160-SPLIT-SHARES-AMT
               PERFORM 4170-SPLIT-FEE
               PERFORM 4180-SPLIT-FEE
               PERFORM 4190-SET-REASON
               PERFORM 4200-ASSESS-PREFERRED
               PERFORM 4210-SET-CYCLE
               PERFORM 4220-DESCRIBE-REASON
               PERFORM 4230-SELECT-DISCOUNT
               PERFORM 4240-COUNT-ITEM
               PERFORM 4250-EDIT-ADDR
               PERFORM 4260-ASSESS-SUSPEND
               PERFORM 4270-EDIT-OPERATOR
               PERFORM 4280-LIMIT-REINVEST
               PERFORM 4290-DESCRIBE-PLAN-OPTION
               PERFORM 4300-EDIT-ADDR
               PERFORM 4310-ACCRUE-CASH-PAY
               PERFORM 4320-LIMIT-ITEM
               PERFORM 4330-SELECT-DISCOUNT
               PERFORM 4340-PRORATE-REINVEST
               PERFORM 4350-CHECK-LAST-PAY
               PERFORM 4360-DESCRIBE-PLAN-OPTION
               PERFORM 4370-SPLIT-SHARES-AMT
               PERFORM 4380-ASSESS-PREFERRED
               PERFORM 4390-EDIT-OPERATOR
               PERFORM 4400-ACCRUE-SHARES-AMT
               PERFORM 4410-SELECT-RATE
               PERFORM 4420-COUNT-ITEM
               PERFORM 4430-ACCUM-CYCLE
               PERFORM 4440-ACCRUE-SHARES-AMT
               PERFORM 4450-AGE-NEXT-REVIEW
               PERFORM 4460-ASSESS-PREFERRED
               PERFORM 4470-DESCRIBE-PLAN-OPTION
               PERFORM 4480-PRORATE-FEE
               PERFORM 4490-ASSESS-SUSPEND
               PERFORM 4500-CHECK-MATURITY
               PERFORM 4510-SPLIT-SHARES-AMT
               PERFORM 4520-PRORATE-SHARES-AMT
               PERFORM 4530-FLAG-ITEM
               PERFORM 7000-CALL-DRSTAT
               REWRITE DRP-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'DRPMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM DRP-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3000-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO DRPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN LAST PAY DATE IS PAST
      * H -> K WHEN LAST PAY DATE IS PAST
      * K -> L WHEN LAST PAY DATE IS PAST
      * L -> M WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   IF DRPM-LAST-PAY-DATE > 0 AND DRPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF DRPM-LAST-PAY-DATE > 0 AND DRPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPM-LAST-PAY-DATE > 0 AND DRPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF DRPM-LAST-PAY-DATE > 0 AND DRPM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3020-DESCRIBE-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'G'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'J'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'T'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Y'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-PAYMENT.
           IF DRPM-PAYMENT-CNT < 9999999
               ADD 1 TO DRPM-PAYMENT-CNT
           END-IF
           IF DRPM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3050-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3060-FLAG-REINVEST.
           IF DRPT-REINVEST-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPT-REINVEST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3070-ACCRUE-FEE.
           IF DRPM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-FEE-AMT * DRPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO DRPT-SHARES-AMT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3080-COUNT-PAYMENT.
           IF DRPM-PAYMENT-CNT < 9999999
               ADD 1 TO DRPM-PAYMENT-CNT
           END-IF
           IF DRPM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN MATURITY DATE IS PAST
      * H -> K WHEN MATURITY DATE IS PAST
      * K -> L WHEN MATURITY DATE IS PAST
      * L -> M WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-NOTICE.
           IF DRPM-NOTICE-CNT < 9999999
               ADD 1 TO DRPM-NOTICE-CNT
           END-IF
           IF DRPM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPT-REINVEST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = DRPT-REINVEST-AMT - (WS-WORK-AMT * 4)
           COMPUTE DRPM-REINVEST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DIVIDEND AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3120-SPLIT-DIVIDEND.
           COMPUTE WS-WORK-AMT = DRPT-DIVIDEND-AMT / 4
           COMPUTE WS-WORK-AMT-2 = DRPT-DIVIDEND-AMT - (WS-WORK-AMT * 4)
           COMPUTE DRPM-WITHHOLD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3130-LIMIT-WITHHOLD.
           IF DRPM-WITHHOLD-AMT < 50.00
               MOVE 50.00 TO DRPM-WITHHOLD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-WITHHOLD-AMT > 10000.00
               MOVE 10000.00 TO DRPM-WITHHOLD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3140-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-YTD - (WS-WORK-AMT *
               4)
           COMPUTE DRPM-DIVIDEND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3150-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-TRAN-CD
                   ADD DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WITHHOLD YTD FEE ON THE CASH PAY AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3160-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO DRPM-WITHHOLD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   COMPUTE DRPM-WITHHOLD-YTD ROUNDED = DRPM-CASH-PAY-AMT
                       * DRPM-DISCOUNT / 200
               WHEN DRPT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO DRPM-WITHHOLD-YTD
               WHEN DRPT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO DRPM-WITHHOLD-YTD
               WHEN OTHER
                   COMPUTE DRPM-WITHHOLD-YTD ROUNDED = DRPM-CASH-PAY-AMT
                       * DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-WITHHOLD-YTD > 0 AND DRPM-WITHHOLD-YTD < 1.00
               MOVE 1.00 TO DRPM-WITHHOLD-YTD
           END-IF
           IF DRPM-WITHHOLD-YTD > 35.00
               MOVE 35.00 TO DRPM-WITHHOLD-YTD
           END-IF
           ADD DRPM-WITHHOLD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3170-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF DRPT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DIVIDEND YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3180-ACCRUE-DIVIDEND.
           IF DRPM-DIVIDEND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-DIVIDEND-YTD * DRPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3190-LIMIT-REINVEST.
           IF DRPT-REINVEST-AMT < 0.00
               MOVE 0.00 TO DRPT-REINVEST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPT-REINVEST-AMT > 1000.00
               MOVE 1000.00 TO DRPT-REINVEST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A SCHED TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-SCHED-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF DRPM-FEE-YTD NOT < WS-SHARES-A-SCHED-LOW(WS-TX)
                   MOVE WS-SHARES-A-SCHED-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> G WHEN OPEN DATE IS PAST
      * G -> J WHEN OPEN DATE IS PAST
      * J -> K WHEN OPEN DATE IS PAST
      * K -> L WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3220-SET-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3230-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES FEE ON THE DIVIDEND AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3250-ASSESS-STOP.
           EVALUATE TRUE
               WHEN DRPM-STOP-FLAG = 'Y'
                   MOVE ZERO TO DRPM-SHARES-AMT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   COMPUTE DRPM-SHARES-AMT-AMT ROUNDED =
                       DRPT-DIVIDEND-AMT * DRPM-RATE / 200
               WHEN DRPM-REASON-CD = 'B'
                   MOVE 15.00 TO DRPM-SHARES-AMT-AMT
               WHEN DRPM-REASON-CD = 'G'
                   MOVE 20.00 TO DRPM-SHARES-AMT-AMT
               WHEN OTHER
                   COMPUTE DRPM-SHARES-AMT-AMT ROUNDED =
                       DRPT-DIVIDEND-AMT * DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-SHARES-AMT-AMT > 0 AND DRPM-SHARES-AMT-AMT < 10.00
               MOVE 10.00 TO DRPM-SHARES-AMT-AMT
           END-IF
           IF DRPM-SHARES-AMT-AMT > 35.00
               MOVE 35.00 TO DRPM-SHARES-AMT-AMT
           END-IF
           ADD DRPM-SHARES-AMT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN OPTION CD:
      * E -> R WHEN LAST MAINT DATE IS PAST
      * R -> T WHEN LAST MAINT DATE IS PAST
      * T -> Z WHEN LAST MAINT DATE IS PAST
      * Z -> E WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3260-SET-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'R' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'T' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'Z' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'E' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WITHHOLD YTD.
      *----------------------------------------------------------------*
       3270-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-WITHHOLD-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-WITHHOLD-YTD
           END-IF
           ADD DRPM-WITHHOLD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3280-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-SOURCE-CD
                   ADD DRPT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DIVIDEND AMT.
      *----------------------------------------------------------------*
       3290-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-DIVIDEND-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-DIVIDEND-AMT
           END-IF
           ADD DRPM-DIVIDEND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT REINVEST YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3300-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPM-REINVEST-YTD / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-REINVEST-YTD - (WS-WORK-AMT *
               12)
           COMPUTE DRPT-REINVEST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REINVEST YTD FEE ON THE DIVIDEND AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3310-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPM-REINVEST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   COMPUTE DRPM-REINVEST-YTD ROUNDED = DRPT-DIVIDEND-AMT
                       * DRPM-PENALTY-PCT / 200
               WHEN DRPT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO DRPM-REINVEST-YTD
               WHEN DRPT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO DRPM-REINVEST-YTD
               WHEN OTHER
                   COMPUTE DRPM-REINVEST-YTD ROUNDED = DRPT-DIVIDEND-AMT
                       * DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-REINVEST-YTD > 0 AND DRPM-REINVEST-YTD < 2.00
               MOVE 2.00 TO DRPM-REINVEST-YTD
           END-IF
           IF DRPM-REINVEST-YTD > 50.00
               MOVE 50.00 TO DRPM-REINVEST-YTD
           END-IF
           ADD DRPM-REINVEST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3320-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF DRPT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3330-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH PAY YTD FEE ON THE REINVEST YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3340-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO DRPM-CASH-PAY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   COMPUTE DRPM-CASH-PAY-YTD ROUNDED = DRPM-REINVEST-YTD
                       * DRPM-RATE / 200
               WHEN DRPM-SOURCE-CD = 'BT'
                   MOVE 10.00 TO DRPM-CASH-PAY-YTD
               WHEN DRPM-SOURCE-CD = 'FL'
                   MOVE 20.00 TO DRPM-CASH-PAY-YTD
               WHEN OTHER
                   COMPUTE DRPM-CASH-PAY-YTD ROUNDED = DRPM-REINVEST-YTD
                       * DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-CASH-PAY-YTD > 0 AND DRPM-CASH-PAY-YTD < 5.00
               MOVE 5.00 TO DRPM-CASH-PAY-YTD
           END-IF
           IF DRPM-CASH-PAY-YTD > 75.00
               MOVE 75.00 TO DRPM-CASH-PAY-YTD
           END-IF
           ADD DRPM-CASH-PAY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3350-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE REINVEST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3360-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF DRPT-REINVEST-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3370-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'K'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3380-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E641 CUST ID REQUIRED
      * E186 SHARES AMT YTD NEGATIVE
      * E037 SHARES AMT YTD OVER 1000000
      * E368 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3390-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CUST-ID = SPACES
                   MOVE 'E641' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-YTD < 0
                   MOVE 'E186' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-YTD > 1000000.00
                   MOVE 'E037' TO WS-REJECT-CODE
               WHEN DRPM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E368' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E587 OPERATOR REQUIRED
      * E761 CASH PAY YTD NEGATIVE
      * E796 CASH PAY YTD OVER 1000000
      * E114 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3400-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-OPERATOR = SPACES
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-YTD < 0
                   MOVE 'E761' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-YTD > 1000000.00
                   MOVE 'E796' TO WS-REJECT-CODE
               WHEN DRPM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E114' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3410-SET-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REINVEST YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3420-LIMIT-REINVEST.
           IF DRPM-REINVEST-YTD < 25.00
               MOVE 25.00 TO DRPM-REINVEST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-REINVEST-YTD > 500.00
               MOVE 500.00 TO DRPM-REINVEST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3430-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E091 CUST ID REQUIRED
      * E106 FEE YTD NEGATIVE
      * E581 FEE YTD OVER 99999
      * E984 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3440-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CUST-ID = SPACES
                   MOVE 'E091' TO WS-REJECT-CODE
               WHEN DRPM-FEE-YTD < 0
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN DRPM-FEE-YTD > 99999.00
                   MOVE 'E581' TO WS-REJECT-CODE
               WHEN DRPM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E984' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A SCHED TIER FOR THE WITHHOLD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-SCHED-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF DRPM-WITHHOLD-AMT NOT < WS-SHARES-A-SCHED-LOW(WS-TX)
                   MOVE WS-SHARES-A-SCHED-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3460-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO DRPT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH PAY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DIVIDEND YTD.
      *----------------------------------------------------------------*
       3470-PRORATE-DIVIDEND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-DIVIDEND-YTD ROUNDED =
                   DRPM-CASH-PAY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-DIVIDEND-YTD
           END-IF
           ADD DRPM-DIVIDEND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DV -> RI WHEN LAST ACT DATE IS PAST
      * RI -> CP WHEN LAST ACT DATE IS PAST
      * CP -> WH WHEN LAST ACT DATE IS PAST
      * WH -> SL WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RI' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RI'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WH'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SL' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHARES AMT AMT.
      *----------------------------------------------------------------*
       3490-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPT-SHARES-AMT-AMT ROUNDED =
                   DRPM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPT-SHARES-AMT-AMT
           END-IF
           ADD DRPT-SHARES-AMT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SHARES AMT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3500-FLAG-SHARES-AMT.
           IF DRPM-SHARES-AMT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-SHARES-AMT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH PAY AMT.
      *----------------------------------------------------------------*
       3510-PRORATE-CASH-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-CASH-PAY-AMT ROUNDED =
                   DRPT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-CASH-PAY-AMT
           END-IF
           ADD DRPM-CASH-PAY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE CASH PAY AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3520-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPT-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPM-CASH-PAY-AMT
                       * DRPM-DISCOUNT / 200
               WHEN DRPM-CYCLE-CD = 'DE'
                   MOVE 5.00 TO DRPT-DIVIDEND-AMT
               WHEN DRPM-CYCLE-CD = 'FB'
                   MOVE 12.00 TO DRPT-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPM-CASH-PAY-AMT
                       * DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPT-DIVIDEND-AMT > 0 AND DRPT-DIVIDEND-AMT < 2.00
               MOVE 2.00 TO DRPT-DIVIDEND-AMT
           END-IF
           IF DRPT-DIVIDEND-AMT > 50.00
               MOVE 50.00 TO DRPT-DIVIDEND-AMT
           END-IF
           ADD DRPT-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE SHARES AMT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3530-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-REASON-CD
                   ADD DRPT-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3540-COUNT-EXCEPT.
           IF DRPM-EXCEPT-CNT < 9999999
               ADD 1 TO DRPM-EXCEPT-CNT
           END-IF
           IF DRPM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3550-LIMIT-REINVEST.
           IF DRPM-REINVEST-YTD < 25.00
               MOVE 25.00 TO DRPM-REINVEST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-REINVEST-YTD > 10000.00
               MOVE 10000.00 TO DRPM-REINVEST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN EFFECTIVE DATE IS PAST
      * FB -> PC WHEN EFFECTIVE DATE IS PAST
      * PC -> WY WHEN EFFECTIVE DATE IS PAST
      * WY -> XD WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3560-SET-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CHANNEL-CD
                   ADD DRPM-DIVIDEND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-DIVIDEND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3580-FLAG-FEE.
           IF DRPT-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3590-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CHANNEL-CD
                   ADD DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3600-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-FEE-YTD ROUNDED =
                   DRPM-SHARES-AMT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-FEE-YTD
           END-IF
           ADD DRPM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3610-FLAG-FEE.
           IF DRPM-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHARES A SCHED TIER FOR THE REINVEST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SHARES-A-SCHED-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF DRPT-REINVEST-AMT NOT < WS-SHARES-A-SCHED-LOW(WS-TX)
                   MOVE WS-SHARES-A-SCHED-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E188 ADDR REQUIRED
      * E002 REINVEST AMT NEGATIVE
      * E505 REINVEST AMT OVER 5000
      * E485 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-ADDR = SPACES
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-AMT < 0
                   MOVE 'E002' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-AMT > 5000.00
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN DRPM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E485' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH PAY AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3640-ACCRUE-CASH-PAY.
           IF DRPM-CASH-PAY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-CASH-PAY-AMT * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-DIVIDEND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WITHHOLD YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3660-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-FEE-YTD ROUNDED =
                   DRPM-WITHHOLD-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-FEE-YTD
           END-IF
           ADD DRPM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3670-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-CASH-PAY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3680-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES YTD FEE ON THE FEE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3690-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO DRPM-SHARES-AMT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED = DRPM-FEE-YTD *
                       DRPM-PENALTY-PCT / 200
               WHEN DRPM-CHANNEL-CD = 'RM'
                   MOVE 15.00 TO DRPM-SHARES-AMT-YTD
               WHEN DRPM-CHANNEL-CD = 'VP'
                   MOVE 5.00 TO DRPM-SHARES-AMT-YTD
               WHEN OTHER
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED = DRPM-FEE-YTD *
                       DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-SHARES-AMT-YTD > 0 AND DRPM-SHARES-AMT-YTD < 1.00
               MOVE 1.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           IF DRPM-SHARES-AMT-YTD > 35.00
               MOVE 35.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           ADD DRPM-SHARES-AMT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3700-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRPM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO DRPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3710-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPM-REINVEST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-REINVEST-AMT - (WS-WORK-AMT *
               12)
           COMPUTE DRPM-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3720-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-PAY-DATE > 0 AND DRPM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-CASH-PAY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WITHHOLD YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3730-FLAG-WITHHOLD.
           IF DRPM-WITHHOLD-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-WITHHOLD-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE FEE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3740-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPM-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-PREFERRED-FLAG = 'Y'
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED = DRPM-FEE-YTD *
                       DRPM-RATE / 200
               WHEN DRPM-CHANNEL-CD = 'RM'
                   MOVE 15.00 TO DRPM-DIVIDEND-AMT
               WHEN DRPM-CHANNEL-CD = 'VP'
                   MOVE 10.00 TO DRPM-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED = DRPM-FEE-YTD *
                       DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-DIVIDEND-AMT > 0 AND DRPM-DIVIDEND-AMT < 10.00
               MOVE 10.00 TO DRPM-DIVIDEND-AMT
           END-IF
           IF DRPM-DIVIDEND-AMT > 50.00
               MOVE 50.00 TO DRPM-DIVIDEND-AMT
           END-IF
           ADD DRPM-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3750-COUNT-EXCEPT.
           IF DRPM-EXCEPT-CNT < 9999999
               ADD 1 TO DRPM-EXCEPT-CNT
           END-IF
           IF DRPM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPT-REINVEST-AMT / 2
           COMPUTE WS-WORK-AMT-2 = DRPT-REINVEST-AMT - (WS-WORK-AMT * 2)
           COMPUTE DRPT-DIVIDEND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BT -> FL WHEN POST DATE IS PAST
      * FL -> NK WHEN POST DATE IS PAST
      * NK -> YC WHEN POST DATE IS PAST
      * YC -> BT WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3770-SET-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NK' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NK'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CHANNEL-CD
                   ADD DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3790-DESCRIBE-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3810-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-CHANNEL-CD
                   ADD DRPT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3820-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRPM-CASH-PAY-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRPM-CASH-PAY-AMT - (WS-WORK-AMT * 3)
           COMPUTE DRPM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-WITHHOLD-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN EFF DATE IS PAST
      * FB -> PC WHEN EFF DATE IS PAST
      * PC -> WY WHEN EFF DATE IS PAST
      * WY -> XD WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRPT-EFF-DATE > 0 AND DRPT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3850-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CYCLE-CD
                   ADD DRPM-DIVIDEND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-DIVIDEND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E162 ADDR REQUIRED
      * E419 CASH PAY AMT NEGATIVE
      * E364 CASH PAY AMT OVER 5000
      * E801 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3860-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-ADDR = SPACES
                   MOVE 'E162' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-AMT < 0
                   MOVE 'E419' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-AMT > 5000.00
                   MOVE 'E364' TO WS-REJECT-CODE
               WHEN DRPM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E801' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E866 REF NO REQUIRED
      * E859 WORK AMT NEGATIVE
      * E983 WORK AMT OVER 5000
      * E445 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3880-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-REF-NO = SPACES
                   MOVE 'E866' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E859' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E983' TO WS-REJECT-CODE
               WHEN DRPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E445' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DIVIDEND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WITHHOLD YTD.
      *----------------------------------------------------------------*
       3890-PRORATE-WITHHOLD.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-WITHHOLD-YTD ROUNDED =
                   DRPM-DIVIDEND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-WITHHOLD-YTD
           END-IF
           ADD DRPM-WITHHOLD-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3900-DESCRIBE-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RI'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3910-DESCRIBE-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3920-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF DRPM-FEE-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHARES AMT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3930-FLAG-SHARES-AMT.
           IF DRPT-SHARES-AMT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPT-SHARES-AMT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-CHANNEL-CD
                   ADD DRPM-DIVIDEND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-DIVIDEND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3950-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'X'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E089 DESC REQUIRED
      * E856 WITHHOLD AMT NEGATIVE
      * E730 WITHHOLD AMT OVER 250000
      * E402 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-DESC = SPACES
                   MOVE 'E089' TO WS-REJECT-CODE
               WHEN DRPM-WITHHOLD-AMT < 0
                   MOVE 'E856' TO WS-REJECT-CODE
               WHEN DRPM-WITHHOLD-AMT > 250000.00
                   MOVE 'E730' TO WS-REJECT-CODE
               WHEN DRPM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E402' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE CASH PAY YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3970-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = DRPM-CASH-PAY-YTD *
                       DRPM-DISCOUNT / 200
               WHEN DRPT-TRAN-CD = 'DV'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN DRPT-TRAN-CD = 'RI'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = DRPM-CASH-PAY-YTD *
                       DRPM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CASH PAY TIER TIER FOR THE SHARES AMT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3980-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-PAY-TIER-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPT-SHARES-AMT-AMT NOT < WS-CASH-PAY-TIER-LOW(WS-TX)
                   MOVE WS-CASH-PAY-TIER-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3990-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRPM-CASH-PAY-AMT / 3
           COMPUTE WS-WORK-AMT-2 = DRPM-CASH-PAY-AMT - (WS-WORK-AMT * 3)
           COMPUTE DRPM-WITHHOLD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DIVIDEND YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4010-FLAG-DIVIDEND.
           IF DRPM-DIVIDEND-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-DIVIDEND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4020-DESCRIBE-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4030-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR HOLDER TYPE CD:
      * C -> H WHEN MATURITY DATE IS PAST
      * H -> K WHEN MATURITY DATE IS PAST
      * K -> L WHEN MATURITY DATE IS PAST
      * L -> M WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4040-SET-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO DRPM-HOLDER-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4050-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-TRAN-CD
                   ADD DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4060-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH PAY AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4070-ACCRUE-CASH-PAY.
           IF DRPM-CASH-PAY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-CASH-PAY-AMT * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPT-SHARES-AMT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4080-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE WITHHOLD YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4090-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO DRPM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-PREFERRED-FLAG = 'Y'
                   COMPUTE DRPM-FEE-AMT ROUNDED = DRPM-WITHHOLD-YTD *
                       DRPM-DISCOUNT / 200
               WHEN DRPT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO DRPM-FEE-AMT
               WHEN DRPT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO DRPM-FEE-AMT
               WHEN OTHER
                   COMPUTE DRPM-FEE-AMT ROUNDED = DRPM-WITHHOLD-YTD *
                       DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-FEE-AMT > 0 AND DRPM-FEE-AMT < 10.00
               MOVE 10.00 TO DRPM-FEE-AMT
           END-IF
           IF DRPM-FEE-AMT > 75.00
               MOVE 75.00 TO DRPM-FEE-AMT
           END-IF
           ADD DRPM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4100-LIMIT-FEE.
           IF DRPT-FEE-AMT < 100.00
               MOVE 100.00 TO DRPT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPT-FEE-AMT > 1000.00
               MOVE 1000.00 TO DRPT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4110-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE DRPT-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E948 ADDR REQUIRED
      * E523 CASH PAY YTD NEGATIVE
      * E762 CASH PAY YTD OVER 25000
      * E813 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4120-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-ADDR = SPACES
                   MOVE 'E948' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-YTD < 0
                   MOVE 'E523' TO WS-REJECT-CODE
               WHEN DRPM-CASH-PAY-YTD > 25000.00
                   MOVE 'E762' TO WS-REJECT-CODE
               WHEN DRPT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E813' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4130-DESCRIBE-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4140-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REINVEST YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4150-FLAG-REINVEST.
           IF DRPM-REINVEST-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4160-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-AMT - (WS-WORK-AMT *
               2)
           COMPUTE DRPM-DIVIDEND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4170-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRPT-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPT-FEE-AMT - (WS-WORK-AMT * 12)
           COMPUTE DRPM-REINVEST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4180-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = DRPT-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = DRPT-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE DRPM-WITHHOLD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> G WHEN NOTICE DATE IS PAST
      * G -> J WHEN NOTICE DATE IS PAST
      * J -> K WHEN NOTICE DATE IS PAST
      * K -> L WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4190-SET-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WITHHOLD YTD FEE ON THE REINVEST YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4200-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN DRPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO DRPM-WITHHOLD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   COMPUTE DRPM-WITHHOLD-YTD ROUNDED = DRPM-REINVEST-YTD
                       * DRPM-PENALTY-PCT / 200
               WHEN DRPM-REASON-CD = 'B'
                   MOVE 20.00 TO DRPM-WITHHOLD-YTD
               WHEN DRPM-REASON-CD = 'G'
                   MOVE 10.00 TO DRPM-WITHHOLD-YTD
               WHEN OTHER
                   COMPUTE DRPM-WITHHOLD-YTD ROUNDED = DRPM-REINVEST-YTD
                       * DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-WITHHOLD-YTD > 0 AND DRPM-WITHHOLD-YTD < 10.00
               MOVE 10.00 TO DRPM-WITHHOLD-YTD
           END-IF
           IF DRPM-WITHHOLD-YTD > 100.00
               MOVE 100.00 TO DRPM-WITHHOLD-YTD
           END-IF
           ADD DRPM-WITHHOLD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN NEXT REVIEW DATE IS PAST
      * FB -> PC WHEN NEXT REVIEW DATE IS PAST
      * PC -> WY WHEN NEXT REVIEW DATE IS PAST
      * WY -> XD WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'G'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'J'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASH PAY TIER TIER FOR THE WITHHOLD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-PAY-TIER-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPM-WITHHOLD-AMT NOT < WS-CASH-PAY-TIER-LOW(WS-TX)
                   MOVE WS-CASH-PAY-TIER-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4240-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 6
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E885 ADDR REQUIRED
      * E939 SHARES AMT YTD NEGATIVE
      * E050 SHARES AMT YTD OVER 5000
      * E665 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4250-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-ADDR = SPACES
                   MOVE 'E885' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-YTD < 0
                   MOVE 'E939' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-YTD > 5000.00
                   MOVE 'E050' TO WS-REJECT-CODE
               WHEN DRPT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E665' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES YTD FEE ON THE CASH PAY AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4260-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPM-SHARES-AMT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-STOP-FLAG = 'Y'
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                       DRPM-CASH-PAY-AMT * DRPM-DISCOUNT / 200
               WHEN DRPT-TRAN-CD = 'DV'
                   MOVE 15.00 TO DRPM-SHARES-AMT-YTD
               WHEN DRPT-TRAN-CD = 'RI'
                   MOVE 7.00 TO DRPM-SHARES-AMT-YTD
               WHEN OTHER
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                       DRPM-CASH-PAY-AMT * DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-SHARES-AMT-YTD > 0 AND DRPM-SHARES-AMT-YTD < 2.00
               MOVE 2.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           IF DRPM-SHARES-AMT-YTD > 100.00
               MOVE 100.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           ADD DRPM-SHARES-AMT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E467 OPERATOR REQUIRED
      * E441 SHARES AMT YTD NEGATIVE
      * E467 SHARES AMT YTD OVER 50000
      * E565 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4270-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-OPERATOR = SPACES
                   MOVE 'E467' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-YTD < 0
                   MOVE 'E441' TO WS-REJECT-CODE
               WHEN DRPM-SHARES-AMT-YTD > 50000.00
                   MOVE 'E467' TO WS-REJECT-CODE
               WHEN DRPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E565' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4280-LIMIT-REINVEST.
           IF DRPM-REINVEST-AMT < 100.00
               MOVE 100.00 TO DRPM-REINVEST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-REINVEST-AMT > 50000.00
               MOVE 50000.00 TO DRPM-REINVEST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN OPTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4290-DESCRIBE-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E704 ADDR REQUIRED
      * E558 WORK AMT 2 NEGATIVE
      * E530 WORK AMT 2 OVER 5000
      * E645 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4300-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-ADDR = SPACES
                   MOVE 'E704' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E558' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E530' TO WS-REJECT-CODE
               WHEN DRPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E645' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH PAY YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4310-ACCRUE-CASH-PAY.
           IF DRPM-CASH-PAY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-CASH-PAY-YTD * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4320-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE SHARES AMT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4330-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF DRPT-SHARES-AMT-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REINVEST AMT.
      *----------------------------------------------------------------*
       4340-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPT-REINVEST-AMT ROUNDED =
                   DRPT-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPT-REINVEST-AMT
           END-IF
           ADD DRPT-REINVEST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4350-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN OPTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4360-DESCRIBE-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4370-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-YTD - (WS-WORK-AMT *
               4)
           COMPUTE DRPM-REINVEST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH PAY FEE ON THE FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4380-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN DRPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO DRPM-CASH-PAY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   COMPUTE DRPM-CASH-PAY-AMT ROUNDED = DRPM-FEE-YTD *
                       DRPM-PENALTY-PCT / 200
               WHEN DRPM-REASON-CD = 'B'
                   MOVE 5.00 TO DRPM-CASH-PAY-AMT
               WHEN DRPM-REASON-CD = 'G'
                   MOVE 12.00 TO DRPM-CASH-PAY-AMT
               WHEN OTHER
                   COMPUTE DRPM-CASH-PAY-AMT ROUNDED = DRPM-FEE-YTD *
                       DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-CASH-PAY-AMT > 0 AND DRPM-CASH-PAY-AMT < 2.00
               MOVE 2.00 TO DRPM-CASH-PAY-AMT
           END-IF
           IF DRPM-CASH-PAY-AMT > 100.00
               MOVE 100.00 TO DRPM-CASH-PAY-AMT
           END-IF
           ADD DRPM-CASH-PAY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E163 OPERATOR REQUIRED
      * E596 REINVEST YTD NEGATIVE
      * E980 REINVEST YTD OVER 10000
      * E369 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4390-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-OPERATOR = SPACES
                   MOVE 'E163' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-YTD < 0
                   MOVE 'E596' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-YTD > 10000.00
                   MOVE 'E980' TO WS-REJECT-CODE
               WHEN DRPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E369' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4400-ACCRUE-SHARES-AMT.
           IF DRPT-SHARES-AMT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPT-SHARES-AMT-AMT * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-DIVIDEND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH PAY TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4410-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-PAY-TIER-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPM-FEE-AMT NOT < WS-CASH-PAY-TIER-LOW(WS-TX)
                   MOVE WS-CASH-PAY-TIER-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4420-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 12
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4430-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CYCLE-CD
                   ADD DRPT-DIVIDEND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-DIVIDEND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4440-ACCRUE-SHARES-AMT.
           IF DRPT-SHARES-AMT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPT-SHARES-AMT-AMT * DRPM-RATE / 100 / 365
               ADD WS-WORK-AMT TO DRPM-REINVEST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4450-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-REVIEW-DATE > 0 AND DRPM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPT-REINVEST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD DRPT-REINVEST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRPT-REINVEST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPT-REINVEST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPT-REINVEST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REINVEST FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4460-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN DRPM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO DRPM-REINVEST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   COMPUTE DRPM-REINVEST-AMT ROUNDED = WS-WORK-AMT *
                       DRPM-DISCOUNT / 200
               WHEN DRPM-HOLDER-TYPE-CD = 'C'
                   MOVE 20.00 TO DRPM-REINVEST-AMT
               WHEN DRPM-HOLDER-TYPE-CD = 'H'
                   MOVE 20.00 TO DRPM-REINVEST-AMT
               WHEN OTHER
                   COMPUTE DRPM-REINVEST-AMT ROUNDED = WS-WORK-AMT *
                       DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-REINVEST-AMT > 0 AND DRPM-REINVEST-AMT < 1.00
               MOVE 1.00 TO DRPM-REINVEST-AMT
           END-IF
           IF DRPM-REINVEST-AMT > 50.00
               MOVE 50.00 TO DRPM-REINVEST-AMT
           END-IF
           ADD DRPM-REINVEST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN OPTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4470-DESCRIBE-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'T'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       4480-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-FEE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-FEE-YTD
           END-IF
           ADD DRPM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES YTD FEE ON THE REINVEST AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4490-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPM-SHARES-AMT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                       DRPT-REINVEST-AMT * DRPM-DISCOUNT / 200
               WHEN DRPM-REASON-CD = 'B'
                   MOVE 12.00 TO DRPM-SHARES-AMT-YTD
               WHEN DRPM-REASON-CD = 'G'
                   MOVE 10.00 TO DRPM-SHARES-AMT-YTD
               WHEN OTHER
                   COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                       DRPT-REINVEST-AMT * DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-SHARES-AMT-YTD > 0 AND DRPM-SHARES-AMT-YTD < 1.00
               MOVE 1.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           IF DRPM-SHARES-AMT-YTD > 100.00
               MOVE 100.00 TO DRPM-SHARES-AMT-YTD
           END-IF
           ADD DRPM-SHARES-AMT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4500-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4510-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPT-SHARES-AMT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = DRPT-SHARES-AMT-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHARES AMT AMT.
      *----------------------------------------------------------------*
       4520-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-SHARES-AMT-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-SHARES-AMT-AMT
           END-IF
           ADD DRPM-SHARES-AMT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4530-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE DRP FIELDS TO DRSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-DRSTAT.
           INITIALIZE DRSTAT-PARMS
           MOVE DRPM-CASH-PAY-AMT
             TO DRSTAT-CASH-PAY-AMT
           MOVE DRPM-FEE-YTD
             TO DRSTAT-FEE-YTD
           MOVE DRPM-DIVIDEND-AMT
             TO DRSTAT-DIVIDEND-AMT
           MOVE DRPM-REINVEST-AMT
             TO DRSTAT-REINVEST-AMT
           MOVE DRPM-SHARES-AMT-AMT
             TO DRSTAT-SHARES-AMT-AMT
           MOVE DRPM-WITHHOLD-YTD
             TO DRSTAT-WITHHOLD-YTD
           MOVE DRPM-LAST-PAY-DATE
             TO DRSTAT-LAST-PAY-DATE
           MOVE DRPM-NEXT-REVIEW-DATE
             TO DRSTAT-NEXT-REVIEW-DATE
           MOVE DRPM-NEXT-DUE-DATE
             TO DRSTAT-NEXT-DUE-DATE
           MOVE DRPM-MATURITY-DATE
             TO DRSTAT-MATURITY-DATE
           MOVE DRPM-PLAN-OPTION-CD
             TO DRSTAT-PLAN-OPTION-CD
           MOVE DRPM-REASON-CD
             TO DRSTAT-REASON-CD
           MOVE DRPM-CHANNEL-CD
             TO DRSTAT-CHANNEL-CD
           MOVE DRPM-HOLDER-TYPE-CD
             TO DRSTAT-HOLDER-TYPE-CD
           MOVE DRPM-OVERRIDE-FLAG
             TO DRSTAT-OVERRIDE-FLAG
           MOVE DRPM-STOP-FLAG
             TO DRSTAT-STOP-FLAG
           MOVE DRPM-AUTO-RENEW-FLAG
             TO DRSTAT-AUTO-RENEW-FLAG
           MOVE DRPM-EXCEPT-CNT
             TO DRSTAT-EXCEPT-CNT
           MOVE DRPM-NOTICE-CNT
             TO DRSTAT-NOTICE-CNT
           MOVE DRPM-RATE
             TO DRSTAT-RATE
           MOVE DRPM-DISCOUNT
             TO DRSTAT-DISCOUNT
           MOVE DRPM-CUST-ID
             TO DRSTAT-CUST-ID
           MOVE DRPM-REF-NO
             TO DRSTAT-REF-NO
           CALL 'DRSTAT' USING DRSTAT-PARMS
           EVALUATE DRSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE DRSTAT-DIVIDEND-AMT
                     TO DRPM-DIVIDEND-AMT
                   MOVE DRSTAT-OVERRIDE-FLAG
                     TO DRPM-OVERRIDE-FLAG
                   MOVE DRSTAT-STOP-FLAG
                     TO DRPM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-DRSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE DRPT-KEY TO RD-KEY
           MOVE DRPT-TRAN-CD TO RD-CODE
           MOVE DRPT-FEE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE DRPT-KEY TO RD-KEY
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
           DISPLAY 'DRPXMIT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'DRPXMIT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
