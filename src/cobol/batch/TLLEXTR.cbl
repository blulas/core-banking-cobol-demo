      *================================================================*
      * PROGRAM  : TLLEXTR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TELLER BALANCING - NIGHTLY EXTRACT. READS THE TBL   *
      *            TRANSACTIONS, UPDATES THE TBL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TELLER CASH DRAWER BALANCING,   *
      *            BATCH PROOF AND OVER/SHORT                          *
      * INPUT    : TBLMST (KSDS), TBLTRN, BANKCTL                      *
      * OUTPUT   : TBLOUT, TLLEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TLPEN, TLFEE, TLWAIV    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-08-05 GAF  ORIGINAL                                       *
      * 1995-11-25 LMB  AUDIT FINDING - CASH-OUT CONTROL TOTALS REQ6919*
      * 2000-07-24 GAF  BATCH CUTOFF MOVED REQ7948                     *
      * 2007-04-19 PQR  NEW OVERSHORT REPORT COLUMNS PRB2672           *
      * 2011-10-05 CRP  VAULT-BUY LIMIT REVISED INC9133                *
      * 2012-02-11 SKN  BATCH FEE SCHEDULE UPDATE INC7709              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLLEXTR.
       AUTHOR. M K LOWE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TBLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TBLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TBLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TBLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TLLEXRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TBLMST.
       FD  TRAN-FILE.
       COPY TBLTRN.
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
      * WS-OVERSHOR-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-OVERSHOR-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000244000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000808500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000664500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010300000000571500'.
       01  WS-OVERSHOR-SCHED-TBL REDEFINES WS-OVERSHOR-SCHED-VALUES.
           05  WS-OVERSHOR-SCHED-ROW OCCURS 4 TIMES.
               10  WS-OVERSHOR-SCHED-LOW   PIC 9(09)V99.
               10  WS-OVERSHOR-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VAULT-SE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VAULT-SE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000061000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000574500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000427000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000482000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023000000000108000'.
       01  WS-VAULT-SE-GRID-TBL REDEFINES WS-VAULT-SE-GRID-VALUES.
           05  WS-VAULT-SE-GRID-ROW OCCURS 5 TIMES.
               10  WS-VAULT-SE-GRID-LOW    PIC 9(09)V99.
               10  WS-VAULT-SE-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TLLEXTR  TELLER BALANCING'.
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
       COPY TLPENP.
       COPY TLFEEP.
       COPY TLWAIVP.

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
               MOVE 'TBLMST' TO ABND-FILE-NAME
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
           MOVE TBLT-KEY TO TBLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-DESCRIBE-REASON
               PERFORM 3010-SPLIT-BATCH
               PERFORM 3020-COUNT-EXCEPT
               PERFORM 3030-AGE-OPEN
               PERFORM 3040-COUNT-EXCEPT
               PERFORM 3050-SET-DRAWER
               PERFORM 3060-AGE-EXPIRY
               PERFORM 3070-CHECK-LAST-ACT
               PERFORM 3080-PRORATE-CASH-IN
               PERFORM 3090-PRORATE-VAULT-SELL
               PERFORM 3100-SELECT-FEE-PCT
               PERFORM 3110-ACCRUE-OVERSHORT
               PERFORM 3120-DESCRIBE-TRAN
               PERFORM 3130-CHECK-EFFECTIVE
               PERFORM 3140-DESCRIBE-PROOF-TYPE
               PERFORM 3150-SPLIT-BATCH
               PERFORM 3160-COUNT-RETRY
               PERFORM 3170-LIMIT-VAULT-SELL
               PERFORM 3180-ACCUM-DRAWER
               PERFORM 3190-CHECK-LAST-PAY
               PERFORM 3200-ACCUM-PROOF-TYPE
               PERFORM 3210-AGE-OPEN
               PERFORM 3220-ASSESS-HOLD
               PERFORM 3230-SELECT-RATE
               PERFORM 3240-CHECK-EXPIRY
               PERFORM 3250-ACCUM-DRAWER
               PERFORM 3260-COUNT-RETRY
               PERFORM 3270-ASSESS-HOLD
               PERFORM 3280-CHECK-NEXT-REVIEW
               PERFORM 3290-COUNT-EXCEPT
               PERFORM 3300-COUNT-NOTICE
               PERFORM 3310-ASSESS-REVIEW
               PERFORM 3320-COUNT-EXCEPT
               PERFORM 3330-ASSESS-SUSPEND
               PERFORM 3340-SPLIT-VAULT-SELL
               PERFORM 3350-DESCRIBE-CHANNEL
               PERFORM 3360-AGE-LAST-PAY
               PERFORM 3370-EDIT-REJECT
               PERFORM 3380-LIMIT-ITEM
               PERFORM 3390-AGE-NOTICE
               PERFORM 3400-FLAG-BATCH
               PERFORM 3410-SELECT-RATE
               PERFORM 3420-LIMIT-CASH-IN
               PERFORM 3430-CHECK-LAST-MAINT
               PERFORM 3440-SELECT-RATE
               PERFORM 3450-DESCRIBE-CHANNEL
               PERFORM 3460-FLAG-OVERSHORT
               PERFORM 3470-PRORATE-OVERSHORT
               PERFORM 3480-SET-REASON
               PERFORM 3490-ACCRUE-FEE-PCT
               PERFORM 3500-SPLIT-BATCH
               PERFORM 3510-ASSESS-REVERSAL
               PERFORM 3520-DESCRIBE-CHANNEL
               PERFORM 3530-CHECK-NEXT-REVIEW
               PERFORM 3540-SPLIT-VAULT-SELL
               PERFORM 3550-SPLIT-BATCH
               PERFORM 3560-ACCRUE-OVERSHORT
               PERFORM 3570-FLAG-CASH-OUT
               PERFORM 3580-ASSESS-EXEMPT
               PERFORM 3590-SELECT-RATE
               PERFORM 3600-COUNT-SEQ
               PERFORM 3610-ACCRUE-CASH-IN
               PERFORM 3620-CHECK-EXPIRY
               PERFORM 3630-ASSESS-STOP
               PERFORM 3640-ACCUM-REASON
               PERFORM 3650-EDIT-REF-NO
               PERFORM 3660-FLAG-BATCH
               PERFORM 3670-EDIT-ADDR
               PERFORM 3680-ACCRUE-VAULT-SELL
               PERFORM 3690-FLAG-CASH-IN
               PERFORM 3700-SPLIT-BATCH
               PERFORM 3710-LIMIT-OVERSHORT
               PERFORM 3720-ACCRUE-BATCH
               PERFORM 3730-SPLIT-VAULT-SELL
               PERFORM 3740-SELECT-RATE
               PERFORM 3750-AGE-NEXT-REVIEW
               PERFORM 3760-PRORATE-VAULT-BUY
               PERFORM 3770-ACCUM-CHANNEL
               PERFORM 3780-DESCRIBE-CYCLE
               PERFORM 3790-ACCUM-CHANNEL
               PERFORM 3800-DESCRIBE-TRAN
               PERFORM 3810-COUNT-NOTICE
               PERFORM 3820-FLAG-CASH-IN
               PERFORM 3830-SELECT-FEE-PCT
               PERFORM 3840-AGE-LAST-ACT
               PERFORM 3850-FLAG-OVERSHORT
               PERFORM 3860-PRORATE-BATCH
               PERFORM 3870-ASSESS-STOP
               PERFORM 3880-PRORATE-CASH-OUT
               PERFORM 3890-SPLIT-BATCH
               PERFORM 3900-PRORATE-BATCH
               PERFORM 3910-EDIT-MEMO
               PERFORM 3920-EDIT-CUST-ID
               PERFORM 3930-AGE-LAST-PAY
               PERFORM 3940-SPLIT-BATCH
               PERFORM 3950-LIMIT-BATCH
               PERFORM 3960-SPLIT-CASH-OUT
               PERFORM 3970-ACCUM-REASON
               PERFORM 3980-LIMIT-BATCH
               PERFORM 3990-SELECT-DISCOUNT
               PERFORM 4000-COUNT-ITEM
               PERFORM 4010-ACCUM-CHANNEL
               PERFORM 4020-PRORATE-CASH-OUT
               PERFORM 4030-COUNT-RETRY
               PERFORM 4040-ASSESS-HOLD
               PERFORM 4050-PRORATE-CASH-OUT
               PERFORM 4060-PRORATE-CASH-IN
               PERFORM 4070-ACCUM-PROOF-TYPE
               PERFORM 4080-LIMIT-CASH-IN
               PERFORM 4090-FLAG-BATCH
               PERFORM 4100-CHECK-EFF
               PERFORM 4110-COUNT-RETRY
               PERFORM 4120-EDIT-ADDR
               PERFORM 4130-CHECK-POST
               PERFORM 4140-ACCRUE-BATCH
               PERFORM 4150-PRORATE-VAULT-SELL
               PERFORM 4160-EDIT-ADDR
               PERFORM 4170-SPLIT-VAULT-BUY
               PERFORM 4180-COUNT-EXCEPT
               PERFORM 4190-LIMIT-OVERSHORT
               PERFORM 4200-ACCUM-CYCLE
               PERFORM 4210-SELECT-FEE-PCT
               PERFORM 4220-AGE-LAST-STMT
               PERFORM 4230-PRORATE-BATCH
               PERFORM 4240-EDIT-OPERATOR
               PERFORM 4250-FLAG-ITEM
               PERFORM 4260-PRORATE-OVERSHORT
               PERFORM 4270-ACCUM-REASON
               PERFORM 4280-SPLIT-OVERSHORT
               PERFORM 4290-ASSESS-SUSPEND
               PERFORM 4300-FLAG-BATCH
               PERFORM 4310-AGE-OPEN
               PERFORM 4320-ASSESS-STOP
               PERFORM 4330-LIMIT-CASH-IN
               PERFORM 4340-COUNT-ITEM
               PERFORM 4350-EDIT-CONTACT
               PERFORM 4360-CHECK-POST
               PERFORM 4370-COUNT-NOTICE
               PERFORM 4380-EDIT-ADDR
               PERFORM 4390-COUNT-NOTICE
               PERFORM 4400-SPLIT-VAULT-BUY
               PERFORM 4410-SPLIT-CASH-OUT
               PERFORM 4420-LIMIT-CASH-OUT
               PERFORM 4430-AGE-OPEN
               PERFORM 4440-ASSESS-SUSPEND
               PERFORM 4450-CHECK-EFFECTIVE
               PERFORM 4460-SPLIT-OVERSHORT
               PERFORM 4470-ASSESS-REVERSAL
               PERFORM 4480-LIMIT-OVERSHORT
               PERFORM 4490-CHECK-EFFECTIVE
               PERFORM 4500-ACCRUE-CASH-IN
               PERFORM 4510-SELECT-FEE-PCT
               PERFORM 4520-ACCRUE-DISCOUNT
               PERFORM 4530-ACCUM-PROOF-TYPE
               PERFORM 4540-PRORATE-VAULT-SELL
               PERFORM 4550-SELECT-DISCOUNT
               PERFORM 4560-EDIT-DESC
               PERFORM 4570-SET-CHANNEL
               PERFORM 4580-SPLIT-OVERSHORT
               PERFORM 4590-DESCRIBE-TRAN
               PERFORM 4600-FLAG-VAULT-BUY
               PERFORM 4610-SET-CYCLE
               PERFORM 4620-DESCRIBE-CHANNEL
               PERFORM 4630-COUNT-NOTICE
               PERFORM 4640-DESCRIBE-DRAWER
               PERFORM 4650-AGE-LAST-ACT
               PERFORM 4660-ACCRUE-DISCOUNT
               PERFORM 4670-SET-DRAWER
               PERFORM 4680-EDIT-REJECT
               PERFORM 4690-CHECK-LAST-ACT
               PERFORM 4700-ASSESS-STOP
               PERFORM 4710-CHECK-EFFECTIVE
               PERFORM 4720-SPLIT-CASH-OUT
               PERFORM 4730-SELECT-DISCOUNT
               PERFORM 4740-EDIT-CONTACT
               PERFORM 4750-AGE-EFF
               PERFORM 4760-SELECT-RATE
               PERFORM 4770-AGE-LAST-ACT
               PERFORM 4780-FLAG-VAULT-SELL
               PERFORM 4790-CHECK-NOTICE
               PERFORM 4800-SELECT-DISCOUNT
               PERFORM 4810-AGE-EXPIRY
               PERFORM 4820-SET-TRAN
               PERFORM 7040-CALL-TLPEN
               PERFORM 7000-CALL-TLFEE
               PERFORM 7020-CALL-TLWAIV
               REWRITE TBL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TBLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TBL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3010-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE TBLT-BATCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TBLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAWER CD:
      * D -> F WHEN LAST STMT DATE IS PAST
      * F -> R WHEN LAST STMT DATE IS PAST
      * R -> D WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-DRAWER.
           EVALUATE TBLM-DRAWER-CD
               WHEN 'D'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TBLM-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TBLM-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TBLM-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3060-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-CASH-OUT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-CASH-OUT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-CASH-OUT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-CASH-OUT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-OUT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TBLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH OUT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH IN YTD.
      *----------------------------------------------------------------*
       3080-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-IN-YTD ROUNDED =
                   TBLT-CASH-OUT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-CASH-IN-YTD
           END-IF
           ADD TBLM-CASH-IN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT SELL AMT.
      *----------------------------------------------------------------*
       3090-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-VAULT-SELL-AMT ROUNDED =
                   TBLM-VAULT-SELL-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLT-VAULT-SELL-AMT
           END-IF
           ADD TBLT-VAULT-SELL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE GRID TIER FOR THE BATCH YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3100-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-GRID-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TBLM-BATCH-YTD NOT < WS-VAULT-SE-GRID-LOW(WS-TX)
                   MOVE WS-VAULT-SE-GRID-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3110-ACCRUE-OVERSHORT.
           IF TBLT-OVERSHORT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-OVERSHORT-AMT * TBLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TBLM-VAULT-SELL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3120-DESCRIBE-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BO'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'OS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TBLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PROOF TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-PROOF-TYPE.
           EVALUATE TBLM-PROOF-TYPE-CD
               WHEN 'JG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BATCH AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3150-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLT-BATCH-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TBLT-BATCH-AMT - (WS-WORK-AMT * 3)
           COMPUTE TBLM-VAULT-BUY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3160-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO TBLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VAULT SELL AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3170-LIMIT-VAULT-SELL.
           IF TBLT-VAULT-SELL-AMT < 100.00
               MOVE 100.00 TO TBLT-VAULT-SELL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-VAULT-SELL-AMT > 2500.00
               MOVE 2500.00 TO TBLT-VAULT-SELL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-DRAWER-CD
                   ADD TBLT-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TBLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT YTD BY PROOF TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-PROOF-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-PROOF-TYPE-CD
                   ADD TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-PROOF-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3210-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLT-OVERSHORT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLT-OVERSHORT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TBLT-OVERSHORT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLT-OVERSHORT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLT-OVERSHORT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT BUY FEE ON THE CASH OUT YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3220-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TBLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO TBLM-VAULT-BUY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   COMPUTE TBLM-VAULT-BUY-AMT ROUNDED =
                       TBLM-CASH-OUT-YTD * TBLM-RATE / 200
               WHEN TBLM-REASON-CD = 'GS'
                   MOVE 7.00 TO TBLM-VAULT-BUY-AMT
               WHEN TBLM-REASON-CD = 'GV'
                   MOVE 12.00 TO TBLM-VAULT-BUY-AMT
               WHEN OTHER
                   COMPUTE TBLM-VAULT-BUY-AMT ROUNDED =
                       TBLM-CASH-OUT-YTD * TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-VAULT-BUY-AMT > 0 AND TBLM-VAULT-BUY-AMT < 5.00
               MOVE 5.00 TO TBLM-VAULT-BUY-AMT
           END-IF
           IF TBLM-VAULT-BUY-AMT > 150.00
               MOVE 150.00 TO TBLM-VAULT-BUY-AMT
           END-IF
           ADD TBLM-VAULT-BUY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE OVERSHOR SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3230-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-OVERSHOR-SCHED-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-OVERSHOR-SCHED-LOW(WS-TX)
                   MOVE WS-OVERSHOR-SCHED-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TBLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH IN YTD BY DRAWER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3250-ACCUM-DRAWER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-DRAWER-CD
                   ADD TBLM-CASH-IN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-DRAWER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-CASH-IN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3260-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 10
               MOVE 'Y' TO TBLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH OUT FEE ON THE CASH OUT YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3270-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TBLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-OUT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLM-CASH-OUT-AMT ROUNDED = TBLM-CASH-OUT-YTD
                       * TBLM-FEE-PCT / 200
               WHEN TBLM-REASON-CD = 'GS'
                   MOVE 5.00 TO TBLM-CASH-OUT-AMT
               WHEN TBLM-REASON-CD = 'GV'
                   MOVE 15.00 TO TBLM-CASH-OUT-AMT
               WHEN OTHER
                   COMPUTE TBLM-CASH-OUT-AMT ROUNDED = TBLM-CASH-OUT-YTD
                       * TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLM-CASH-OUT-AMT > 0 AND TBLM-CASH-OUT-AMT < 10.00
               MOVE 10.00 TO TBLM-CASH-OUT-AMT
           END-IF
           IF TBLM-CASH-OUT-AMT > 75.00
               MOVE 75.00 TO TBLM-CASH-OUT-AMT
           END-IF
           ADD TBLM-CASH-OUT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3280-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TBLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TBLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-NOTICE.
           IF TBLM-NOTICE-CNT < 9999999
               ADD 1 TO TBLM-NOTICE-CNT
           END-IF
           IF TBLM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT SELL YTD FEE ON THE VAULT SELL AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3310-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TBLM-VAULT-SELL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TBLM-VAULT-SELL-YTD ROUNDED =
                       TBLT-VAULT-SELL-AMT * TBLM-DISCOUNT / 200
               WHEN TBLM-CYCLE-CD = 'KX'
                   MOVE 10.00 TO TBLM-VAULT-SELL-YTD
               WHEN TBLM-CYCLE-CD = 'TA'
                   MOVE 3.00 TO TBLM-VAULT-SELL-YTD
               WHEN OTHER
                   COMPUTE TBLM-VAULT-SELL-YTD ROUNDED =
                       TBLT-VAULT-SELL-AMT * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-VAULT-SELL-YTD > 0 AND TBLM-VAULT-SELL-YTD < 10.00
               MOVE 10.00 TO TBLM-VAULT-SELL-YTD
           END-IF
           IF TBLM-VAULT-SELL-YTD > 25.00
               MOVE 25.00 TO TBLM-VAULT-SELL-YTD
           END-IF
           ADD TBLM-VAULT-SELL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3320-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO TBLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH FEE ON THE CASH OUT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3330-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TBLT-BATCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-HOLD-FLAG = 'Y'
                   COMPUTE TBLT-BATCH-AMT ROUNDED = TBLM-CASH-OUT-AMT *
                       TBLM-RATE / 200
               WHEN TBLT-TRAN-CD = 'BI'
                   MOVE 15.00 TO TBLT-BATCH-AMT
               WHEN TBLT-TRAN-CD = 'BO'
                   MOVE 12.00 TO TBLT-BATCH-AMT
               WHEN OTHER
                   COMPUTE TBLT-BATCH-AMT ROUNDED = TBLM-CASH-OUT-AMT *
                       TBLM-RATE / 100
           END-EVALUATE
           IF TBLT-BATCH-AMT > 0 AND TBLT-BATCH-AMT < 1.00
               MOVE 1.00 TO TBLT-BATCH-AMT
           END-IF
           IF TBLT-BATCH-AMT > 35.00
               MOVE 35.00 TO TBLT-BATCH-AMT
           END-IF
           ADD TBLT-BATCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3340-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLT-VAULT-SELL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TBLT-VAULT-SELL-AMT - (WS-WORK-AMT *
               2)
           COMPUTE TBLT-BATCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3350-DESCRIBE-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3360-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E938 REJECT CD REQUIRED
      * E939 CASH IN AMT NEGATIVE
      * E751 CASH IN AMT OVER 5000
      * E814 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3370-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-REJECT-CD = SPACES
                   MOVE 'E938' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT < 0
                   MOVE 'E939' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT > 5000.00
                   MOVE 'E751' TO WS-REJECT-CODE
               WHEN TBLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E814' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3380-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3390-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BATCH YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3400-FLAG-BATCH.
           IF TBLM-BATCH-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLM-BATCH-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OVERSHOR SCHED TIER FOR THE VAULT BUY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3410-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-OVERSHOR-SCHED-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TBLM-VAULT-BUY-AMT NOT < WS-OVERSHOR-SCHED-LOW(WS-TX)
                   MOVE WS-OVERSHOR-SCHED-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3420-LIMIT-CASH-IN.
           IF TBLM-CASH-IN-YTD < 25.00
               MOVE 25.00 TO TBLM-CASH-IN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-IN-YTD > 1000.00
               MOVE 1000.00 TO TBLM-CASH-IN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3430-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE OVERSHOR SCHED TIER FOR THE CASH OUT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3440-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-OVERSHOR-SCHED-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TBLM-CASH-OUT-AMT NOT < WS-OVERSHOR-SCHED-LOW(WS-TX)
                   MOVE WS-OVERSHOR-SCHED-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3450-DESCRIBE-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OVERSHORT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3460-FLAG-OVERSHORT.
           IF TBLM-OVERSHORT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLM-OVERSHORT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OVERSHORT AMT.
      *----------------------------------------------------------------*
       3470-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-OVERSHORT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLT-OVERSHORT-AMT
           END-IF
           ADD TBLT-OVERSHORT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GS -> GV WHEN LAST PAY DATE IS PAST
      * GV -> PG WHEN LAST PAY DATE IS PAST
      * PG -> SX WHEN LAST PAY DATE IS PAST
      * SX -> XN WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PG' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SX' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SX'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XN' TO TBLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TBLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BATCH AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3500-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLM-BATCH-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TBLM-BATCH-AMT - (WS-WORK-AMT * 2)
           COMPUTE TBLM-CASH-IN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT FEE ON THE BATCH AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3510-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TBLM-OVERSHORT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TBLM-OVERSHORT-AMT ROUNDED = TBLM-BATCH-AMT *
                       TBLM-FEE-PCT / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 15.00 TO TBLM-OVERSHORT-AMT
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 7.00 TO TBLM-OVERSHORT-AMT
               WHEN OTHER
                   COMPUTE TBLM-OVERSHORT-AMT ROUNDED = TBLM-BATCH-AMT *
                       TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLM-OVERSHORT-AMT > 0 AND TBLM-OVERSHORT-AMT < 2.00
               MOVE 2.00 TO TBLM-OVERSHORT-AMT
           END-IF
           IF TBLM-OVERSHORT-AMT > 150.00
               MOVE 150.00 TO TBLM-OVERSHORT-AMT
           END-IF
           ADD TBLM-OVERSHORT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'R'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3540-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-SELL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-SELL-AMT - (WS-WORK-AMT *
               3)
           COMPUTE TBLM-CASH-OUT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BATCH AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3550-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLT-BATCH-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TBLT-BATCH-AMT - (WS-WORK-AMT * 4)
           COMPUTE TBLM-BATCH-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-OVERSHORT.
           IF TBLT-OVERSHORT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-OVERSHORT-AMT * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TBLM-OVERSHORT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3570-FLAG-CASH-OUT.
           IF TBLM-CASH-OUT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-OUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3580-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLM-BATCH-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLM-BATCH-YTD ROUNDED = WS-WORK-AMT *
                       TBLM-DISCOUNT / 200
               WHEN TBLT-TRAN-CD = 'BI'
                   MOVE 15.00 TO TBLM-BATCH-YTD
               WHEN TBLT-TRAN-CD = 'BO'
                   MOVE 10.00 TO TBLM-BATCH-YTD
               WHEN OTHER
                   COMPUTE TBLM-BATCH-YTD ROUNDED = WS-WORK-AMT *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-BATCH-YTD > 0 AND TBLM-BATCH-YTD < 1.00
               MOVE 1.00 TO TBLM-BATCH-YTD
           END-IF
           IF TBLM-BATCH-YTD > 35.00
               MOVE 35.00 TO TBLM-BATCH-YTD
           END-IF
           ADD TBLM-BATCH-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE OVERSHOR SCHED TIER FOR THE CASH IN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-OVERSHOR-SCHED-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TBLM-CASH-IN-AMT NOT < WS-OVERSHOR-SCHED-LOW(WS-TX)
                   MOVE WS-OVERSHOR-SCHED-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-SEQ.
           IF TBLT-SEQ < 9999999
               ADD 1 TO TBLT-SEQ
           END-IF
           IF TBLT-SEQ NOT < 12
               MOVE 'Y' TO TBLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3610-ACCRUE-CASH-IN.
           IF TBLM-CASH-IN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-IN-AMT * TBLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TBLM-CASH-IN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TBLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT FEE ON THE CASH OUT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3630-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TBLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TBLT-OVERSHORT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLT-OVERSHORT-AMT ROUNDED =
                       TBLT-CASH-OUT-AMT * TBLM-RATE / 200
               WHEN TBLT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO TBLT-OVERSHORT-AMT
               WHEN TBLT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO TBLT-OVERSHORT-AMT
               WHEN OTHER
                   COMPUTE TBLT-OVERSHORT-AMT ROUNDED =
                       TBLT-CASH-OUT-AMT * TBLM-RATE / 100
           END-EVALUATE
           IF TBLT-OVERSHORT-AMT > 0 AND TBLT-OVERSHORT-AMT < 2.00
               MOVE 2.00 TO TBLT-OVERSHORT-AMT
           END-IF
           IF TBLT-OVERSHORT-AMT > 35.00
               MOVE 35.00 TO TBLT-OVERSHORT-AMT
           END-IF
           ADD TBLT-OVERSHORT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE BATCH AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3640-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-REASON-CD
                   ADD TBLM-BATCH-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-BATCH-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E088 REF NO REQUIRED
      * E397 CASH OUT AMT NEGATIVE
      * E358 CASH OUT AMT OVER 250000
      * E863 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-REF-NO = SPACES
                   MOVE 'E088' TO WS-REJECT-CODE
               WHEN TBLT-CASH-OUT-AMT < 0
                   MOVE 'E397' TO WS-REJECT-CODE
               WHEN TBLT-CASH-OUT-AMT > 250000.00
                   MOVE 'E358' TO WS-REJECT-CODE
               WHEN TBLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E863' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BATCH AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3660-FLAG-BATCH.
           IF TBLT-BATCH-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLT-BATCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E866 ADDR REQUIRED
      * E796 OVERSHORT AMT NEGATIVE
      * E218 OVERSHORT AMT OVER 99999
      * E062 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3670-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-ADDR = SPACES
                   MOVE 'E866' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-AMT < 0
                   MOVE 'E796' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-AMT > 99999.00
                   MOVE 'E218' TO WS-REJECT-CODE
               WHEN TBLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E062' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3680-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-YTD * TBLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-CASH-IN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3690-FLAG-CASH-IN.
           IF TBLM-CASH-IN-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-IN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BATCH AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3700-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLT-BATCH-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TBLT-BATCH-AMT - (WS-WORK-AMT * 2)
           COMPUTE TBLM-OVERSHORT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3710-LIMIT-OVERSHORT.
           IF TBLM-OVERSHORT-YTD < 100.00
               MOVE 100.00 TO TBLM-OVERSHORT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-OVERSHORT-YTD > 10000.00
               MOVE 10000.00 TO TBLM-OVERSHORT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BATCH AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3720-ACCRUE-BATCH.
           IF TBLM-BATCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-BATCH-AMT * TBLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TBLM-OVERSHORT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3730-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-SELL-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-SELL-AMT - (WS-WORK-AMT *
               12)
           COMPUTE TBLM-CASH-OUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE GRID TIER FOR THE VAULT SELL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-GRID-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TBLT-VAULT-SELL-AMT NOT < WS-VAULT-SE-GRID-LOW(WS-TX)
                   MOVE WS-VAULT-SE-GRID-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NEXT-REVIEW-DATE > 0 AND TBLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-SELL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT BUY YTD.
      *----------------------------------------------------------------*
       3760-PRORATE-VAULT-BUY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-BUY-YTD ROUNDED =
                   TBLM-OVERSHORT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-VAULT-BUY-YTD
           END-IF
           ADD TBLM-VAULT-BUY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT SELL YTD BY CHANNEL CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3770-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLT-CHANNEL-CD
                   ADD TBLM-VAULT-SELL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-VAULT-SELL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3780-DESCRIBE-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE BATCH AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3790-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLT-CHANNEL-CD
                   ADD TBLM-BATCH-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-BATCH-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3800-DESCRIBE-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BO'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'OS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3810-COUNT-NOTICE.
           IF TBLM-NOTICE-CNT < 9999999
               ADD 1 TO TBLM-NOTICE-CNT
           END-IF
           IF TBLM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3820-FLAG-CASH-IN.
           IF TBLM-CASH-IN-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-IN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE GRID TIER FOR THE CASH IN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3830-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-GRID-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TBLM-CASH-IN-AMT NOT < WS-VAULT-SE-GRID-LOW(WS-TX)
                   MOVE WS-VAULT-SE-GRID-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3840-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-BUY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * OVERSHORT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3850-FLAG-OVERSHORT.
           IF TBLT-OVERSHORT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLT-OVERSHORT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH IN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BATCH AMT.
      *----------------------------------------------------------------*
       3860-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-BATCH-AMT ROUNDED =
                   TBLM-CASH-IN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-BATCH-AMT
           END-IF
           ADD TBLM-BATCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT BUY YTD FEE ON THE VAULT SELL AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3870-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TBLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TBLM-VAULT-BUY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   COMPUTE TBLM-VAULT-BUY-YTD ROUNDED =
                       TBLT-VAULT-SELL-AMT * TBLM-RATE / 200
               WHEN TBLM-CYCLE-CD = 'KX'
                   MOVE 3.00 TO TBLM-VAULT-BUY-YTD
               WHEN TBLM-CYCLE-CD = 'TA'
                   MOVE 20.00 TO TBLM-VAULT-BUY-YTD
               WHEN OTHER
                   COMPUTE TBLM-VAULT-BUY-YTD ROUNDED =
                       TBLT-VAULT-SELL-AMT * TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-VAULT-BUY-YTD > 0 AND TBLM-VAULT-BUY-YTD < 2.00
               MOVE 2.00 TO TBLM-VAULT-BUY-YTD
           END-IF
           IF TBLM-VAULT-BUY-YTD > 250.00
               MOVE 250.00 TO TBLM-VAULT-BUY-YTD
           END-IF
           ADD TBLM-VAULT-BUY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH OUT YTD.
      *----------------------------------------------------------------*
       3880-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-OUT-YTD ROUNDED =
                   TBLM-VAULT-BUY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-CASH-OUT-YTD
           END-IF
           ADD TBLM-CASH-OUT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT BATCH YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLM-BATCH-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TBLM-BATCH-YTD - (WS-WORK-AMT * 4)
           COMPUTE TBLM-OVERSHORT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BATCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3900-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-BATCH-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E357 MEMO REQUIRED
      * E998 BATCH YTD NEGATIVE
      * E849 BATCH YTD OVER 10000
      * E788 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-MEMO = SPACES
                   MOVE 'E357' TO WS-REJECT-CODE
               WHEN TBLM-BATCH-YTD < 0
                   MOVE 'E998' TO WS-REJECT-CODE
               WHEN TBLM-BATCH-YTD > 10000.00
                   MOVE 'E849' TO WS-REJECT-CODE
               WHEN TBLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E788' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E643 CUST ID REQUIRED
      * E668 OVERSHORT AMT NEGATIVE
      * E782 OVERSHORT AMT OVER 250000
      * E224 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CUST-ID = SPACES
                   MOVE 'E643' TO WS-REJECT-CODE
               WHEN TBLT-OVERSHORT-AMT < 0
                   MOVE 'E668' TO WS-REJECT-CODE
               WHEN TBLT-OVERSHORT-AMT > 250000.00
                   MOVE 'E782' TO WS-REJECT-CODE
               WHEN TBLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E224' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3930-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-BATCH-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BATCH AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3940-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLT-BATCH-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TBLT-BATCH-AMT - (WS-WORK-AMT * 2)
           COMPUTE TBLM-CASH-IN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BATCH AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3950-LIMIT-BATCH.
           IF TBLT-BATCH-AMT < 50.00
               MOVE 50.00 TO TBLT-BATCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-BATCH-AMT > 5000.00
               MOVE 5000.00 TO TBLT-BATCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH OUT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = TBLT-CASH-OUT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TBLT-CASH-OUT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE TBLM-BATCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3970-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-REASON-CD
                   ADD TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * BATCH YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3980-LIMIT-BATCH.
           IF TBLM-BATCH-YTD < 0.00
               MOVE 0.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-BATCH-YTD > 5000.00
               MOVE 5000.00 TO TBLM-BATCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE GRID TIER FOR THE VAULT SELL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3990-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-GRID-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TBLM-VAULT-SELL-YTD NOT < WS-VAULT-SE-GRID-LOW(WS-TX)
                   MOVE WS-VAULT-SE-GRID-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4000-COUNT-ITEM.
           IF TBLM-ITEM-CNT < 9999999
               ADD 1 TO TBLM-ITEM-CNT
           END-IF
           IF TBLM-ITEM-CNT NOT < 12
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BATCH YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLT-CHANNEL-CD
                   ADD TBLM-BATCH-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-BATCH-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH OUT YTD.
      *----------------------------------------------------------------*
       4020-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-OUT-YTD ROUNDED =
                   TBLT-VAULT-SELL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-CASH-OUT-YTD
           END-IF
           ADD TBLM-CASH-OUT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4030-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 3
               MOVE 'Y' TO TBLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4040-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TBLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO TBLM-OVERSHORT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TBLM-OVERSHORT-YTD ROUNDED = WS-WORK-AMT-2 *
                       TBLM-RATE / 200
               WHEN TBLM-CYCLE-CD = 'KX'
                   MOVE 10.00 TO TBLM-OVERSHORT-YTD
               WHEN TBLM-CYCLE-CD = 'TA'
                   MOVE 12.00 TO TBLM-OVERSHORT-YTD
               WHEN OTHER
                   COMPUTE TBLM-OVERSHORT-YTD ROUNDED = WS-WORK-AMT-2 *
                       TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-OVERSHORT-YTD > 0 AND TBLM-OVERSHORT-YTD < 2.00
               MOVE 2.00 TO TBLM-OVERSHORT-YTD
           END-IF
           IF TBLM-OVERSHORT-YTD > 75.00
               MOVE 75.00 TO TBLM-OVERSHORT-YTD
           END-IF
           ADD TBLM-OVERSHORT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       4050-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                   TBLT-OVERSHORT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH IN YTD.
      *----------------------------------------------------------------*
       4060-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-IN-YTD ROUNDED =
                   TBLM-VAULT-BUY-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-CASH-IN-YTD
           END-IF
           ADD TBLM-CASH-IN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE VAULT SELL AMT BY PROOF TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-PROOF-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-PROOF-TYPE-CD
                   ADD TBLT-VAULT-SELL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-PROOF-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-VAULT-SELL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4080-LIMIT-CASH-IN.
           IF TBLM-CASH-IN-AMT < 100.00
               MOVE 100.00 TO TBLM-CASH-IN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-IN-AMT > 10000.00
               MOVE 10000.00 TO TBLM-CASH-IN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * BATCH YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4090-FLAG-BATCH.
           IF TBLM-BATCH-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-BATCH-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4100-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF TBLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4110-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E290 ADDR REQUIRED
      * E776 CASH OUT YTD NEGATIVE
      * E083 CASH OUT YTD OVER 99999
      * E140 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4120-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-ADDR = SPACES
                   MOVE 'E290' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-YTD < 0
                   MOVE 'E776' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-YTD > 99999.00
                   MOVE 'E083' TO WS-REJECT-CODE
               WHEN TBLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E140' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4130-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF TBLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TBLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BATCH AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4140-ACCRUE-BATCH.
           IF TBLM-BATCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-BATCH-AMT * TBLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-CASH-IN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT SELL AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-VAULT-SELL-AMT ROUNDED =
                   TBLM-OVERSHORT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLT-VAULT-SELL-AMT
           END-IF
           ADD TBLT-VAULT-SELL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E117 ADDR REQUIRED
      * E344 CASH IN AMT NEGATIVE
      * E583 CASH IN AMT OVER 50000
      * E330 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4160-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-ADDR = SPACES
                   MOVE 'E117' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT < 0
                   MOVE 'E344' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT > 50000.00
                   MOVE 'E583' TO WS-REJECT-CODE
               WHEN TBLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT BUY YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4170-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-BUY-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-BUY-YTD - (WS-WORK-AMT *
               2)
           COMPUTE TBLM-VAULT-SELL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4180-COUNT-EXCEPT.
           IF TBLM-EXCEPT-CNT < 9999999
               ADD 1 TO TBLM-EXCEPT-CNT
           END-IF
           IF TBLM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO TBLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4190-LIMIT-OVERSHORT.
           IF TBLM-OVERSHORT-AMT < 100.00
               MOVE 100.00 TO TBLM-OVERSHORT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-OVERSHORT-AMT > 2500.00
               MOVE 2500.00 TO TBLM-OVERSHORT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4200-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-CYCLE-CD
                   ADD TBLM-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE GRID TIER FOR THE BATCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4210-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-GRID-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TBLM-BATCH-AMT NOT < WS-VAULT-SE-GRID-LOW(WS-TX)
                   MOVE WS-VAULT-SE-GRID-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4220-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CASH OUT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BATCH AMT.
      *----------------------------------------------------------------*
       4230-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-BATCH-AMT ROUNDED =
                   TBLM-CASH-OUT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLT-BATCH-AMT
           END-IF
           ADD TBLT-BATCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E432 OPERATOR REQUIRED
      * E808 BATCH YTD NEGATIVE
      * E028 BATCH YTD OVER 99999
      * E602 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4240-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-OPERATOR = SPACES
                   MOVE 'E432' TO WS-REJECT-CODE
               WHEN TBLM-BATCH-YTD < 0
                   MOVE 'E808' TO WS-REJECT-CODE
               WHEN TBLM-BATCH-YTD > 99999.00
                   MOVE 'E028' TO WS-REJECT-CODE
               WHEN TBLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4250-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT BUY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO OVERSHORT YTD.
      *----------------------------------------------------------------*
       4260-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-OVERSHORT-YTD ROUNDED =
                   TBLM-VAULT-BUY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-OVERSHORT-YTD
           END-IF
           ADD TBLM-OVERSHORT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4270-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TBLM-OVERSHORT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TBLM-OVERSHORT-YTD - (WS-WORK-AMT *
               12)
           COMPUTE TBLT-CASH-OUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT FEE ON THE CASH IN YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4290-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TBLM-OVERSHORT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLM-OVERSHORT-AMT ROUNDED = TBLM-CASH-IN-YTD
                       * TBLM-RATE / 200
               WHEN TBLM-SOURCE-CD = 'AF'
                   MOVE 10.00 TO TBLM-OVERSHORT-AMT
               WHEN TBLM-SOURCE-CD = 'PT'
                   MOVE 5.00 TO TBLM-OVERSHORT-AMT
               WHEN OTHER
                   COMPUTE TBLM-OVERSHORT-AMT ROUNDED = TBLM-CASH-IN-YTD
                       * TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-OVERSHORT-AMT > 0 AND TBLM-OVERSHORT-AMT < 1.00
               MOVE 1.00 TO TBLM-OVERSHORT-AMT
           END-IF
           IF TBLM-OVERSHORT-AMT > 150.00
               MOVE 150.00 TO TBLM-OVERSHORT-AMT
           END-IF
           ADD TBLM-OVERSHORT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BATCH AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4300-FLAG-BATCH.
           IF TBLM-BATCH-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-BATCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4310-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLT-VAULT-SELL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4320-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TBLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED = WS-WORK-AMT-2 *
                       TBLM-RATE / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 12.00 TO TBLM-CASH-IN-AMT
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 15.00 TO TBLM-CASH-IN-AMT
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED = WS-WORK-AMT-2 *
                       TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-CASH-IN-AMT > 0 AND TBLM-CASH-IN-AMT < 2.00
               MOVE 2.00 TO TBLM-CASH-IN-AMT
           END-IF
           IF TBLM-CASH-IN-AMT > 75.00
               MOVE 75.00 TO TBLM-CASH-IN-AMT
           END-IF
           ADD TBLM-CASH-IN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASH IN YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4330-LIMIT-CASH-IN.
           IF TBLM-CASH-IN-YTD < 100.00
               MOVE 100.00 TO TBLM-CASH-IN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-IN-YTD > 500.00
               MOVE 500.00 TO TBLM-CASH-IN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4340-COUNT-ITEM.
           IF TBLM-ITEM-CNT < 9999999
               ADD 1 TO TBLM-ITEM-CNT
           END-IF
           IF TBLM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E530 CONTACT REQUIRED
      * E824 OVERSHORT AMT NEGATIVE
      * E764 OVERSHORT AMT OVER 99999
      * E760 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4350-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CONTACT = SPACES
                   MOVE 'E530' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-AMT < 0
                   MOVE 'E824' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-AMT > 99999.00
                   MOVE 'E764' TO WS-REJECT-CODE
               WHEN TBLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E760' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4360-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF TBLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4370-COUNT-NOTICE.
           IF TBLM-NOTICE-CNT < 9999999
               ADD 1 TO TBLM-NOTICE-CNT
           END-IF
           IF TBLM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO TBLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E204 ADDR REQUIRED
      * E031 VAULT BUY YTD NEGATIVE
      * E440 VAULT BUY YTD OVER 5000
      * E026 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4380-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-ADDR = SPACES
                   MOVE 'E204' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-BUY-YTD < 0
                   MOVE 'E031' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-BUY-YTD > 5000.00
                   MOVE 'E440' TO WS-REJECT-CODE
               WHEN TBLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E026' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4390-COUNT-NOTICE.
           IF TBLM-NOTICE-CNT < 9999999
               ADD 1 TO TBLM-NOTICE-CNT
           END-IF
           IF TBLM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO TBLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT BUY YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4400-SPLIT-VAULT-BUY.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-BUY-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-BUY-YTD - (WS-WORK-AMT *
               2)
           COMPUTE TBLM-OVERSHORT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH OUT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4410-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = TBLM-CASH-OUT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TBLM-CASH-OUT-YTD - (WS-WORK-AMT *
               12)
           COMPUTE TBLM-BATCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4420-LIMIT-CASH-OUT.
           IF TBLM-CASH-OUT-YTD < 10.00
               MOVE 10.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-OUT-YTD > 1000.00
               MOVE 1000.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4430-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN YTD FEE ON THE VAULT BUY YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4440-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLM-VAULT-BUY-YTD
                       * TBLM-DISCOUNT / 200
               WHEN TBLM-CHANNEL-CD = 'D'
                   MOVE 12.00 TO TBLM-CASH-IN-YTD
               WHEN TBLM-CHANNEL-CD = 'F'
                   MOVE 7.00 TO TBLM-CASH-IN-YTD
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLM-VAULT-BUY-YTD
                       * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-CASH-IN-YTD > 0 AND TBLM-CASH-IN-YTD < 2.00
               MOVE 2.00 TO TBLM-CASH-IN-YTD
           END-IF
           IF TBLM-CASH-IN-YTD > 50.00
               MOVE 50.00 TO TBLM-CASH-IN-YTD
           END-IF
           ADD TBLM-CASH-IN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4450-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4460-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TBLM-OVERSHORT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TBLM-OVERSHORT-YTD - (WS-WORK-AMT *
               12)
           COMPUTE TBLT-OVERSHORT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH YTD FEE ON THE OVERSHORT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4470-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TBLM-BATCH-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   COMPUTE TBLM-BATCH-YTD ROUNDED = TBLT-OVERSHORT-AMT *
                       TBLM-DISCOUNT / 200
               WHEN TBLM-CYCLE-CD = 'KX'
                   MOVE 20.00 TO TBLM-BATCH-YTD
               WHEN TBLM-CYCLE-CD = 'TA'
                   MOVE 15.00 TO TBLM-BATCH-YTD
               WHEN OTHER
                   COMPUTE TBLM-BATCH-YTD ROUNDED = TBLT-OVERSHORT-AMT *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-BATCH-YTD > 0 AND TBLM-BATCH-YTD < 10.00
               MOVE 10.00 TO TBLM-BATCH-YTD
           END-IF
           IF TBLM-BATCH-YTD > 50.00
               MOVE 50.00 TO TBLM-BATCH-YTD
           END-IF
           ADD TBLM-BATCH-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * OVERSHORT AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4480-LIMIT-OVERSHORT.
           IF TBLT-OVERSHORT-AMT < 10.00
               MOVE 10.00 TO TBLT-OVERSHORT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-OVERSHORT-AMT > 5000.00
               MOVE 5000.00 TO TBLT-OVERSHORT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4490-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TBLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH IN YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4500-ACCRUE-CASH-IN.
           IF TBLM-CASH-IN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-CASH-IN-YTD * TBLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TBLM-CASH-OUT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4510-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-GRID-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT NOT < WS-VAULT-SE-GRID-LOW(WS-TX)
                   MOVE WS-VAULT-SE-GRID-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4520-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TBLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-BATCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY PROOF TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4530-ACCUM-PROOF-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-PROOF-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-PROOF-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4540-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE GRID TIER FOR THE CASH IN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4550-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-GRID-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TBLM-CASH-IN-AMT NOT < WS-VAULT-SE-GRID-LOW(WS-TX)
                   MOVE WS-VAULT-SE-GRID-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E713 DESC REQUIRED
      * E550 CASH OUT AMT NEGATIVE
      * E188 CASH OUT AMT OVER 10000
      * E151 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4560-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-DESC = SPACES
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-AMT < 0
                   MOVE 'E550' TO WS-REJECT-CODE
               WHEN TBLM-CASH-OUT-AMT > 10000.00
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN TBLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E151' TO WS-REJECT-CODE
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
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4570-SET-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4580-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TBLT-OVERSHORT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TBLT-OVERSHORT-AMT - (WS-WORK-AMT *
               3)
           COMPUTE TBLT-BATCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4590-DESCRIBE-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BO'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'OS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT BUY AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4600-FLAG-VAULT-BUY.
           IF TBLM-VAULT-BUY-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-BUY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * KX -> TA WHEN EFF DATE IS PAST
      * TA -> TG WHEN EFF DATE IS PAST
      * TG -> WG WHEN EFF DATE IS PAST
      * WG -> KX WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4610-SET-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TG'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WG'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4620-DESCRIBE-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4630-COUNT-NOTICE.
           IF TBLM-NOTICE-CNT < 9999999
               ADD 1 TO TBLM-NOTICE-CNT
           END-IF
           IF TBLM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4640-DESCRIBE-DRAWER.
           EVALUATE TBLM-DRAWER-CD
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'R'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4650-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4660-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TBLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TBLT-OVERSHORT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAWER CD:
      * D -> F WHEN POST DATE IS PAST
      * F -> R WHEN POST DATE IS PAST
      * R -> D WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4670-SET-DRAWER.
           EVALUATE TBLM-DRAWER-CD
               WHEN 'D'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TBLM-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TBLM-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TBLM-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E092 REJECT CD REQUIRED
      * E962 OVERSHORT AMT NEGATIVE
      * E302 OVERSHORT AMT OVER 50000
      * E674 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4680-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-REJECT-CD = SPACES
                   MOVE 'E092' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-AMT < 0
                   MOVE 'E962' TO WS-REJECT-CODE
               WHEN TBLM-OVERSHORT-AMT > 50000.00
                   MOVE 'E302' TO WS-REJECT-CODE
               WHEN TBLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E674' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4690-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT SELL YTD FEE ON THE OVERSHORT YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4700-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TBLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TBLM-VAULT-SELL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLM-VAULT-SELL-YTD ROUNDED =
                       TBLM-OVERSHORT-YTD * TBLM-RATE / 200
               WHEN TBLM-SOURCE-CD = 'AF'
                   MOVE 5.00 TO TBLM-VAULT-SELL-YTD
               WHEN TBLM-SOURCE-CD = 'PT'
                   MOVE 20.00 TO TBLM-VAULT-SELL-YTD
               WHEN OTHER
                   COMPUTE TBLM-VAULT-SELL-YTD ROUNDED =
                       TBLM-OVERSHORT-YTD * TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-VAULT-SELL-YTD > 0 AND TBLM-VAULT-SELL-YTD < 1.00
               MOVE 1.00 TO TBLM-VAULT-SELL-YTD
           END-IF
           IF TBLM-VAULT-SELL-YTD > 250.00
               MOVE 250.00 TO TBLM-VAULT-SELL-YTD
           END-IF
           ADD TBLM-VAULT-SELL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4710-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH OUT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4720-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = TBLT-CASH-OUT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TBLT-CASH-OUT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE TBLM-VAULT-SELL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE GRID TIER FOR THE OVERSHORT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4730-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-GRID-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TBLM-OVERSHORT-AMT NOT < WS-VAULT-SE-GRID-LOW(WS-TX)
                   MOVE WS-VAULT-SE-GRID-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E366 CONTACT REQUIRED
      * E106 VAULT BUY AMT NEGATIVE
      * E705 VAULT BUY AMT OVER 25000
      * E709 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4740-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CONTACT = SPACES
                   MOVE 'E366' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-BUY-AMT < 0
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-BUY-AMT > 25000.00
                   MOVE 'E705' TO WS-REJECT-CODE
               WHEN TBLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E709' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4750-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-VAULT-BUY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-VAULT-BUY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-VAULT-BUY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-VAULT-BUY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-VAULT-BUY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE OVERSHOR SCHED TIER FOR THE VAULT SELL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4760-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-OVERSHOR-SCHED-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TBLT-VAULT-SELL-AMT NOT < WS-OVERSHOR-SCHED-LOW(WS-TX)
                   MOVE WS-OVERSHOR-SCHED-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4770-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT SELL AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4780-FLAG-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-SELL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4790-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TBLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE GRID TIER FOR THE CASH IN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4800-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-GRID-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TBLM-CASH-IN-YTD NOT < WS-VAULT-SE-GRID-LOW(WS-TX)
                   MOVE WS-VAULT-SE-GRID-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4810-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BI -> BO WHEN OPEN DATE IS PAST
      * BO -> VB WHEN OPEN DATE IS PAST
      * VB -> VS WHEN OPEN DATE IS PAST
      * VS -> OS WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4820-SET-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BO' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BO'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VB'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VS'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-TLPEN.
           INITIALIZE TLPEN-PARMS
           MOVE TBLM-OVERSHORT-AMT
             TO TLPEN-OVERSHORT-AMT
           MOVE TBLM-VAULT-BUY-AMT
             TO TLPEN-VAULT-BUY-AMT
           MOVE TBLM-CASH-IN-AMT
             TO TLPEN-CASH-IN-AMT
           MOVE TBLM-CASH-OUT-YTD
             TO TLPEN-CASH-OUT-YTD
           MOVE TBLM-BATCH-AMT
             TO TLPEN-BATCH-AMT
           MOVE TBLM-CASH-OUT-AMT
             TO TLPEN-CASH-OUT-AMT
           MOVE TBLM-OPEN-DATE
             TO TLPEN-OPEN-DATE
           MOVE TBLM-EFFECTIVE-DATE
             TO TLPEN-EFFECTIVE-DATE
           MOVE TBLM-NOTICE-DATE
             TO TLPEN-NOTICE-DATE
           MOVE TBLM-NEXT-REVIEW-DATE
             TO TLPEN-NEXT-REVIEW-DATE
           MOVE TBLM-DRAWER-CD
             TO TLPEN-DRAWER-CD
           MOVE TBLM-REASON-CD
             TO TLPEN-REASON-CD
           MOVE TBLM-CHANNEL-CD
             TO TLPEN-CHANNEL-CD
           MOVE TBLM-SOURCE-CD
             TO TLPEN-SOURCE-CD
           MOVE TBLM-SUSPEND-FLAG
             TO TLPEN-SUSPEND-FLAG
           MOVE TBLM-REVIEW-FLAG
             TO TLPEN-REVIEW-FLAG
           MOVE TBLM-HOLD-FLAG
             TO TLPEN-HOLD-FLAG
           MOVE TBLM-ITEM-CNT
             TO TLPEN-ITEM-CNT
           MOVE TBLM-RETRY-CNT
             TO TLPEN-RETRY-CNT
           MOVE TBLM-RATE
             TO TLPEN-RATE
           MOVE TBLM-FEE-PCT
             TO TLPEN-FEE-PCT
           MOVE TBLM-REF-NO
             TO TLPEN-REF-NO
           MOVE TBLM-DESC
             TO TLPEN-DESC
           CALL 'TLPEN' USING TLPEN-PARMS
           EVALUATE TLPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLPEN-OVERSHORT-AMT
                     TO TBLM-OVERSHORT-AMT
                   MOVE TLPEN-VAULT-BUY-AMT
                     TO TBLM-VAULT-BUY-AMT
                   MOVE TLPEN-CASH-OUT-YTD
                     TO TBLM-CASH-OUT-YTD
                   MOVE TLPEN-CASH-OUT-AMT
                     TO TBLM-CASH-OUT-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-TLPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-TLFEE.
           INITIALIZE TLFEE-PARMS
           MOVE TBLM-VAULT-BUY-AMT
             TO TLFEE-VAULT-BUY-AMT
           MOVE TBLM-VAULT-SELL-YTD
             TO TLFEE-VAULT-SELL-YTD
           MOVE TBLM-CASH-IN-YTD
             TO TLFEE-CASH-IN-YTD
           MOVE TBLM-VAULT-BUY-YTD
             TO TLFEE-VAULT-BUY-YTD
           MOVE TBLM-VAULT-SELL-AMT
             TO TLFEE-VAULT-SELL-AMT
           MOVE TBLM-CASH-OUT-YTD
             TO TLFEE-CASH-OUT-YTD
           MOVE TBLM-OPEN-DATE
             TO TLFEE-OPEN-DATE
           MOVE TBLM-NEXT-REVIEW-DATE
             TO TLFEE-NEXT-REVIEW-DATE
           MOVE TBLM-LAST-PAY-DATE
             TO TLFEE-LAST-PAY-DATE
           MOVE TBLM-LAST-ACT-DATE
             TO TLFEE-LAST-ACT-DATE
           MOVE TBLM-CHANNEL-CD
             TO TLFEE-CHANNEL-CD
           MOVE TBLM-PROOF-TYPE-CD
             TO TLFEE-PROOF-TYPE-CD
           MOVE TBLM-DRAWER-CD
             TO TLFEE-DRAWER-CD
           MOVE TBLM-CYCLE-CD
             TO TLFEE-CYCLE-CD
           MOVE TBLM-EXEMPT-FLAG
             TO TLFEE-EXEMPT-FLAG
           MOVE TBLM-STOP-FLAG
             TO TLFEE-STOP-FLAG
           MOVE TBLM-OVERRIDE-FLAG
             TO TLFEE-OVERRIDE-FLAG
           MOVE TBLM-EXCEPT-CNT
             TO TLFEE-EXCEPT-CNT
           MOVE TBLM-NOTICE-CNT
             TO TLFEE-NOTICE-CNT
           MOVE TBLM-DISCOUNT
             TO TLFEE-DISCOUNT
           MOVE TBLM-RATE
             TO TLFEE-RATE
           MOVE TBLM-DESC
             TO TLFEE-DESC
           MOVE TBLM-NAME
             TO TLFEE-NAME
           CALL 'TLFEE' USING TLFEE-PARMS
           EVALUATE TLFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLFEE-VAULT-BUY-AMT
                     TO TBLM-VAULT-BUY-AMT
                   MOVE TLFEE-VAULT-BUY-YTD
                     TO TBLM-VAULT-BUY-YTD
                   MOVE TLFEE-CASH-OUT-YTD
                     TO TBLM-CASH-OUT-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-TLFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-TLWAIV.
           INITIALIZE TLWAIV-PARMS
           MOVE TBLM-VAULT-SELL-AMT
             TO TLWAIV-VAULT-SELL-AMT
           MOVE TBLM-VAULT-BUY-YTD
             TO TLWAIV-VAULT-BUY-YTD
           MOVE TBLM-BATCH-YTD
             TO TLWAIV-BATCH-YTD
           MOVE TBLM-CASH-IN-AMT
             TO TLWAIV-CASH-IN-AMT
           MOVE TBLM-CASH-IN-YTD
             TO TLWAIV-CASH-IN-YTD
           MOVE TBLM-CASH-OUT-YTD
             TO TLWAIV-CASH-OUT-YTD
           MOVE TBLM-EFFECTIVE-DATE
             TO TLWAIV-EFFECTIVE-DATE
           MOVE TBLM-LAST-STMT-DATE
             TO TLWAIV-LAST-STMT-DATE
           MOVE TBLM-LAST-PAY-DATE
             TO TLWAIV-LAST-PAY-DATE
           MOVE TBLM-OPEN-DATE
             TO TLWAIV-OPEN-DATE
           MOVE TBLM-SOURCE-CD
             TO TLWAIV-SOURCE-CD
           MOVE TBLM-DRAWER-CD
             TO TLWAIV-DRAWER-CD
           MOVE TBLM-CHANNEL-CD
             TO TLWAIV-CHANNEL-CD
           MOVE TBLM-REASON-CD
             TO TLWAIV-REASON-CD
           MOVE TBLM-STOP-FLAG
             TO TLWAIV-STOP-FLAG
           MOVE TBLM-SUSPEND-FLAG
             TO TLWAIV-SUSPEND-FLAG
           MOVE TBLM-EXEMPT-FLAG
             TO TLWAIV-EXEMPT-FLAG
           MOVE TBLM-ITEM-CNT
             TO TLWAIV-ITEM-CNT
           MOVE TBLM-EXCEPT-CNT
             TO TLWAIV-EXCEPT-CNT
           MOVE TBLM-FEE-PCT
             TO TLWAIV-FEE-PCT
           MOVE TBLM-RATE
             TO TLWAIV-RATE
           MOVE TBLM-DESC
             TO TLWAIV-DESC
           MOVE TBLM-REF-NO
             TO TLWAIV-REF-NO
           CALL 'TLWAIV' USING TLWAIV-PARMS
           EVALUATE TLWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLWAIV-VAULT-BUY-YTD
                     TO TBLM-VAULT-BUY-YTD
                   MOVE TLWAIV-STOP-FLAG
                     TO TBLM-STOP-FLAG
                   MOVE TLWAIV-SUSPEND-FLAG
                     TO TBLM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-TLWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TBLT-KEY TO RD-KEY
           MOVE TBLT-TRAN-CD TO RD-CODE
           MOVE TBLT-CASH-OUT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TBLT-KEY TO RD-KEY
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
           DISPLAY 'TLLEXTR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TLLEXTR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
