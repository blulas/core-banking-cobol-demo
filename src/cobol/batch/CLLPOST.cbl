      *================================================================*
      * PROGRAM  : CLLPOST                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : COLLECTIONS QUEUE - DAILY POSTING. READS THE COL    *
      *            TRANSACTIONS, UPDATES THE COL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. DELINQUENT ACCOUNT COLLECTION   *
      *            QUEUES, PROMISES AND LETTERS                        *
      * INPUT    : COLMST (KSDS), COLTRN, BANKCTL                      *
      * OUTPUT   : COLOUT, CLLPORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CLTIER, CLLIM, CLRISK   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-04-22 TWH  ORIGINAL                                       *
      * 1997-08-07 LMB  WRITEOFF EXCEPTIONS TO OPS QUEUE INC1943       *
      * 1998-02-24 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-01-20 CRP  COLLECTED CALCULATION CORRECTED INC5773        *
      * 1999-11-23 EWS  PAST-DUE CALCULATION CORRECTED PRB9065         *
      * 2000-03-08 EWS  PROMISED NOTICE TEXT PER LEGAL PRB4377         *
      * 2001-10-09 JLM  NEW PAST-DUE REPORT COLUMNS INC5116            *
      * 2003-02-14 JLM  COLLECTED ROUNDING - HALF UP REQ3590           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLLPOST.
       AUTHOR. M K LOWE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "COLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS COLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "COLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "COLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "CLLPORPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY COLMST.
       FD  TRAN-FILE.
       COPY COLTRN.
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
                                           VALUE 15.000000.
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
      * WS-PROMISED-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PROMISED-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000463000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000486500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000863500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001200000000673500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001450000000212500'.
       01  WS-PROMISED-TIER-TBL REDEFINES WS-PROMISED-TIER-VALUES.
           05  WS-PROMISED-TIER-ROW OCCURS 5 TIMES.
               10  WS-PROMISED-TIER-LOW    PIC 9(09)V99.
               10  WS-PROMISED-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-WRITEOFF-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WRITEOFF-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000793500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000360000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000409000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000692500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000463000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000617500'.
       01  WS-WRITEOFF-GRID-TBL REDEFINES WS-WRITEOFF-GRID-VALUES.
           05  WS-WRITEOFF-GRID-ROW OCCURS 6 TIMES.
               10  WS-WRITEOFF-GRID-LOW    PIC 9(09)V99.
               10  WS-WRITEOFF-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CLLPOST  COLLECTIONS QUEUE'.
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
       COPY CLTIERP.
       COPY CLLIMP.
       COPY CLRISKP.

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
               MOVE 'COLMST' TO ABND-FILE-NAME
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
           MOVE COLT-KEY TO COLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SPLIT-FEE
               PERFORM 3010-ACCUM-REASON
               PERFORM 3020-DESCRIBE-ACTION
               PERFORM 3030-SPLIT-COLLECTED
               PERFORM 3040-PRORATE-WRITEOFF
               PERFORM 3050-ACCUM-TRAN
               PERFORM 3060-CHECK-NOTICE
               PERFORM 3070-LIMIT-COLLECTED
               PERFORM 3080-SPLIT-COLLECTED
               PERFORM 3090-COUNT-ITEM
               PERFORM 3100-AGE-NOTICE
               PERFORM 3110-COUNT-SEQ
               PERFORM 3120-SET-TRAN
               PERFORM 3130-DESCRIBE-CHANNEL
               PERFORM 3140-PRORATE-PAST-DUE
               PERFORM 3150-ASSESS-SUSPEND
               PERFORM 3160-EDIT-NAME
               PERFORM 3170-FLAG-PAST-DUE
               PERFORM 3180-SET-REGION
               PERFORM 3190-SET-STATUS
               PERFORM 3200-SPLIT-WRITEOFF
               PERFORM 3210-LIMIT-FEE
               PERFORM 3220-SPLIT-COLLECTED
               PERFORM 3230-SPLIT-FEE
               PERFORM 3240-SELECT-PENALTY-PCT
               PERFORM 3250-COUNT-CYCLE
               PERFORM 3260-DESCRIBE-REGION
               PERFORM 3270-LIMIT-COLLECTED
               PERFORM 3280-FLAG-PROMISED
               PERFORM 3290-FLAG-FEE
               PERFORM 3300-LIMIT-FEE
               PERFORM 3310-SELECT-FEE-PCT
               PERFORM 3320-LIMIT-WRITEOFF
               PERFORM 3330-AGE-LAST-PAY
               PERFORM 3340-ASSESS-SUSPEND
               PERFORM 3350-LIMIT-PROMISED
               PERFORM 3360-PRORATE-FEE
               PERFORM 3370-AGE-OPEN
               PERFORM 3380-SET-REGION
               PERFORM 3390-ACCRUE-COLLECTED
               PERFORM 3400-PRORATE-FEE
               PERFORM 3410-AGE-LAST-PAY
               PERFORM 3420-ACCUM-REGION
               PERFORM 3430-FLAG-PAST-DUE
               PERFORM 3440-SELECT-PENALTY-PCT
               PERFORM 3450-DESCRIBE-CHANNEL
               PERFORM 3460-ACCUM-CHANNEL
               PERFORM 3470-ACCUM-CYCLE
               PERFORM 3480-ACCUM-TRAN
               PERFORM 3490-DESCRIBE-QUEUE
               PERFORM 3500-PRORATE-WRITEOFF
               PERFORM 3510-ACCUM-CHANNEL
               PERFORM 3520-FLAG-ITEM
               PERFORM 3530-FLAG-FEE
               PERFORM 3540-SELECT-FEE-PCT
               PERFORM 3550-ACCUM-REGION
               PERFORM 3560-ACCRUE-PAST-DUE
               PERFORM 3570-PRORATE-FEE
               PERFORM 3580-DESCRIBE-REASON
               PERFORM 3590-LIMIT-COLLECTED
               PERFORM 3600-FLAG-FEE
               PERFORM 3610-PRORATE-PAST-DUE
               PERFORM 3620-PRORATE-PROMISED
               PERFORM 3630-PRORATE-COLLECTED
               PERFORM 3640-ACCUM-REASON
               PERFORM 3650-ASSESS-PREFERRED
               PERFORM 3660-ACCRUE-MARGIN
               PERFORM 3670-SELECT-MARGIN
               PERFORM 3680-SELECT-FEE-PCT
               PERFORM 3690-ACCUM-REASON
               PERFORM 3700-SPLIT-PAST-DUE
               PERFORM 3710-SET-REASON
               PERFORM 3720-PRORATE-PROMISED
               PERFORM 3730-EDIT-CONTACT
               PERFORM 3740-AGE-LAST-MAINT
               PERFORM 3750-FLAG-FEE
               PERFORM 3760-PRORATE-PROMISED
               PERFORM 3770-ASSESS-SUSPEND
               PERFORM 3780-AGE-NEXT-REVIEW
               PERFORM 3790-CHECK-LAST-MAINT
               PERFORM 3800-LIMIT-ITEM
               PERFORM 3810-SPLIT-PAST-DUE
               PERFORM 3820-PRORATE-ITEM
               PERFORM 3830-FLAG-PROMISED
               PERFORM 3840-EDIT-ADDR
               PERFORM 3850-ASSESS-OVERRIDE
               PERFORM 3860-SET-QUEUE
               PERFORM 3870-ASSESS-OVERRIDE
               PERFORM 3880-COUNT-SEQ
               PERFORM 3890-ACCUM-TRAN
               PERFORM 3900-LIMIT-WRITEOFF
               PERFORM 3910-COUNT-NOTICE
               PERFORM 3920-CHECK-EFF
               PERFORM 3930-AGE-NOTICE
               PERFORM 3940-ACCUM-ACTION
               PERFORM 3950-AGE-NEXT-REVIEW
               PERFORM 3960-DESCRIBE-REGION
               PERFORM 3970-FLAG-WRITEOFF
               PERFORM 3980-ASSESS-SUSPEND
               PERFORM 3990-SET-REASON
               PERFORM 4000-PRORATE-WRITEOFF
               PERFORM 4010-ACCUM-ACTION
               PERFORM 4020-PRORATE-COLLECTED
               PERFORM 4030-AGE-NEXT-REVIEW
               PERFORM 4040-ACCUM-TRAN
               PERFORM 4050-AGE-NEXT-DUE
               PERFORM 4060-PRORATE-FEE
               PERFORM 4070-AGE-LAST-PAY
               PERFORM 4080-ACCRUE-FEE
               PERFORM 4090-ACCUM-ACTION
               PERFORM 4100-LIMIT-COLLECTED
               PERFORM 4110-PRORATE-COLLECTED
               PERFORM 4120-SELECT-FEE-PCT
               PERFORM 4130-COUNT-SEQ
               PERFORM 4140-FLAG-FEE
               PERFORM 4150-LIMIT-ITEM
               PERFORM 4160-EDIT-CONTACT
               PERFORM 4170-SELECT-MARGIN
               PERFORM 4180-COUNT-ITEM
               PERFORM 4190-ASSESS-NOTICE-SENT
               PERFORM 4200-FLAG-PAST-DUE
               PERFORM 4210-SPLIT-PROMISED
               PERFORM 4220-SET-REGION
               PERFORM 4230-SELECT-PENALTY-PCT
               PERFORM 4240-AGE-EFFECTIVE
               PERFORM 4250-AGE-NEXT-REVIEW
               PERFORM 4260-CHECK-NEXT-REVIEW
               PERFORM 4270-COUNT-SEQ
               PERFORM 4280-SELECT-FEE-PCT
               PERFORM 4290-SET-CYCLE
               PERFORM 4300-COUNT-CYCLE
               PERFORM 4310-SET-REASON
               PERFORM 4320-ASSESS-EXEMPT
               PERFORM 4330-COUNT-RETRY
               PERFORM 4340-SELECT-PENALTY-PCT
               PERFORM 4350-CHECK-NEXT-REVIEW
               PERFORM 4360-SPLIT-FEE
               PERFORM 4370-SELECT-PENALTY-PCT
               PERFORM 4380-ASSESS-REVIEW
               PERFORM 4390-PRORATE-PROMISED
               PERFORM 4400-LIMIT-PAST-DUE
               PERFORM 4410-FLAG-PAST-DUE
               PERFORM 4420-SET-CYCLE
               PERFORM 4430-SELECT-MARGIN
               PERFORM 4440-SELECT-PENALTY-PCT
               PERFORM 4450-DESCRIBE-CHANNEL
               PERFORM 4460-ASSESS-REVIEW
               PERFORM 4470-LIMIT-FEE
               PERFORM 4480-FLAG-WRITEOFF
               PERFORM 4490-SELECT-PENALTY-PCT
               PERFORM 4500-AGE-POST
               PERFORM 4510-ACCRUE-PAST-DUE
               PERFORM 4520-LIMIT-WRITEOFF
               PERFORM 4530-PRORATE-FEE
               PERFORM 4540-AGE-EFFECTIVE
               PERFORM 4550-SPLIT-COLLECTED
               PERFORM 4560-ACCRUE-PENALTY-PCT
               PERFORM 4570-PRORATE-PROMISED
               PERFORM 4580-DESCRIBE-CYCLE
               PERFORM 4590-FLAG-PAST-DUE
               PERFORM 4600-PRORATE-PAST-DUE
               PERFORM 4610-SPLIT-COLLECTED
               PERFORM 4620-SPLIT-PAST-DUE
               PERFORM 4630-FLAG-COLLECTED
               PERFORM 4640-LIMIT-PROMISED
               PERFORM 4650-DESCRIBE-TRAN
               PERFORM 4660-ACCUM-QUEUE
               PERFORM 4670-COUNT-SEQ
               PERFORM 4680-AGE-OPEN
               PERFORM 4690-CHECK-LAST-PAY
               PERFORM 4700-LIMIT-WRITEOFF
               PERFORM 4710-ACCRUE-COLLECTED
               PERFORM 4720-COUNT-SEQ
               PERFORM 4730-LIMIT-PAST-DUE
               PERFORM 4740-SELECT-FEE-PCT
               PERFORM 4750-SET-ACTION
               PERFORM 4760-COUNT-CYCLE
               PERFORM 4770-COUNT-RETRY
               PERFORM 4780-EDIT-REF-NO
               PERFORM 4790-ASSESS-NOTICE-SENT
               PERFORM 4800-AGE-LAST-PAY
               PERFORM 4810-EDIT-DESC
               PERFORM 4820-SPLIT-WRITEOFF
               PERFORM 4830-EDIT-REF-NO
               PERFORM 4840-SPLIT-WRITEOFF
               PERFORM 4850-DESCRIBE-REGION
               PERFORM 4860-PRORATE-COLLECTED
               PERFORM 4870-SPLIT-COLLECTED
               PERFORM 4880-AGE-LAST-MAINT
               PERFORM 7010-CALL-CLTIER
               PERFORM 7030-CALL-CLLIM
               PERFORM 7020-CALL-CLRISK
               REWRITE COL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'COLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM COL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = COLT-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = COLT-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REASON-CD
                   ADD COLM-COLLECTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-COLLECTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ACTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3020-DESCRIBE-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'VL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3030-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLM-COLLECTED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = COLM-COLLECTED-AMT - (WS-WORK-AMT *
               4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WRITEOFF YTD.
      *----------------------------------------------------------------*
       3040-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-WRITEOFF-YTD ROUNDED =
                   COLM-PROMISED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-WRITEOFF-YTD
           END-IF
           ADD COLM-WRITEOFF-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE PAST DUE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-TRAN-CD
                   ADD COLM-PAST-DUE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-PAST-DUE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF COLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO COLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3070-LIMIT-COLLECTED.
           IF COLM-COLLECTED-AMT < 50.00
               MOVE 50.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-COLLECTED-AMT > 1000.00
               MOVE 1000.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3080-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLT-COLLECTED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = COLT-COLLECTED-AMT - (WS-WORK-AMT *
               3)
           COMPUTE COLM-WRITEOFF-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3090-COUNT-ITEM.
           IF COLM-ITEM-CNT < 9999999
               ADD 1 TO COLM-ITEM-CNT
           END-IF
           IF COLM-ITEM-CNT NOT < 5
               MOVE 'Y' TO COLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 3
               MOVE 'Y' TO COLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> CL WHEN NEXT REVIEW DATE IS PAST
      * CL -> LT WHEN NEXT REVIEW DATE IS PAST
      * LT -> WO WHEN NEXT REVIEW DATE IS PAST
      * WO -> RF WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'CL' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CL'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LT' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LT'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WO' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WO'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RF' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAST DUE AMT.
      *----------------------------------------------------------------*
       3140-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-PAST-DUE-AMT ROUNDED =
                   COLM-COLLECTED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-PAST-DUE-AMT
           END-IF
           ADD COLM-PAST-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WRITEOFF AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3150-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = COLM-WRITEOFF-AMT *
                       COLM-PENALTY-PCT / 200
               WHEN COLT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN COLT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = COLM-WRITEOFF-AMT *
                       COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E223 NAME REQUIRED
      * E335 FEE YTD NEGATIVE
      * E442 FEE YTD OVER 10000
      * E825 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3160-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-NAME = SPACES
                   MOVE 'E223' TO WS-REJECT-CODE
               WHEN COLM-FEE-YTD < 0
                   MOVE 'E335' TO WS-REJECT-CODE
               WHEN COLM-FEE-YTD > 10000.00
                   MOVE 'E442' TO WS-REJECT-CODE
               WHEN COLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E825' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3170-FLAG-PAST-DUE.
           IF COLT-PAST-DUE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLT-PAST-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AT -> AU WHEN CLOSE DATE IS PAST
      * AU -> XR WHEN CLOSE DATE IS PAST
      * XR -> XU WHEN CLOSE DATE IS PAST
      * XU -> AT WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3180-SET-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AU'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XR' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XR'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XU'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN EXPIRY DATE IS PAST
      * UR -> XM WHEN EXPIRY DATE IS PAST
      * XM -> YA WHEN EXPIRY DATE IS PAST
      * YA -> PS WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3190-SET-STATUS.
           EVALUATE COLM-STATUS-CD
               WHEN 'PS'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YA' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3200-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = COLM-WRITEOFF-AMT / 12
           COMPUTE WS-WORK-AMT-2 = COLM-WRITEOFF-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3210-LIMIT-FEE.
           IF COLM-FEE-AMT < 0.00
               MOVE 0.00 TO COLM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-FEE-AMT > 10000.00
               MOVE 10000.00 TO COLM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3220-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLM-COLLECTED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = COLM-COLLECTED-AMT - (WS-WORK-AMT *
               12)
           COMPUTE COLT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3230-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = COLT-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = COLT-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE COLM-PAST-DUE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF GRID TIER FOR THE PROMISED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-GRID-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF COLM-PROMISED-YTD NOT < WS-WRITEOFF-GRID-LOW(WS-TX)
                   MOVE WS-WRITEOFF-GRID-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3250-COUNT-CYCLE.
           IF COLM-CYCLE-CNT < 9999999
               ADD 1 TO COLM-CYCLE-CNT
           END-IF
           IF COLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO COLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3260-DESCRIBE-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'AU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3270-LIMIT-COLLECTED.
           IF COLM-COLLECTED-AMT < 10.00
               MOVE 10.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-COLLECTED-AMT > 5000.00
               MOVE 5000.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PROMISED AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3280-FLAG-PROMISED.
           IF COLM-PROMISED-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLM-PROMISED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3290-FLAG-FEE.
           IF COLT-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3300-LIMIT-FEE.
           IF COLM-FEE-AMT < 50.00
               MOVE 50.00 TO COLM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-FEE-AMT > 10000.00
               MOVE 10000.00 TO COLM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF GRID TIER FOR THE WRITEOFF YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3310-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-GRID-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF COLM-WRITEOFF-YTD NOT < WS-WRITEOFF-GRID-LOW(WS-TX)
                   MOVE WS-WRITEOFF-GRID-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3320-LIMIT-WRITEOFF.
           IF COLT-WRITEOFF-AMT < 0.00
               MOVE 0.00 TO COLT-WRITEOFF-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-WRITEOFF-AMT > 1000.00
               MOVE 1000.00 TO COLT-WRITEOFF-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3330-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED FEE ON THE WRITEOFF AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3340-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO COLM-COLLECTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   COMPUTE COLM-COLLECTED-AMT ROUNDED =
                       COLM-WRITEOFF-AMT * COLM-FEE-PCT / 200
               WHEN COLT-TRAN-CD = 'PR'
                   MOVE 7.00 TO COLM-COLLECTED-AMT
               WHEN COLT-TRAN-CD = 'CL'
                   MOVE 7.00 TO COLM-COLLECTED-AMT
               WHEN OTHER
                   COMPUTE COLM-COLLECTED-AMT ROUNDED =
                       COLM-WRITEOFF-AMT * COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-COLLECTED-AMT > 0 AND COLM-COLLECTED-AMT < 5.00
               MOVE 5.00 TO COLM-COLLECTED-AMT
           END-IF
           IF COLM-COLLECTED-AMT > 25.00
               MOVE 25.00 TO COLM-COLLECTED-AMT
           END-IF
           ADD COLM-COLLECTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PROMISED AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3350-LIMIT-PROMISED.
           IF COLM-PROMISED-AMT < 25.00
               MOVE 25.00 TO COLM-PROMISED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PROMISED-AMT > 500.00
               MOVE 500.00 TO COLM-PROMISED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3360-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-FEE-AMT ROUNDED =
                   COLT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-FEE-AMT
           END-IF
           ADD COLM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AT -> AU WHEN NEXT DUE DATE IS PAST
      * AU -> XR WHEN NEXT DUE DATE IS PAST
      * XR -> XU WHEN NEXT DUE DATE IS PAST
      * XU -> AT WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AU'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XR' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XR'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XU'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3390-ACCRUE-COLLECTED.
           IF COLM-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-COLLECTED-AMT * COLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3400-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-FEE-AMT ROUNDED =
                   COLT-PAST-DUE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLT-FEE-AMT
           END-IF
           ADD COLT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAST DUE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3420-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REGION-CD
                   ADD COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3430-FLAG-PAST-DUE.
           IF COLM-PAST-DUE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLM-PAST-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PROMISED TIER TIER FOR THE PROMISED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3440-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PROMISED-TIER-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF COLM-PROMISED-AMT NOT < WS-PROMISED-TIER-LOW(WS-TX)
                   MOVE WS-PROMISED-TIER-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3450-DESCRIBE-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3460-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-CHANNEL-CD
                   ADD COLM-COLLECTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-COLLECTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-CYCLE-CD
                   ADD COLM-COLLECTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-COLLECTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3480-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-TRAN-CD
                   ADD COLM-COLLECTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-COLLECTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE QUEUE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ER'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ME'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3500-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   COLM-WRITEOFF-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-CHANNEL-CD
                   ADD COLM-COLLECTED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-COLLECTED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3520-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3530-FLAG-FEE.
           IF COLM-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF GRID TIER FOR THE COLLECTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-GRID-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF COLT-COLLECTED-AMT NOT < WS-WRITEOFF-GRID-LOW(WS-TX)
                   MOVE WS-WRITEOFF-GRID-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3550-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REGION-CD
                   ADD COLM-WRITEOFF-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-WRITEOFF-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-PAST-DUE.
           IF COLT-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-PAST-DUE-AMT * COLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-PROMISED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3570-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3580-DESCRIBE-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3590-LIMIT-COLLECTED.
           IF COLT-COLLECTED-AMT < 50.00
               MOVE 50.00 TO COLT-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-COLLECTED-AMT > 50000.00
               MOVE 50000.00 TO COLT-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3600-FLAG-FEE.
           IF COLM-FEE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAST DUE AMT.
      *----------------------------------------------------------------*
       3610-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-PAST-DUE-AMT ROUNDED =
                   COLM-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLT-PAST-DUE-AMT
           END-IF
           ADD COLT-PAST-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PROMISED AMT.
      *----------------------------------------------------------------*
       3620-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-PROMISED-AMT ROUNDED =
                   COLM-PAST-DUE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-PROMISED-AMT
           END-IF
           ADD COLM-PROMISED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PROMISED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COLLECTED YTD.
      *----------------------------------------------------------------*
       3630-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-COLLECTED-YTD ROUNDED =
                   COLM-PROMISED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-COLLECTED-YTD
           END-IF
           ADD COLM-COLLECTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE PROMISED AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3640-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REASON-CD
                   ADD COLM-PROMISED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-PROMISED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FEE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3650-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = COLM-FEE-AMT *
                       COLM-MARGIN / 200
               WHEN COLM-STATUS-CD = 'PS'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN COLM-STATUS-CD = 'UR'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = COLM-FEE-AMT *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-MARGIN.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * COLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO COLT-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF GRID TIER FOR THE COLLECTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3670-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-GRID-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF COLT-COLLECTED-AMT NOT < WS-WRITEOFF-GRID-LOW(WS-TX)
                   MOVE WS-WRITEOFF-GRID-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PROMISED TIER TIER FOR THE COLLECTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PROMISED-TIER-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF COLT-COLLECTED-AMT NOT < WS-PROMISED-TIER-LOW(WS-TX)
                   MOVE WS-PROMISED-TIER-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3690-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REASON-CD
                   ADD COLM-WRITEOFF-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-WRITEOFF-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAST DUE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3700-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = COLM-PAST-DUE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = COLM-PAST-DUE-YTD - (WS-WORK-AMT * 3)
           COMPUTE COLM-COLLECTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN LAST PAY DATE IS PAST
      * BJ -> GG WHEN LAST PAY DATE IS PAST
      * GG -> ZR WHEN LAST PAY DATE IS PAST
      * ZR -> AT WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3710-SET-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZR' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PROMISED AMT.
      *----------------------------------------------------------------*
       3720-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-PROMISED-AMT ROUNDED =
                   COLM-WRITEOFF-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-PROMISED-AMT
           END-IF
           ADD COLM-PROMISED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E758 CONTACT REQUIRED
      * E879 PAST DUE YTD NEGATIVE
      * E076 PAST DUE YTD OVER 50000
      * E044 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3730-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CONTACT = SPACES
                   MOVE 'E758' TO WS-REJECT-CODE
               WHEN COLM-PAST-DUE-YTD < 0
                   MOVE 'E879' TO WS-REJECT-CODE
               WHEN COLM-PAST-DUE-YTD > 50000.00
                   MOVE 'E076' TO WS-REJECT-CODE
               WHEN COLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3750-FLAG-FEE.
           IF COLM-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3760-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   COLM-PROMISED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED FEE ON THE COLLECTED AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3770-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO COLM-COLLECTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE COLM-COLLECTED-AMT ROUNDED =
                       COLT-COLLECTED-AMT * COLM-FEE-PCT / 200
               WHEN COLT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO COLM-COLLECTED-AMT
               WHEN COLT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO COLM-COLLECTED-AMT
               WHEN OTHER
                   COMPUTE COLM-COLLECTED-AMT ROUNDED =
                       COLT-COLLECTED-AMT * COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-COLLECTED-AMT > 0 AND COLM-COLLECTED-AMT < 5.00
               MOVE 5.00 TO COLM-COLLECTED-AMT
           END-IF
           IF COLM-COLLECTED-AMT > 150.00
               MOVE 150.00 TO COLM-COLLECTED-AMT
           END-IF
           ADD COLM-COLLECTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3780-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-REVIEW-DATE > 0 AND COLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF COLM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO COLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3800-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAST DUE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3810-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = COLT-PAST-DUE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = COLT-PAST-DUE-AMT - (WS-WORK-AMT * 3)
           COMPUTE COLT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3820-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PROMISED AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3830-FLAG-PROMISED.
           IF COLM-PROMISED-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLM-PROMISED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E154 ADDR REQUIRED
      * E756 WRITEOFF YTD NEGATIVE
      * E873 WRITEOFF YTD OVER 99999
      * E833 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-ADDR = SPACES
                   MOVE 'E154' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-YTD < 0
                   MOVE 'E756' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-YTD > 99999.00
                   MOVE 'E873' TO WS-REJECT-CODE
               WHEN COLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E833' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3850-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO COLM-PAST-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE COLM-PAST-DUE-AMT ROUNDED = WS-WORK-AMT-2 *
                       COLM-PENALTY-PCT / 200
               WHEN COLT-TRAN-CD = 'PR'
                   MOVE 5.00 TO COLM-PAST-DUE-AMT
               WHEN COLT-TRAN-CD = 'CL'
                   MOVE 15.00 TO COLM-PAST-DUE-AMT
               WHEN OTHER
                   COMPUTE COLM-PAST-DUE-AMT ROUNDED = WS-WORK-AMT-2 *
                       COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF COLM-PAST-DUE-AMT > 0 AND COLM-PAST-DUE-AMT < 2.00
               MOVE 2.00 TO COLM-PAST-DUE-AMT
           END-IF
           IF COLM-PAST-DUE-AMT > 100.00
               MOVE 100.00 TO COLM-PAST-DUE-AMT
           END-IF
           ADD COLM-PAST-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN EXPIRY DATE IS PAST
      * ER -> HA WHEN EXPIRY DATE IS PAST
      * HA -> HW WHEN EXPIRY DATE IS PAST
      * HW -> ME WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3860-SET-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ER' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HA' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HW' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE PAST DUE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3870-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO COLM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE COLM-FEE-AMT ROUNDED = COLM-PAST-DUE-YTD *
                       COLM-MARGIN / 200
               WHEN COLM-QUEUE-CD = 'DL'
                   MOVE 5.00 TO COLM-FEE-AMT
               WHEN COLM-QUEUE-CD = 'ER'
                   MOVE 15.00 TO COLM-FEE-AMT
               WHEN OTHER
                   COMPUTE COLM-FEE-AMT ROUNDED = COLM-PAST-DUE-YTD *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-FEE-AMT > 0 AND COLM-FEE-AMT < 5.00
               MOVE 5.00 TO COLM-FEE-AMT
           END-IF
           IF COLM-FEE-AMT > 75.00
               MOVE 75.00 TO COLM-FEE-AMT
           END-IF
           ADD COLM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3880-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 5
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-TRAN-CD
                   ADD COLM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3900-LIMIT-WRITEOFF.
           IF COLM-WRITEOFF-YTD < 25.00
               MOVE 25.00 TO COLM-WRITEOFF-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-WRITEOFF-YTD > 500.00
               MOVE 500.00 TO COLM-WRITEOFF-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3910-COUNT-NOTICE.
           IF COLM-NOTICE-CNT < 9999999
               ADD 1 TO COLM-NOTICE-CNT
           END-IF
           IF COLM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO COLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF COLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO COLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3930-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NOTICE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF AMT BY ACTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3940-ACCUM-ACTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-ACTION-CD
                   ADD COLM-WRITEOFF-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-ACTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-WRITEOFF-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3950-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-REVIEW-DATE > 0 AND COLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3960-DESCRIBE-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'AU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WRITEOFF AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3970-FLAG-WRITEOFF.
           IF COLM-WRITEOFF-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLM-WRITEOFF-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE FEE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3980-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO COLM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   COMPUTE COLM-FEE-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-MARGIN / 200
               WHEN COLM-QUEUE-CD = 'DL'
                   MOVE 7.00 TO COLM-FEE-AMT
               WHEN COLM-QUEUE-CD = 'ER'
                   MOVE 10.00 TO COLM-FEE-AMT
               WHEN OTHER
                   COMPUTE COLM-FEE-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-FEE-AMT > 0 AND COLM-FEE-AMT < 5.00
               MOVE 5.00 TO COLM-FEE-AMT
           END-IF
           IF COLM-FEE-AMT > 100.00
               MOVE 100.00 TO COLM-FEE-AMT
           END-IF
           ADD COLM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN NOTICE DATE IS PAST
      * BJ -> GG WHEN NOTICE DATE IS PAST
      * GG -> ZR WHEN NOTICE DATE IS PAST
      * ZR -> AT WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3990-SET-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZR' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WRITEOFF YTD.
      *----------------------------------------------------------------*
       4000-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-WRITEOFF-YTD ROUNDED =
                   COLM-PAST-DUE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-WRITEOFF-YTD
           END-IF
           ADD COLM-WRITEOFF-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF YTD BY ACTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-ACTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-ACTION-CD
                   ADD COLM-WRITEOFF-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-ACTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-WRITEOFF-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COLLECTED AMT.
      *----------------------------------------------------------------*
       4020-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-COLLECTED-AMT ROUNDED =
                   COLM-PROMISED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLT-COLLECTED-AMT
           END-IF
           ADD COLT-COLLECTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4030-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-REVIEW-DATE > 0 AND COLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4040-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-TRAN-CD
                   ADD COLT-WRITEOFF-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-WRITEOFF-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       4060-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-FEE-YTD ROUNDED =
                   COLT-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-FEE-YTD
           END-IF
           ADD COLM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4070-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4080-ACCRUE-FEE.
           IF COLT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-FEE-AMT * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLM-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY ACTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-ACTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-ACTION-CD
                   ADD COLM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-ACTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4100-LIMIT-COLLECTED.
           IF COLT-COLLECTED-AMT < 0.00
               MOVE 0.00 TO COLT-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-COLLECTED-AMT > 2500.00
               MOVE 2500.00 TO COLT-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COLLECTED YTD.
      *----------------------------------------------------------------*
       4110-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-COLLECTED-YTD ROUNDED =
                   COLM-WRITEOFF-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-COLLECTED-YTD
           END-IF
           ADD COLM-COLLECTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF GRID TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4120-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-GRID-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF COLM-PAST-DUE-AMT NOT < WS-WRITEOFF-GRID-LOW(WS-TX)
                   MOVE WS-WRITEOFF-GRID-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4130-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 10
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4140-FLAG-FEE.
           IF COLM-FEE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4150-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E641 CONTACT REQUIRED
      * E109 WORK AMT NEGATIVE
      * E329 WORK AMT OVER 10000
      * E648 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4160-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CONTACT = SPACES
                   MOVE 'E641' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E329' TO WS-REJECT-CODE
               WHEN COLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E648' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4170-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-GRID-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-WRITEOFF-GRID-LOW(WS-TX)
                   MOVE WS-WRITEOFF-GRID-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4180-COUNT-ITEM.
           IF COLM-ITEM-CNT < 9999999
               ADD 1 TO COLM-ITEM-CNT
           END-IF
           IF COLM-ITEM-CNT NOT < 6
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PROMISED AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4190-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = COLM-PROMISED-AMT *
                       COLM-FEE-PCT / 200
               WHEN COLT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN COLT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = COLM-PROMISED-AMT *
                       COLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAST DUE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4200-FLAG-PAST-DUE.
           IF COLT-PAST-DUE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLT-PAST-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PROMISED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4210-SPLIT-PROMISED.
           COMPUTE WS-WORK-AMT = COLM-PROMISED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = COLM-PROMISED-YTD - (WS-WORK-AMT * 2)
           COMPUTE COLT-COLLECTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AT -> AU WHEN LAST MAINT DATE IS PAST
      * AU -> XR WHEN LAST MAINT DATE IS PAST
      * XR -> XU WHEN LAST MAINT DATE IS PAST
      * XU -> AT WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4220-SET-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AU'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XR' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XR'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XU'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF GRID TIER FOR THE PROMISED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-GRID-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF COLM-PROMISED-AMT NOT < WS-WRITEOFF-GRID-LOW(WS-TX)
                   MOVE WS-WRITEOFF-GRID-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       4240-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF COLM-EFFECTIVE-DATE > 0 AND COLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4250-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-REVIEW-DATE > 0 AND COLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4260-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO COLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4270-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 12
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PROMISED TIER TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4280-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PROMISED-TIER-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF COLM-FEE-YTD NOT < WS-PROMISED-TIER-LOW(WS-TX)
                   MOVE WS-PROMISED-TIER-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * BE -> GV WHEN NEXT REVIEW DATE IS PAST
      * GV -> JM WHEN NEXT REVIEW DATE IS PAST
      * JM -> LC WHEN NEXT REVIEW DATE IS PAST
      * LC -> MK WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4290-SET-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GV' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JM' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LC' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LC'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MK' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4300-COUNT-CYCLE.
           IF COLM-CYCLE-CNT < 9999999
               ADD 1 TO COLM-CYCLE-CNT
           END-IF
           IF COLM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO COLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN NEXT REVIEW DATE IS PAST
      * BJ -> GG WHEN NEXT REVIEW DATE IS PAST
      * GG -> ZR WHEN NEXT REVIEW DATE IS PAST
      * ZR -> AT WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4310-SET-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BJ' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GG' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZR' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED YTD FEE ON THE PAST DUE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4320-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO COLM-COLLECTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   COMPUTE COLM-COLLECTED-YTD ROUNDED =
                       COLM-PAST-DUE-YTD * COLM-FEE-PCT / 200
               WHEN COLM-STATUS-CD = 'PS'
                   MOVE 3.00 TO COLM-COLLECTED-YTD
               WHEN COLM-STATUS-CD = 'UR'
                   MOVE 15.00 TO COLM-COLLECTED-YTD
               WHEN OTHER
                   COMPUTE COLM-COLLECTED-YTD ROUNDED =
                       COLM-PAST-DUE-YTD * COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-COLLECTED-YTD > 0 AND COLM-COLLECTED-YTD < 1.00
               MOVE 1.00 TO COLM-COLLECTED-YTD
           END-IF
           IF COLM-COLLECTED-YTD > 100.00
               MOVE 100.00 TO COLM-COLLECTED-YTD
           END-IF
           ADD COLM-COLLECTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4330-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 5
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF GRID TIER FOR THE PAST DUE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4340-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-GRID-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF COLM-PAST-DUE-YTD NOT < WS-WRITEOFF-GRID-LOW(WS-TX)
                   MOVE WS-WRITEOFF-GRID-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4350-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4360-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = COLM-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = COLM-FEE-AMT - (WS-WORK-AMT * 12)
           COMPUTE COLM-PROMISED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF GRID TIER FOR THE WRITEOFF AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4370-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-GRID-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF COLM-WRITEOFF-AMT NOT < WS-WRITEOFF-GRID-LOW(WS-TX)
                   MOVE WS-WRITEOFF-GRID-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PROMISED FEE ON THE FEE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4380-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN COLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO COLM-PROMISED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   COMPUTE COLM-PROMISED-AMT ROUNDED = COLT-FEE-AMT *
                       COLM-FEE-PCT / 200
               WHEN COLM-REASON-CD = 'AT'
                   MOVE 20.00 TO COLM-PROMISED-AMT
               WHEN COLM-REASON-CD = 'BJ'
                   MOVE 20.00 TO COLM-PROMISED-AMT
               WHEN OTHER
                   COMPUTE COLM-PROMISED-AMT ROUNDED = COLT-FEE-AMT *
                       COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-PROMISED-AMT > 0 AND COLM-PROMISED-AMT < 1.00
               MOVE 1.00 TO COLM-PROMISED-AMT
           END-IF
           IF COLM-PROMISED-AMT > 100.00
               MOVE 100.00 TO COLM-PROMISED-AMT
           END-IF
           ADD COLM-PROMISED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PROMISED AMT.
      *----------------------------------------------------------------*
       4390-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-PROMISED-AMT ROUNDED =
                   COLM-PROMISED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-PROMISED-AMT
           END-IF
           ADD COLM-PROMISED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAST DUE YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4400-LIMIT-PAST-DUE.
           IF COLM-PAST-DUE-YTD < 10.00
               MOVE 10.00 TO COLM-PAST-DUE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PAST-DUE-YTD > 1000.00
               MOVE 1000.00 TO COLM-PAST-DUE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4410-FLAG-PAST-DUE.
           IF COLM-PAST-DUE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLM-PAST-DUE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * BE -> GV WHEN LAST PAY DATE IS PAST
      * GV -> JM WHEN LAST PAY DATE IS PAST
      * JM -> LC WHEN LAST PAY DATE IS PAST
      * LC -> MK WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4420-SET-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LC' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LC'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MK' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF GRID TIER FOR THE WRITEOFF YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4430-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-GRID-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF COLM-WRITEOFF-YTD NOT < WS-WRITEOFF-GRID-LOW(WS-TX)
                   MOVE WS-WRITEOFF-GRID-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF GRID TIER FOR THE WRITEOFF AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4440-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-GRID-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF COLT-WRITEOFF-AMT NOT < WS-WRITEOFF-GRID-LOW(WS-TX)
                   MOVE WS-WRITEOFF-GRID-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4450-DESCRIBE-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE WRITEOFF AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4460-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN COLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO COLT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE COLT-FEE-AMT ROUNDED = COLM-WRITEOFF-AMT *
                       COLM-PENALTY-PCT / 200
               WHEN COLM-ACTION-CD = 'KJ'
                   MOVE 20.00 TO COLT-FEE-AMT
               WHEN COLM-ACTION-CD = 'TM'
                   MOVE 20.00 TO COLT-FEE-AMT
               WHEN OTHER
                   COMPUTE COLT-FEE-AMT ROUNDED = COLM-WRITEOFF-AMT *
                       COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF COLT-FEE-AMT > 0 AND COLT-FEE-AMT < 1.00
               MOVE 1.00 TO COLT-FEE-AMT
           END-IF
           IF COLT-FEE-AMT > 250.00
               MOVE 250.00 TO COLT-FEE-AMT
           END-IF
           ADD COLT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4470-LIMIT-FEE.
           IF COLT-FEE-AMT < 25.00
               MOVE 25.00 TO COLT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-FEE-AMT > 2500.00
               MOVE 2500.00 TO COLT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4480-FLAG-WRITEOFF.
           IF COLM-WRITEOFF-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLM-WRITEOFF-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF GRID TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4490-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-GRID-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF COLM-PAST-DUE-AMT NOT < WS-WRITEOFF-GRID-LOW(WS-TX)
                   MOVE WS-WRITEOFF-GRID-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4500-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF COLT-POST-DATE > 0 AND COLT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4510-ACCRUE-PAST-DUE.
           IF COLM-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PAST-DUE-AMT * COLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO COLM-WRITEOFF-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4520-LIMIT-WRITEOFF.
           IF COLT-WRITEOFF-AMT < 25.00
               MOVE 25.00 TO COLT-WRITEOFF-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-WRITEOFF-AMT > 1000.00
               MOVE 1000.00 TO COLT-WRITEOFF-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       4530-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-FEE-AMT ROUNDED =
                   COLM-PAST-DUE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLT-FEE-AMT
           END-IF
           ADD COLT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4540-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF COLM-EFFECTIVE-DATE > 0 AND COLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4550-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLM-COLLECTED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = COLM-COLLECTED-AMT - (WS-WORK-AMT *
               12)
           COMPUTE COLM-COLLECTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4560-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * COLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLM-COLLECTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PROMISED AMT.
      *----------------------------------------------------------------*
       4570-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-PROMISED-AMT ROUNDED =
                   COLM-WRITEOFF-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-PROMISED-AMT
           END-IF
           ADD COLM-PROMISED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4580-DESCRIBE-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAST DUE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4590-FLAG-PAST-DUE.
           IF COLT-PAST-DUE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLT-PAST-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAST DUE AMT.
      *----------------------------------------------------------------*
       4600-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-PAST-DUE-AMT ROUNDED =
                   COLM-COLLECTED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLT-PAST-DUE-AMT
           END-IF
           ADD COLT-PAST-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4610-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLM-COLLECTED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = COLM-COLLECTED-YTD - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAST DUE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4620-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = COLT-PAST-DUE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = COLT-PAST-DUE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE COLM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4630-FLAG-COLLECTED.
           IF COLM-COLLECTED-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLM-COLLECTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PROMISED AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4640-LIMIT-PROMISED.
           IF COLM-PROMISED-AMT < 25.00
               MOVE 25.00 TO COLM-PROMISED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PROMISED-AMT > 1000.00
               MOVE 1000.00 TO COLM-PROMISED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4650-DESCRIBE-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WO'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4660-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-QUEUE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4670-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 6
               MOVE 'Y' TO COLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4680-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4690-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF COLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO COLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4700-LIMIT-WRITEOFF.
           IF COLT-WRITEOFF-AMT < 0.00
               MOVE 0.00 TO COLT-WRITEOFF-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-WRITEOFF-AMT > 5000.00
               MOVE 5000.00 TO COLT-WRITEOFF-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4710-ACCRUE-COLLECTED.
           IF COLM-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-COLLECTED-AMT * COLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4720-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 12
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4730-LIMIT-PAST-DUE.
           IF COLT-PAST-DUE-AMT < 100.00
               MOVE 100.00 TO COLT-PAST-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-PAST-DUE-AMT > 2500.00
               MOVE 2500.00 TO COLT-PAST-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PROMISED TIER TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4740-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PROMISED-TIER-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF COLT-PAST-DUE-AMT NOT < WS-PROMISED-TIER-LOW(WS-TX)
                   MOVE WS-PROMISED-TIER-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ACTION CD:
      * KJ -> TM WHEN CLOSE DATE IS PAST
      * TM -> VL WHEN CLOSE DATE IS PAST
      * VL -> ZS WHEN CLOSE DATE IS PAST
      * ZS -> KJ WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4750-SET-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TM'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VL' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VL'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZS' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZS'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJ' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4760-COUNT-CYCLE.
           IF COLM-CYCLE-CNT < 9999999
               ADD 1 TO COLM-CYCLE-CNT
           END-IF
           IF COLM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4770-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 10
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E650 REF NO REQUIRED
      * E423 WRITEOFF YTD NEGATIVE
      * E102 WRITEOFF YTD OVER 99999
      * E146 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4780-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-REF-NO = SPACES
                   MOVE 'E650' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-YTD < 0
                   MOVE 'E423' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-YTD > 99999.00
                   MOVE 'E102' TO WS-REJECT-CODE
               WHEN COLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E146' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED FEE ON THE WRITEOFF AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4790-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO COLT-COLLECTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   COMPUTE COLT-COLLECTED-AMT ROUNDED =
                       COLM-WRITEOFF-AMT * COLM-PENALTY-PCT / 200
               WHEN COLT-TRAN-CD = 'PR'
                   MOVE 12.00 TO COLT-COLLECTED-AMT
               WHEN COLT-TRAN-CD = 'CL'
                   MOVE 10.00 TO COLT-COLLECTED-AMT
               WHEN OTHER
                   COMPUTE COLT-COLLECTED-AMT ROUNDED =
                       COLM-WRITEOFF-AMT * COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF COLT-COLLECTED-AMT > 0 AND COLT-COLLECTED-AMT < 5.00
               MOVE 5.00 TO COLT-COLLECTED-AMT
           END-IF
           IF COLT-COLLECTED-AMT > 150.00
               MOVE 150.00 TO COLT-COLLECTED-AMT
           END-IF
           ADD COLT-COLLECTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4800-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E293 DESC REQUIRED
      * E187 COLLECTED YTD NEGATIVE
      * E429 COLLECTED YTD OVER 99999
      * E482 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4810-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-DESC = SPACES
                   MOVE 'E293' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-YTD < 0
                   MOVE 'E187' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-YTD > 99999.00
                   MOVE 'E429' TO WS-REJECT-CODE
               WHEN COLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4820-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE COLM-WRITEOFF-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E708 REF NO REQUIRED
      * E133 COLLECTED AMT NEGATIVE
      * E815 COLLECTED AMT OVER 50000
      * E925 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4830-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-REF-NO = SPACES
                   MOVE 'E708' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-AMT < 0
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-AMT > 50000.00
                   MOVE 'E815' TO WS-REJECT-CODE
               WHEN COLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E925' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4840-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = COLT-WRITEOFF-AMT / 4
           COMPUTE WS-WORK-AMT-2 = COLT-WRITEOFF-AMT - (WS-WORK-AMT * 4)
           COMPUTE COLT-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4850-DESCRIBE-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'AU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COLLECTED AMT.
      *----------------------------------------------------------------*
       4860-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-COLLECTED-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-COLLECTED-AMT
           END-IF
           ADD COLM-COLLECTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4870-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLM-COLLECTED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = COLM-COLLECTED-AMT - (WS-WORK-AMT *
               2)
           COMPUTE COLT-COLLECTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       4880-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE COL FIELDS TO CLTIER AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-CLTIER.
           INITIALIZE CLTIER-PARMS
           MOVE COLM-COLLECTED-YTD
             TO CLTIER-COLLECTED-YTD
           MOVE COLM-PAST-DUE-YTD
             TO CLTIER-PAST-DUE-YTD
           MOVE COLM-PROMISED-AMT
             TO CLTIER-PROMISED-AMT
           MOVE COLM-PAST-DUE-AMT
             TO CLTIER-PAST-DUE-AMT
           MOVE COLM-COLLECTED-AMT
             TO CLTIER-COLLECTED-AMT
           MOVE COLM-PROMISED-YTD
             TO CLTIER-PROMISED-YTD
           MOVE COLM-NEXT-DUE-DATE
             TO CLTIER-NEXT-DUE-DATE
           MOVE COLM-OPEN-DATE
             TO CLTIER-OPEN-DATE
           MOVE COLM-LAST-PAY-DATE
             TO CLTIER-LAST-PAY-DATE
           MOVE COLM-CLOSE-DATE
             TO CLTIER-CLOSE-DATE
           MOVE COLM-STATUS-CD
             TO CLTIER-STATUS-CD
           MOVE COLM-REASON-CD
             TO CLTIER-REASON-CD
           MOVE COLM-ACTION-CD
             TO CLTIER-ACTION-CD
           MOVE COLM-CYCLE-CD
             TO CLTIER-CYCLE-CD
           MOVE COLM-EXEMPT-FLAG
             TO CLTIER-EXEMPT-FLAG
           MOVE COLM-SUSPEND-FLAG
             TO CLTIER-SUSPEND-FLAG
           MOVE COLM-OVERRIDE-FLAG
             TO CLTIER-OVERRIDE-FLAG
           MOVE COLM-CYCLE-CNT
             TO CLTIER-CYCLE-CNT
           MOVE COLM-RETRY-CNT
             TO CLTIER-RETRY-CNT
           MOVE COLM-FEE-PCT
             TO CLTIER-FEE-PCT
           MOVE COLM-MARGIN
             TO CLTIER-MARGIN
           MOVE COLM-CONTACT
             TO CLTIER-CONTACT
           MOVE COLM-CUST-ID
             TO CLTIER-CUST-ID
           CALL 'CLTIER' USING CLTIER-PARMS
           EVALUATE CLTIER-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CLTIER-PROMISED-AMT
                     TO COLM-PROMISED-AMT
                   MOVE CLTIER-PAST-DUE-AMT
                     TO COLM-PAST-DUE-AMT
                   MOVE CLTIER-COLLECTED-AMT
                     TO COLM-COLLECTED-AMT
                   MOVE CLTIER-OVERRIDE-FLAG
                     TO COLM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RIER' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-CLTIER' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE COL FIELDS TO CLLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-CLLIM.
           INITIALIZE CLLIM-PARMS
           MOVE COLM-COLLECTED-YTD
             TO CLLIM-COLLECTED-YTD
           MOVE COLM-COLLECTED-AMT
             TO CLLIM-COLLECTED-AMT
           MOVE COLM-PAST-DUE-AMT
             TO CLLIM-PAST-DUE-AMT
           MOVE COLM-PROMISED-YTD
             TO CLLIM-PROMISED-YTD
           MOVE COLM-WRITEOFF-YTD
             TO CLLIM-WRITEOFF-YTD
           MOVE COLM-FEE-YTD
             TO CLLIM-FEE-YTD
           MOVE COLM-CLOSE-DATE
             TO CLLIM-CLOSE-DATE
           MOVE COLM-NOTICE-DATE
             TO CLLIM-NOTICE-DATE
           MOVE COLM-OPEN-DATE
             TO CLLIM-OPEN-DATE
           MOVE COLM-LAST-PAY-DATE
             TO CLLIM-LAST-PAY-DATE
           MOVE COLM-CYCLE-CD
             TO CLLIM-CYCLE-CD
           MOVE COLM-REASON-CD
             TO CLLIM-REASON-CD
           MOVE COLM-QUEUE-CD
             TO CLLIM-QUEUE-CD
           MOVE COLM-REGION-CD
             TO CLLIM-REGION-CD
           MOVE COLM-REVIEW-FLAG
             TO CLLIM-REVIEW-FLAG
           MOVE COLM-OVERRIDE-FLAG
             TO CLLIM-OVERRIDE-FLAG
           MOVE COLM-PREFERRED-FLAG
             TO CLLIM-PREFERRED-FLAG
           MOVE COLM-ITEM-CNT
             TO CLLIM-ITEM-CNT
           MOVE COLM-NOTICE-CNT
             TO CLLIM-NOTICE-CNT
           MOVE COLM-FEE-PCT
             TO CLLIM-FEE-PCT
           MOVE COLM-MARGIN
             TO CLLIM-MARGIN
           MOVE COLM-CONTACT
             TO CLLIM-CONTACT
           MOVE COLM-NAME
             TO CLLIM-NAME
           CALL 'CLLIM' USING CLLIM-PARMS
           EVALUATE CLLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CLLIM-COLLECTED-YTD
                     TO COLM-COLLECTED-YTD
                   MOVE CLLIM-COLLECTED-AMT
                     TO COLM-COLLECTED-AMT
                   MOVE CLLIM-WRITEOFF-YTD
                     TO COLM-WRITEOFF-YTD
                   MOVE CLLIM-REVIEW-FLAG
                     TO COLM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-CLLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE COL FIELDS TO CLRISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-CLRISK.
           INITIALIZE CLRISK-PARMS
           MOVE COLM-PAST-DUE-AMT
             TO CLRISK-PAST-DUE-AMT
           MOVE COLM-PAST-DUE-YTD
             TO CLRISK-PAST-DUE-YTD
           MOVE COLM-COLLECTED-AMT
             TO CLRISK-COLLECTED-AMT
           MOVE COLM-WRITEOFF-YTD
             TO CLRISK-WRITEOFF-YTD
           MOVE COLM-FEE-YTD
             TO CLRISK-FEE-YTD
           MOVE COLM-PROMISED-YTD
             TO CLRISK-PROMISED-YTD
           MOVE COLM-EFFECTIVE-DATE
             TO CLRISK-EFFECTIVE-DATE
           MOVE COLM-OPEN-DATE
             TO CLRISK-OPEN-DATE
           MOVE COLM-LAST-MAINT-DATE
             TO CLRISK-LAST-MAINT-DATE
           MOVE COLM-NOTICE-DATE
             TO CLRISK-NOTICE-DATE
           MOVE COLM-CYCLE-CD
             TO CLRISK-CYCLE-CD
           MOVE COLM-REGION-CD
             TO CLRISK-REGION-CD
           MOVE COLM-ACTION-CD
             TO CLRISK-ACTION-CD
           MOVE COLM-STATUS-CD
             TO CLRISK-STATUS-CD
           MOVE COLM-NOTICE-SENT-FLAG
             TO CLRISK-NOTICE-SENT-FLAG
           MOVE COLM-REVIEW-FLAG
             TO CLRISK-REVIEW-FLAG
           MOVE COLM-EXEMPT-FLAG
             TO CLRISK-EXEMPT-FLAG
           MOVE COLM-ITEM-CNT
             TO CLRISK-ITEM-CNT
           MOVE COLM-RETRY-CNT
             TO CLRISK-RETRY-CNT
           MOVE COLM-FEE-PCT
             TO CLRISK-FEE-PCT
           MOVE COLM-MARGIN
             TO CLRISK-MARGIN
           MOVE COLM-DESC
             TO CLRISK-DESC
           MOVE COLM-CONTACT
             TO CLRISK-CONTACT
           CALL 'CLRISK' USING CLRISK-PARMS
           EVALUATE CLRISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CLRISK-COLLECTED-AMT
                     TO COLM-COLLECTED-AMT
                   MOVE CLRISK-WRITEOFF-YTD
                     TO COLM-WRITEOFF-YTD
                   MOVE CLRISK-PROMISED-YTD
                     TO COLM-PROMISED-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-CLRISK' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE COLT-KEY TO RD-KEY
           MOVE COLT-TRAN-CD TO RD-CODE
           MOVE COLT-COLLECTED-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE COLT-KEY TO RD-KEY
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
           DISPLAY 'CLLPOST READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CLLPOST' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
