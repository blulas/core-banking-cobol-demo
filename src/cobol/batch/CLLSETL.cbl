      *================================================================*
      * PROGRAM  : CLLSETL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : COLLECTIONS QUEUE - SETTLEMENT. READS THE COL       *
      *            TRANSACTIONS, UPDATES THE COL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. DELINQUENT ACCOUNT COLLECTION   *
      *            QUEUES, PROMISES AND LETTERS                        *
      * INPUT    : COLMST (KSDS), COLTRN, BANKCTL                      *
      * OUTPUT   : COLOUT, CLLSERPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CLTIER, CLLIM, CLRISK   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-02-27 SKN  ORIGINAL                                       *
      * 1998-10-25 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-10-15 GAF  FEE NOTICE TEXT PER LEGAL PRB9650              *
      * 2003-02-06 LMB  FEE EXCEPTIONS TO OPS QUEUE INC0738            *
      * 2004-11-08 PQR  PROMISED RERUN PROTECTION INC8989              *
      * 2004-08-22 TWH  FEE CALCULATION CORRECTED PRB3654              *
      * 2004-04-02 GAF  COLLECTED EXCEPTIONS TO OPS QUEUE REQ8157      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLLSETL.
       AUTHOR. B H SANDERS.

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
           SELECT REPORT-FILE ASSIGN TO "CLLSERPT"
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
                                           VALUE 9.000000.
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
      * WS-WRITEOFF-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WRITEOFF-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000322500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000542000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000326000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000584000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000363000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008500000000664000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018500000000547000'.
       01  WS-WRITEOFF-BAND-TBL REDEFINES WS-WRITEOFF-BAND-VALUES.
           05  WS-WRITEOFF-BAND-ROW OCCURS 7 TIMES.
               10  WS-WRITEOFF-BAND-LOW    PIC 9(09)V99.
               10  WS-WRITEOFF-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COLLECTE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COLLECTE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000352500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000231500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000330500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000535500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000452500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000116500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000383000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000222500'.
       01  WS-COLLECTE-TIER-TBL REDEFINES WS-COLLECTE-TIER-VALUES.
           05  WS-COLLECTE-TIER-ROW OCCURS 8 TIMES.
               10  WS-COLLECTE-TIER-LOW    PIC 9(09)V99.
               10  WS-COLLECTE-TIER-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CLLSETL  COLLECTIONS QUEUE'.
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
               PERFORM 3000-ACCRUE-PAST-DUE
               PERFORM 3010-ACCUM-REGION
               PERFORM 3020-ACCRUE-COLLECTED
               PERFORM 3030-PRORATE-FEE
               PERFORM 3040-ACCUM-TRAN
               PERFORM 3050-AGE-EXPIRY
               PERFORM 3060-LIMIT-WRITEOFF
               PERFORM 3070-ACCUM-CYCLE
               PERFORM 3080-LIMIT-COLLECTED
               PERFORM 3090-EDIT-ADDR
               PERFORM 3100-LIMIT-FEE
               PERFORM 3110-SET-REGION
               PERFORM 3120-FLAG-ITEM
               PERFORM 3130-LIMIT-COLLECTED
               PERFORM 3140-SET-STATUS
               PERFORM 3150-ACCUM-REASON
               PERFORM 3160-SELECT-MARGIN
               PERFORM 3170-ASSESS-REVERSAL
               PERFORM 3180-AGE-LAST-PAY
               PERFORM 3190-CHECK-EFFECTIVE
               PERFORM 3200-SELECT-FEE-PCT
               PERFORM 3210-COUNT-RETRY
               PERFORM 3220-PRORATE-COLLECTED
               PERFORM 3230-PRORATE-WRITEOFF
               PERFORM 3240-LIMIT-WRITEOFF
               PERFORM 3250-COUNT-ITEM
               PERFORM 3260-EDIT-MEMO
               PERFORM 3270-CHECK-EFFECTIVE
               PERFORM 3280-ACCRUE-WRITEOFF
               PERFORM 3290-COUNT-RETRY
               PERFORM 3300-EDIT-MEMO
               PERFORM 3310-EDIT-DESC
               PERFORM 3320-CHECK-POST
               PERFORM 3330-SET-REASON
               PERFORM 3340-CHECK-EFFECTIVE
               PERFORM 3350-EDIT-DESC
               PERFORM 3360-PRORATE-PAST-DUE
               PERFORM 3370-AGE-LAST-MAINT
               PERFORM 3380-AGE-EFFECTIVE
               PERFORM 3390-LIMIT-COLLECTED
               PERFORM 3400-FLAG-WRITEOFF
               PERFORM 3410-SET-QUEUE
               PERFORM 3420-FLAG-PROMISED
               PERFORM 3430-SELECT-PENALTY-PCT
               PERFORM 3440-LIMIT-ITEM
               PERFORM 3450-DESCRIBE-TRAN
               PERFORM 3460-SET-CHANNEL
               PERFORM 3470-ACCUM-ACTION
               PERFORM 3480-CHECK-NEXT-DUE
               PERFORM 3490-ACCRUE-PAST-DUE
               PERFORM 3500-ASSESS-OVERRIDE
               PERFORM 3510-ACCUM-QUEUE
               PERFORM 3520-CHECK-POST
               PERFORM 3530-SPLIT-COLLECTED
               PERFORM 3540-FLAG-PAST-DUE
               PERFORM 3550-FLAG-FEE
               PERFORM 3560-EDIT-NAME
               PERFORM 3570-PRORATE-FEE
               PERFORM 3580-COUNT-RETRY
               PERFORM 3590-COUNT-CYCLE
               PERFORM 3600-SET-REGION
               PERFORM 3610-DESCRIBE-ACTION
               PERFORM 3620-CHECK-NEXT-DUE
               PERFORM 3630-ASSESS-SUSPEND
               PERFORM 3640-ACCRUE-PROMISED
               PERFORM 3650-LIMIT-PAST-DUE
               PERFORM 3660-DESCRIBE-CHANNEL
               PERFORM 3670-DESCRIBE-REASON
               PERFORM 3680-LIMIT-COLLECTED
               PERFORM 3690-SELECT-MARGIN
               PERFORM 3700-PRORATE-COLLECTED
               PERFORM 3710-SPLIT-PROMISED
               PERFORM 3720-EDIT-OPERATOR
               PERFORM 3730-PRORATE-PAST-DUE
               PERFORM 3740-DESCRIBE-QUEUE
               PERFORM 3750-ACCRUE-PROMISED
               PERFORM 3760-ASSESS-EXEMPT
               PERFORM 3770-SELECT-PENALTY-PCT
               PERFORM 3780-COUNT-CYCLE
               PERFORM 3790-SET-TRAN
               PERFORM 3800-EDIT-MEMO
               PERFORM 3810-FLAG-PAST-DUE
               PERFORM 3820-PRORATE-COLLECTED
               PERFORM 3830-ASSESS-EXEMPT
               PERFORM 3840-EDIT-MEMO
               PERFORM 3850-DESCRIBE-REASON
               PERFORM 3860-SELECT-MARGIN
               PERFORM 3870-COUNT-CYCLE
               PERFORM 3880-COUNT-SEQ
               PERFORM 3890-DESCRIBE-STATUS
               PERFORM 3900-EDIT-NAME
               PERFORM 3910-EDIT-ADDR
               PERFORM 3920-DESCRIBE-REGION
               PERFORM 3930-PRORATE-FEE
               PERFORM 3940-LIMIT-FEE
               PERFORM 3950-CHECK-NOTICE
               PERFORM 3960-SPLIT-PAST-DUE
               PERFORM 3970-FLAG-WRITEOFF
               PERFORM 3980-COUNT-CYCLE
               PERFORM 3990-ACCRUE-COLLECTED
               PERFORM 4000-ACCUM-REGION
               PERFORM 4010-PRORATE-FEE
               PERFORM 4020-SET-REASON
               PERFORM 4030-PRORATE-PAST-DUE
               PERFORM 4040-COUNT-ITEM
               PERFORM 4050-AGE-NOTICE
               PERFORM 4060-SET-CYCLE
               PERFORM 4070-ACCRUE-WRITEOFF
               PERFORM 4080-SET-REGION
               PERFORM 4090-ACCRUE-WRITEOFF
               PERFORM 4100-COUNT-RETRY
               PERFORM 4110-FLAG-WRITEOFF
               PERFORM 4120-PRORATE-ITEM
               PERFORM 4130-ASSESS-NOTICE-SENT
               PERFORM 4140-SPLIT-COLLECTED
               PERFORM 4150-ACCUM-STATUS
               PERFORM 4160-EDIT-ADDR
               PERFORM 4170-COUNT-NOTICE
               PERFORM 4180-ACCUM-REGION
               PERFORM 4190-CHECK-NEXT-REVIEW
               PERFORM 4200-DESCRIBE-CYCLE
               PERFORM 4210-ACCUM-TRAN
               PERFORM 4220-CHECK-EXPIRY
               PERFORM 4230-SPLIT-FEE
               PERFORM 4240-LIMIT-COLLECTED
               PERFORM 4250-ACCUM-REGION
               PERFORM 4260-CHECK-EFFECTIVE
               PERFORM 4270-ACCUM-CHANNEL
               PERFORM 4280-LIMIT-COLLECTED
               PERFORM 4290-ACCRUE-FEE
               PERFORM 4300-ASSESS-SUSPEND
               PERFORM 4310-ACCUM-CHANNEL
               PERFORM 4320-AGE-EFF
               PERFORM 4330-ACCUM-CYCLE
               PERFORM 4340-COUNT-SEQ
               PERFORM 4350-CHECK-NEXT-DUE
               PERFORM 4360-ASSESS-REVERSAL
               PERFORM 4370-EDIT-CONTACT
               PERFORM 4380-FLAG-FEE
               PERFORM 4390-COUNT-CYCLE
               PERFORM 4400-COUNT-NOTICE
               PERFORM 4410-CHECK-EFF
               PERFORM 4420-LIMIT-COLLECTED
               PERFORM 4430-AGE-LAST-PAY
               PERFORM 4440-ACCUM-QUEUE
               PERFORM 4450-AGE-EFFECTIVE
               PERFORM 4460-DESCRIBE-TRAN
               PERFORM 4470-SET-REGION
               PERFORM 4480-ASSESS-REVIEW
               PERFORM 4490-DESCRIBE-REGION
               PERFORM 4500-DESCRIBE-TRAN
               PERFORM 4510-CHECK-NEXT-REVIEW
               PERFORM 4520-EDIT-CUST-ID
               PERFORM 4530-SPLIT-COLLECTED
               PERFORM 4540-PRORATE-PROMISED
               PERFORM 4550-DESCRIBE-CHANNEL
               PERFORM 4560-SPLIT-COLLECTED
               PERFORM 4570-SET-QUEUE
               PERFORM 4580-CHECK-CLOSE
               PERFORM 4590-DESCRIBE-TRAN
               PERFORM 4600-COUNT-SEQ
               PERFORM 4610-EDIT-DESC
               PERFORM 4620-COUNT-ITEM
               PERFORM 4630-COUNT-NOTICE
               PERFORM 4640-EDIT-MEMO
               PERFORM 4650-PRORATE-FEE
               PERFORM 4660-SET-STATUS
               PERFORM 4670-FLAG-PAST-DUE
               PERFORM 4680-AGE-NEXT-DUE
               PERFORM 4690-CHECK-OPEN
               PERFORM 4700-ACCRUE-FEE
               PERFORM 4710-CHECK-EFFECTIVE
               PERFORM 4720-LIMIT-PAST-DUE
               PERFORM 4730-PRORATE-COLLECTED
               PERFORM 4740-COUNT-RETRY
               PERFORM 4750-LIMIT-FEE
               PERFORM 4760-ASSESS-SUSPEND
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
      * DAILY INTEREST ON PAST DUE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3000-ACCRUE-PAST-DUE.
           IF COLT-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-PAST-DUE-AMT * COLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PROMISED YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REGION-CD
                   ADD COLM-PROMISED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-PROMISED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3020-ACCRUE-COLLECTED.
           IF COLM-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-COLLECTED-AMT * COLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLT-PAST-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3030-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-FEE-YTD ROUNDED =
                   COLM-PROMISED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-FEE-YTD
           END-IF
           ADD COLM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE PROMISED YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3040-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-TRAN-CD
                   ADD COLM-PROMISED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-PROMISED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3050-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EXPIRY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-WRITEOFF-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WRITEOFF YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3060-LIMIT-WRITEOFF.
           IF COLM-WRITEOFF-YTD < 25.00
               MOVE 25.00 TO COLM-WRITEOFF-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-WRITEOFF-YTD > 500.00
               MOVE 500.00 TO COLM-WRITEOFF-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-CYCLE-CD
                   ADD COLT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3080-LIMIT-COLLECTED.
           IF COLM-COLLECTED-YTD < 25.00
               MOVE 25.00 TO COLM-COLLECTED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-COLLECTED-YTD > 5000.00
               MOVE 5000.00 TO COLM-COLLECTED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E979 ADDR REQUIRED
      * E651 WRITEOFF AMT NEGATIVE
      * E714 WRITEOFF AMT OVER 5000
      * E442 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-ADDR = SPACES
                   MOVE 'E979' TO WS-REJECT-CODE
               WHEN COLT-WRITEOFF-AMT < 0
                   MOVE 'E651' TO WS-REJECT-CODE
               WHEN COLT-WRITEOFF-AMT > 5000.00
                   MOVE 'E714' TO WS-REJECT-CODE
               WHEN COLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E442' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3100-LIMIT-FEE.
           IF COLT-FEE-AMT < 100.00
               MOVE 100.00 TO COLT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-FEE-AMT > 50000.00
               MOVE 50000.00 TO COLT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AT -> AU WHEN POST DATE IS PAST
      * AU -> XR WHEN POST DATE IS PAST
      * XR -> XU WHEN POST DATE IS PAST
      * XU -> AT WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AU'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XR' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XR'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XU'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3120-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3130-LIMIT-COLLECTED.
           IF COLM-COLLECTED-AMT < 10.00
               MOVE 10.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-COLLECTED-AMT > 1000.00
               MOVE 1000.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN NEXT REVIEW DATE IS PAST
      * UR -> XM WHEN NEXT REVIEW DATE IS PAST
      * XM -> YA WHEN NEXT REVIEW DATE IS PAST
      * YA -> PS WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3140-SET-STATUS.
           EVALUATE COLM-STATUS-CD
               WHEN 'PS'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UR' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XM' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'YA' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PS' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3150-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REASON-CD
                   ADD COLM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF BAND TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3160-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-BAND-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF COLM-PAST-DUE-AMT NOT < WS-WRITEOFF-BAND-LOW(WS-TX)
                   MOVE WS-WRITEOFF-BAND-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED FEE ON THE FEE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3170-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO COLM-COLLECTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   COMPUTE COLM-COLLECTED-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-MARGIN / 200
               WHEN COLM-QUEUE-CD = 'DL'
                   MOVE 20.00 TO COLM-COLLECTED-AMT
               WHEN COLM-QUEUE-CD = 'ER'
                   MOVE 7.00 TO COLM-COLLECTED-AMT
               WHEN OTHER
                   COMPUTE COLM-COLLECTED-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-COLLECTED-AMT > 0 AND COLM-COLLECTED-AMT < 2.00
               MOVE 2.00 TO COLM-COLLECTED-AMT
           END-IF
           IF COLM-COLLECTED-AMT > 50.00
               MOVE 50.00 TO COLM-COLLECTED-AMT
           END-IF
           ADD COLM-COLLECTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-LAST-PAY.
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
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF COLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WRITEOFF BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WRITEOFF-BAND-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF COLM-FEE-AMT NOT < WS-WRITEOFF-BAND-LOW(WS-TX)
                   MOVE WS-WRITEOFF-BAND-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 10
               MOVE 'Y' TO COLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COLLECTED AMT.
      *----------------------------------------------------------------*
       3220-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-COLLECTED-AMT ROUNDED =
                   COLM-PROMISED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLT-COLLECTED-AMT
           END-IF
           ADD COLT-COLLECTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WRITEOFF AMT.
      *----------------------------------------------------------------*
       3230-PRORATE-WRITEOFF.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-WRITEOFF-AMT ROUNDED =
                   COLM-COLLECTED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLT-WRITEOFF-AMT
           END-IF
           ADD COLT-WRITEOFF-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WRITEOFF YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3240-LIMIT-WRITEOFF.
           IF COLM-WRITEOFF-YTD < 50.00
               MOVE 50.00 TO COLM-WRITEOFF-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-WRITEOFF-YTD > 1000.00
               MOVE 1000.00 TO COLM-WRITEOFF-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3250-COUNT-ITEM.
           IF COLM-ITEM-CNT < 9999999
               ADD 1 TO COLM-ITEM-CNT
           END-IF
           IF COLM-ITEM-CNT NOT < 3
               MOVE 'Y' TO COLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E807 MEMO REQUIRED
      * E740 COLLECTED AMT NEGATIVE
      * E067 COLLECTED AMT OVER 10000
      * E107 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-MEMO = SPACES
                   MOVE 'E807' TO WS-REJECT-CODE
               WHEN COLT-COLLECTED-AMT < 0
                   MOVE 'E740' TO WS-REJECT-CODE
               WHEN COLT-COLLECTED-AMT > 10000.00
                   MOVE 'E067' TO WS-REJECT-CODE
               WHEN COLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E107' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3270-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF COLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO COLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WRITEOFF YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-WRITEOFF.
           IF COLM-WRITEOFF-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-WRITEOFF-YTD * COLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO COLT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 3
               MOVE 'Y' TO COLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E619 MEMO REQUIRED
      * E573 PROMISED YTD NEGATIVE
      * E847 PROMISED YTD OVER 5000
      * E766 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3300-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-MEMO = SPACES
                   MOVE 'E619' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-YTD < 0
                   MOVE 'E573' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-YTD > 5000.00
                   MOVE 'E847' TO WS-REJECT-CODE
               WHEN COLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E766' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E470 DESC REQUIRED
      * E022 PROMISED AMT NEGATIVE
      * E821 PROMISED AMT OVER 250000
      * E212 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3310-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-DESC = SPACES
                   MOVE 'E470' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT < 0
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT > 250000.00
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN COLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E212' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3320-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF COLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO COLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN EFF DATE IS PAST
      * BJ -> GG WHEN EFF DATE IS PAST
      * GG -> ZR WHEN EFF DATE IS PAST
      * ZR -> AT WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3330-SET-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZR' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF COLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO COLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E321 DESC REQUIRED
      * E930 PROMISED YTD NEGATIVE
      * E149 PROMISED YTD OVER 250000
      * E470 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-DESC = SPACES
                   MOVE 'E321' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-YTD < 0
                   MOVE 'E930' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-YTD > 250000.00
                   MOVE 'E149' TO WS-REJECT-CODE
               WHEN COLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E470' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAST DUE AMT.
      *----------------------------------------------------------------*
       3360-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-PAST-DUE-AMT ROUNDED =
                   COLM-PROMISED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-PAST-DUE-AMT
           END-IF
           ADD COLM-PAST-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-LAST-MAINT.
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
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3380-AGE-EFFECTIVE.
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
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3390-LIMIT-COLLECTED.
           IF COLM-COLLECTED-AMT < 0.00
               MOVE 0.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-COLLECTED-AMT > 500.00
               MOVE 500.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3400-FLAG-WRITEOFF.
           IF COLT-WRITEOFF-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLT-WRITEOFF-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN CLOSE DATE IS PAST
      * ER -> HA WHEN CLOSE DATE IS PAST
      * HA -> HW WHEN CLOSE DATE IS PAST
      * HW -> ME WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3410-SET-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ER' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HA' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HW' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PROMISED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3420-FLAG-PROMISED.
           IF COLM-PROMISED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLM-PROMISED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE TIER TIER FOR THE COLLECTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3430-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-TIER-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF COLM-COLLECTED-AMT NOT < WS-COLLECTE-TIER-LOW(WS-TX)
                   MOVE WS-COLLECTE-TIER-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3440-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3450-DESCRIBE-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WO'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3460-SET-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF COLM-EXPIRY-DATE > 0 AND COLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF YTD BY ACTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-ACTION.
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
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3480-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO COLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-PAST-DUE.
           IF COLM-PAST-DUE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PAST-DUE-YTD * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE YTD FEE ON THE PROMISED YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3500-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO COLM-PAST-DUE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-REVIEW-FLAG = 'Y'
                   COMPUTE COLM-PAST-DUE-YTD ROUNDED = COLM-PROMISED-YTD
                       * COLM-FEE-PCT / 200
               WHEN COLM-STATUS-CD = 'PS'
                   MOVE 3.00 TO COLM-PAST-DUE-YTD
               WHEN COLM-STATUS-CD = 'UR'
                   MOVE 3.00 TO COLM-PAST-DUE-YTD
               WHEN OTHER
                   COMPUTE COLM-PAST-DUE-YTD ROUNDED = COLM-PROMISED-YTD
                       * COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-PAST-DUE-YTD > 0 AND COLM-PAST-DUE-YTD < 5.00
               MOVE 5.00 TO COLM-PAST-DUE-YTD
           END-IF
           IF COLM-PAST-DUE-YTD > 75.00
               MOVE 75.00 TO COLM-PAST-DUE-YTD
           END-IF
           ADD COLM-PAST-DUE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PAST DUE AMT BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-QUEUE-CD
                   ADD COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3520-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF COLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO COLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLM-COLLECTED-YTD / 12
           COMPUTE WS-WORK-AMT-2 = COLM-COLLECTED-YTD - (WS-WORK-AMT *
               12)
           COMPUTE COLT-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3540-FLAG-PAST-DUE.
           IF COLM-PAST-DUE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLM-PAST-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3550-FLAG-FEE.
           IF COLT-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E742 NAME REQUIRED
      * E916 PROMISED YTD NEGATIVE
      * E233 PROMISED YTD OVER 50000
      * E546 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3560-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-NAME = SPACES
                   MOVE 'E742' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-YTD < 0
                   MOVE 'E916' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-YTD > 50000.00
                   MOVE 'E233' TO WS-REJECT-CODE
               WHEN COLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E546' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3570-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-FEE-AMT ROUNDED =
                   COLM-COLLECTED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLT-FEE-AMT
           END-IF
           ADD COLT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3580-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 3
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-CYCLE.
           IF COLM-CYCLE-CNT < 9999999
               ADD 1 TO COLM-CYCLE-CNT
           END-IF
           IF COLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO COLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AT -> AU WHEN LAST MAINT DATE IS PAST
      * AU -> XR WHEN LAST MAINT DATE IS PAST
      * XR -> XU WHEN LAST MAINT DATE IS PAST
      * XU -> AT WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3600-SET-REGION.
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
      * TRANSLATE ACTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3610-DESCRIBE-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WRITEOFF AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3630-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = COLM-WRITEOFF-AMT *
                       COLM-PENALTY-PCT / 200
               WHEN COLM-CYCLE-CD = 'BE'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN COLM-CYCLE-CD = 'GV'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = COLM-WRITEOFF-AMT *
                       COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PROMISED YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3640-ACCRUE-PROMISED.
           IF COLM-PROMISED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PROMISED-YTD * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3650-LIMIT-PAST-DUE.
           IF COLM-PAST-DUE-YTD < 25.00
               MOVE 25.00 TO COLM-PAST-DUE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PAST-DUE-YTD > 10000.00
               MOVE 10000.00 TO COLM-PAST-DUE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3660-DESCRIBE-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COLLECTED YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3680-LIMIT-COLLECTED.
           IF COLM-COLLECTED-YTD < 50.00
               MOVE 50.00 TO COLM-COLLECTED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-COLLECTED-YTD > 2500.00
               MOVE 2500.00 TO COLM-COLLECTED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE TIER TIER FOR THE PROMISED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3690-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-TIER-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF COLM-PROMISED-AMT NOT < WS-COLLECTE-TIER-LOW(WS-TX)
                   MOVE WS-COLLECTE-TIER-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COLLECTED AMT.
      *----------------------------------------------------------------*
       3700-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-COLLECTED-AMT ROUNDED =
                   COLM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLT-COLLECTED-AMT
           END-IF
           ADD COLT-COLLECTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT PROMISED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3710-SPLIT-PROMISED.
           COMPUTE WS-WORK-AMT = COLM-PROMISED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = COLM-PROMISED-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E109 OPERATOR REQUIRED
      * E935 WORK AMT 2 NEGATIVE
      * E062 WORK AMT 2 OVER 10000
      * E227 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-OPERATOR = SPACES
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E935' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E062' TO WS-REJECT-CODE
               WHEN COLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E227' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COLLECTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAST DUE AMT.
      *----------------------------------------------------------------*
       3730-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-PAST-DUE-AMT ROUNDED =
                   COLM-COLLECTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLT-PAST-DUE-AMT
           END-IF
           ADD COLT-PAST-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE QUEUE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3740-DESCRIBE-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ER'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ME'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PROMISED YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3750-ACCRUE-PROMISED.
           IF COLM-PROMISED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PROMISED-YTD * COLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLT-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WRITEOFF YTD FEE ON THE PROMISED AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3760-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO COLM-WRITEOFF-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-REVIEW-FLAG = 'Y'
                   COMPUTE COLM-WRITEOFF-YTD ROUNDED = COLM-PROMISED-AMT
                       * COLM-MARGIN / 200
               WHEN COLM-REGION-CD = 'AT'
                   MOVE 10.00 TO COLM-WRITEOFF-YTD
               WHEN COLM-REGION-CD = 'AU'
                   MOVE 10.00 TO COLM-WRITEOFF-YTD
               WHEN OTHER
                   COMPUTE COLM-WRITEOFF-YTD ROUNDED = COLM-PROMISED-AMT
                       * COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-WRITEOFF-YTD > 0 AND COLM-WRITEOFF-YTD < 1.00
               MOVE 1.00 TO COLM-WRITEOFF-YTD
           END-IF
           IF COLM-WRITEOFF-YTD > 75.00
               MOVE 75.00 TO COLM-WRITEOFF-YTD
           END-IF
           ADD COLM-WRITEOFF-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE TIER TIER FOR THE COLLECTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3770-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-TIER-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF COLM-COLLECTED-AMT NOT < WS-COLLECTE-TIER-LOW(WS-TX)
                   MOVE WS-COLLECTE-TIER-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3780-COUNT-CYCLE.
           IF COLM-CYCLE-CNT < 9999999
               ADD 1 TO COLM-CYCLE-CNT
           END-IF
           IF COLM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> CL WHEN LAST MAINT DATE IS PAST
      * CL -> LT WHEN LAST MAINT DATE IS PAST
      * LT -> WO WHEN LAST MAINT DATE IS PAST
      * WO -> RF WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CL' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CL'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LT' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LT'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WO' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WO'
                   IF COLM-LAST-MAINT-DATE > 0 AND COLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RF' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E954 MEMO REQUIRED
      * E305 WORK AMT 2 NEGATIVE
      * E023 WORK AMT 2 OVER 10000
      * E439 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-MEMO = SPACES
                   MOVE 'E954' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E305' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E023' TO WS-REJECT-CODE
               WHEN COLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3810-FLAG-PAST-DUE.
           IF COLM-PAST-DUE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLM-PAST-DUE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COLLECTED YTD.
      *----------------------------------------------------------------*
       3820-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-COLLECTED-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-COLLECTED-YTD
           END-IF
           ADD COLM-COLLECTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED FEE ON THE COLLECTED AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3830-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO COLT-COLLECTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-REVIEW-FLAG = 'Y'
                   COMPUTE COLT-COLLECTED-AMT ROUNDED =
                       COLM-COLLECTED-AMT * COLM-MARGIN / 200
               WHEN COLM-QUEUE-CD = 'DL'
                   MOVE 5.00 TO COLT-COLLECTED-AMT
               WHEN COLM-QUEUE-CD = 'ER'
                   MOVE 20.00 TO COLT-COLLECTED-AMT
               WHEN OTHER
                   COMPUTE COLT-COLLECTED-AMT ROUNDED =
                       COLM-COLLECTED-AMT * COLM-MARGIN / 100
           END-EVALUATE
           IF COLT-COLLECTED-AMT > 0 AND COLT-COLLECTED-AMT < 5.00
               MOVE 5.00 TO COLT-COLLECTED-AMT
           END-IF
           IF COLT-COLLECTED-AMT > 75.00
               MOVE 75.00 TO COLT-COLLECTED-AMT
           END-IF
           ADD COLT-COLLECTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E421 MEMO REQUIRED
      * E447 PROMISED AMT NEGATIVE
      * E549 PROMISED AMT OVER 50000
      * E452 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-MEMO = SPACES
                   MOVE 'E421' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT < 0
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT > 50000.00
                   MOVE 'E549' TO WS-REJECT-CODE
               WHEN COLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E452' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3850-DESCRIBE-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE TIER TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3860-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-TIER-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF COLM-PAST-DUE-AMT NOT < WS-COLLECTE-TIER-LOW(WS-TX)
                   MOVE WS-COLLECTE-TIER-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3870-COUNT-CYCLE.
           IF COLM-CYCLE-CNT < 9999999
               ADD 1 TO COLM-CYCLE-CNT
           END-IF
           IF COLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3880-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 10
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3890-DESCRIBE-STATUS.
           EVALUATE COLM-STATUS-CD
               WHEN 'PS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E145 NAME REQUIRED
      * E521 FEE AMT NEGATIVE
      * E944 FEE AMT OVER 5000
      * E351 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3900-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-NAME = SPACES
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN COLT-FEE-AMT < 0
                   MOVE 'E521' TO WS-REJECT-CODE
               WHEN COLT-FEE-AMT > 5000.00
                   MOVE 'E944' TO WS-REJECT-CODE
               WHEN COLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E351' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E095 ADDR REQUIRED
      * E956 WORK AMT NEGATIVE
      * E921 WORK AMT OVER 10000
      * E941 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-ADDR = SPACES
                   MOVE 'E095' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E956' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E921' TO WS-REJECT-CODE
               WHEN COLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E941' TO WS-REJECT-CODE
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
       3920-DESCRIBE-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'AU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3930-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-FEE-AMT ROUNDED =
                   COLT-WRITEOFF-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-FEE-AMT
           END-IF
           ADD COLM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3940-LIMIT-FEE.
           IF COLT-FEE-AMT < 25.00
               MOVE 25.00 TO COLT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-FEE-AMT > 5000.00
               MOVE 5000.00 TO COLT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-NOTICE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO COLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAST DUE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = COLM-PAST-DUE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = COLM-PAST-DUE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE COLM-PAST-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3970-FLAG-WRITEOFF.
           IF COLM-WRITEOFF-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLM-WRITEOFF-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-CYCLE.
           IF COLM-CYCLE-CNT < 9999999
               ADD 1 TO COLM-CYCLE-CNT
           END-IF
           IF COLM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO COLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3990-ACCRUE-COLLECTED.
           IF COLM-COLLECTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-COLLECTED-YTD * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4000-ACCUM-REGION.
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
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       4010-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-FEE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLT-FEE-AMT
           END-IF
           ADD COLT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN POST DATE IS PAST
      * BJ -> GG WHEN POST DATE IS PAST
      * GG -> ZR WHEN POST DATE IS PAST
      * ZR -> AT WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4020-SET-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZR' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAST DUE YTD.
      *----------------------------------------------------------------*
       4030-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-PAST-DUE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-PAST-DUE-YTD
           END-IF
           ADD COLM-PAST-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4040-COUNT-ITEM.
           IF COLM-ITEM-CNT < 9999999
               ADD 1 TO COLM-ITEM-CNT
           END-IF
           IF COLM-ITEM-CNT NOT < 5
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-NOTICE.
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
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * BE -> GV WHEN EFF DATE IS PAST
      * GV -> JM WHEN EFF DATE IS PAST
      * JM -> LC WHEN EFF DATE IS PAST
      * LC -> MK WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4060-SET-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GV' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GV'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LC' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LC'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MK' TO COLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WRITEOFF AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4070-ACCRUE-WRITEOFF.
           IF COLM-WRITEOFF-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-WRITEOFF-AMT * COLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AT -> AU WHEN NEXT DUE DATE IS PAST
      * AU -> XR WHEN NEXT DUE DATE IS PAST
      * XR -> XU WHEN NEXT DUE DATE IS PAST
      * XU -> AT WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4080-SET-REGION.
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
      * DAILY INTEREST ON WRITEOFF YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4090-ACCRUE-WRITEOFF.
           IF COLM-WRITEOFF-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-WRITEOFF-YTD * COLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-PROMISED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 12
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4110-FLAG-WRITEOFF.
           IF COLM-WRITEOFF-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE COLM-WRITEOFF-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4120-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE YTD FEE ON THE WRITEOFF AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4130-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO COLM-PAST-DUE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   COMPUTE COLM-PAST-DUE-YTD ROUNDED = COLT-WRITEOFF-AMT
                       * COLM-MARGIN / 200
               WHEN COLM-ACTION-CD = 'KJ'
                   MOVE 20.00 TO COLM-PAST-DUE-YTD
               WHEN COLM-ACTION-CD = 'TM'
                   MOVE 12.00 TO COLM-PAST-DUE-YTD
               WHEN OTHER
                   COMPUTE COLM-PAST-DUE-YTD ROUNDED = COLT-WRITEOFF-AMT
                       * COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-PAST-DUE-YTD > 0 AND COLM-PAST-DUE-YTD < 1.00
               MOVE 1.00 TO COLM-PAST-DUE-YTD
           END-IF
           IF COLM-PAST-DUE-YTD > 50.00
               MOVE 50.00 TO COLM-PAST-DUE-YTD
           END-IF
           ADD COLM-PAST-DUE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4140-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLM-COLLECTED-YTD / 12
           COMPUTE WS-WORK-AMT-2 = COLM-COLLECTED-YTD - (WS-WORK-AMT *
               12)
           COMPUTE COLM-COLLECTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4150-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E370 ADDR REQUIRED
      * E671 WRITEOFF AMT NEGATIVE
      * E608 WRITEOFF AMT OVER 10000
      * E576 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4160-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-ADDR = SPACES
                   MOVE 'E370' TO WS-REJECT-CODE
               WHEN COLT-WRITEOFF-AMT < 0
                   MOVE 'E671' TO WS-REJECT-CODE
               WHEN COLT-WRITEOFF-AMT > 10000.00
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN COLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E576' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-NOTICE.
           IF COLM-NOTICE-CNT < 9999999
               ADD 1 TO COLM-NOTICE-CNT
           END-IF
           IF COLM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO COLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4180-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REGION-CD
                   ADD COLT-COLLECTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-COLLECTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4190-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO COLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4200-DESCRIBE-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4210-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-TRAN-CD
                   ADD COLT-COLLECTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-COLLECTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4220-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF COLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO COLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4230-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = COLM-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = COLM-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE COLM-PAST-DUE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4240-LIMIT-COLLECTED.
           IF COLM-COLLECTED-AMT < 50.00
               MOVE 50.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-COLLECTED-AMT > 1000.00
               MOVE 1000.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4250-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REGION-CD
                   ADD COLT-WRITEOFF-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-WRITEOFF-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4260-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF COLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4270-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-CHANNEL-CD
                   ADD COLM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4280-LIMIT-COLLECTED.
           IF COLM-COLLECTED-YTD < 25.00
               MOVE 25.00 TO COLM-COLLECTED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-COLLECTED-YTD > 500.00
               MOVE 500.00 TO COLM-COLLECTED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4290-ACCRUE-FEE.
           IF COLM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-FEE-YTD * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLM-PAST-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WRITEOFF YTD FEE ON THE PAST DUE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4300-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO COLM-WRITEOFF-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-REVIEW-FLAG = 'Y'
                   COMPUTE COLM-WRITEOFF-YTD ROUNDED = COLM-PAST-DUE-YTD
                       * COLM-FEE-PCT / 200
               WHEN COLM-ACTION-CD = 'KJ'
                   MOVE 5.00 TO COLM-WRITEOFF-YTD
               WHEN COLM-ACTION-CD = 'TM'
                   MOVE 15.00 TO COLM-WRITEOFF-YTD
               WHEN OTHER
                   COMPUTE COLM-WRITEOFF-YTD ROUNDED = COLM-PAST-DUE-YTD
                       * COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-WRITEOFF-YTD > 0 AND COLM-WRITEOFF-YTD < 10.00
               MOVE 10.00 TO COLM-WRITEOFF-YTD
           END-IF
           IF COLM-WRITEOFF-YTD > 75.00
               MOVE 75.00 TO COLM-WRITEOFF-YTD
           END-IF
           ADD COLM-WRITEOFF-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4310-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-CHANNEL-CD
                   ADD COLT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4320-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4330-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-CYCLE-CD
                   ADD COLM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4340-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 3
               MOVE 'Y' TO COLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4350-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF COLM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE COLLECTED AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4360-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = COLM-COLLECTED-AMT *
                       COLM-MARGIN / 200
               WHEN COLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN COLT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = COLM-COLLECTED-AMT *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E593 CONTACT REQUIRED
      * E479 WORK AMT NEGATIVE
      * E607 WORK AMT OVER 50000
      * E995 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4370-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CONTACT = SPACES
                   MOVE 'E593' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E479' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E607' TO WS-REJECT-CODE
               WHEN COLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E995' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4380-FLAG-FEE.
           IF COLM-FEE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4390-COUNT-CYCLE.
           IF COLM-CYCLE-CNT < 9999999
               ADD 1 TO COLM-CYCLE-CNT
           END-IF
           IF COLM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4400-COUNT-NOTICE.
           IF COLM-NOTICE-CNT < 9999999
               ADD 1 TO COLM-NOTICE-CNT
           END-IF
           IF COLM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO COLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4410-CHECK-EFF.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO COLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4420-LIMIT-COLLECTED.
           IF COLT-COLLECTED-AMT < 0.00
               MOVE 0.00 TO COLT-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-COLLECTED-AMT > 10000.00
               MOVE 10000.00 TO COLT-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4430-AGE-LAST-PAY.
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
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4440-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-QUEUE-CD
                   ADD COLT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4450-AGE-EFFECTIVE.
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
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4460-DESCRIBE-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WO'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AT -> AU WHEN LAST PAY DATE IS PAST
      * AU -> XR WHEN LAST PAY DATE IS PAST
      * XR -> XU WHEN LAST PAY DATE IS PAST
      * XU -> AT WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4470-SET-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AU'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XR' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XR'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XU'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WRITEOFF FEE ON THE WORK AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4480-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN COLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO COLM-WRITEOFF-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE COLM-WRITEOFF-AMT ROUNDED = WS-WORK-AMT *
                       COLM-FEE-PCT / 200
               WHEN COLM-QUEUE-CD = 'DL'
                   MOVE 7.00 TO COLM-WRITEOFF-AMT
               WHEN COLM-QUEUE-CD = 'ER'
                   MOVE 10.00 TO COLM-WRITEOFF-AMT
               WHEN OTHER
                   COMPUTE COLM-WRITEOFF-AMT ROUNDED = WS-WORK-AMT *
                       COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-WRITEOFF-AMT > 0 AND COLM-WRITEOFF-AMT < 2.00
               MOVE 2.00 TO COLM-WRITEOFF-AMT
           END-IF
           IF COLM-WRITEOFF-AMT > 75.00
               MOVE 75.00 TO COLM-WRITEOFF-AMT
           END-IF
           ADD COLM-WRITEOFF-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4490-DESCRIBE-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'AU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4500-DESCRIBE-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'CL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WO'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4510-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO COLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E290 CUST ID REQUIRED
      * E269 COLLECTED YTD NEGATIVE
      * E653 COLLECTED YTD OVER 250000
      * E997 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4520-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CUST-ID = SPACES
                   MOVE 'E290' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-YTD < 0
                   MOVE 'E269' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-YTD > 250000.00
                   MOVE 'E653' TO WS-REJECT-CODE
               WHEN COLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E997' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4530-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLT-COLLECTED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = COLT-COLLECTED-AMT - (WS-WORK-AMT *
               2)
           COMPUTE COLT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PROMISED AMT.
      *----------------------------------------------------------------*
       4540-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-PROMISED-AMT ROUNDED =
                   COLM-PAST-DUE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-PROMISED-AMT
           END-IF
           ADD COLM-PROMISED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4550-DESCRIBE-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4560-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLT-COLLECTED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = COLT-COLLECTED-AMT - (WS-WORK-AMT *
               12)
           COMPUTE COLM-COLLECTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN LAST PAY DATE IS PAST
      * ER -> HA WHEN LAST PAY DATE IS PAST
      * HA -> HW WHEN LAST PAY DATE IS PAST
      * HW -> ME WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4570-SET-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ER' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HA' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HW' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF COLM-LAST-PAY-DATE > 0 AND COLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ME' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4580-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF COLM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO COLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4590-DESCRIBE-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WO'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4600-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 10
               MOVE 'Y' TO COLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E492 DESC REQUIRED
      * E669 PROMISED AMT NEGATIVE
      * E172 PROMISED AMT OVER 10000
      * E954 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4610-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-DESC = SPACES
                   MOVE 'E492' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT < 0
                   MOVE 'E669' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-AMT > 10000.00
                   MOVE 'E172' TO WS-REJECT-CODE
               WHEN COLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E954' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4620-COUNT-ITEM.
           IF COLM-ITEM-CNT < 9999999
               ADD 1 TO COLM-ITEM-CNT
           END-IF
           IF COLM-ITEM-CNT NOT < 10
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4630-COUNT-NOTICE.
           IF COLM-NOTICE-CNT < 9999999
               ADD 1 TO COLM-NOTICE-CNT
           END-IF
           IF COLM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E114 MEMO REQUIRED
      * E886 PROMISED YTD NEGATIVE
      * E058 PROMISED YTD OVER 5000
      * E333 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4640-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-MEMO = SPACES
                   MOVE 'E114' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-YTD < 0
                   MOVE 'E886' TO WS-REJECT-CODE
               WHEN COLM-PROMISED-YTD > 5000.00
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN COLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E333' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       4650-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-FEE-AMT ROUNDED =
                   COLM-WRITEOFF-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-FEE-AMT
           END-IF
           ADD COLM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN POST DATE IS PAST
      * UR -> XM WHEN POST DATE IS PAST
      * XM -> YA WHEN POST DATE IS PAST
      * YA -> PS WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4660-SET-STATUS.
           EVALUATE COLM-STATUS-CD
               WHEN 'PS'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YA' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF COLT-POST-DATE > 0 AND COLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAST DUE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4670-FLAG-PAST-DUE.
           IF COLT-PAST-DUE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLT-PAST-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4680-AGE-NEXT-DUE.
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
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4690-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF COLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4700-ACCRUE-FEE.
           IF COLM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-FEE-AMT * COLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4710-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF COLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO COLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4720-LIMIT-PAST-DUE.
           IF COLM-PAST-DUE-AMT < 50.00
               MOVE 50.00 TO COLM-PAST-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PAST-DUE-AMT > 50000.00
               MOVE 50000.00 TO COLM-PAST-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COLLECTED AMT.
      *----------------------------------------------------------------*
       4730-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-COLLECTED-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-COLLECTED-AMT
           END-IF
           ADD COLM-COLLECTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4740-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 3
               MOVE 'Y' TO COLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4750-LIMIT-FEE.
           IF COLM-FEE-AMT < 0.00
               MOVE 0.00 TO COLM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-FEE-AMT > 10000.00
               MOVE 10000.00 TO COLM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PROMISED AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4760-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = COLM-PROMISED-AMT *
                       COLM-PENALTY-PCT / 200
               WHEN COLM-ACTION-CD = 'KJ'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN COLM-ACTION-CD = 'TM'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = COLM-PROMISED-AMT *
                       COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

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
                   MOVE CLTIER-COLLECTED-AMT
                     TO COLM-COLLECTED-AMT
                   MOVE CLTIER-PROMISED-YTD
                     TO COLM-PROMISED-YTD
                   MOVE CLTIER-EXEMPT-FLAG
                     TO COLM-EXEMPT-FLAG
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
                   MOVE CLLIM-COLLECTED-AMT
                     TO COLM-COLLECTED-AMT
                   MOVE CLLIM-PROMISED-YTD
                     TO COLM-PROMISED-YTD
                   MOVE CLLIM-FEE-YTD
                     TO COLM-FEE-YTD
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
                   MOVE CLRISK-PAST-DUE-AMT
                     TO COLM-PAST-DUE-AMT
                   MOVE CLRISK-PAST-DUE-YTD
                     TO COLM-PAST-DUE-YTD
                   MOVE CLRISK-WRITEOFF-YTD
                     TO COLM-WRITEOFF-YTD
                   MOVE CLRISK-PROMISED-YTD
                     TO COLM-PROMISED-YTD
                   MOVE CLRISK-EXEMPT-FLAG
                     TO COLM-EXEMPT-FLAG
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
           DISPLAY 'CLLSETL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CLLSETL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
