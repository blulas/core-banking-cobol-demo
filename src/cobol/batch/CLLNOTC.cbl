      *================================================================*
      * PROGRAM  : CLLNOTC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : COLLECTIONS QUEUE - CUSTOMER NOTICES. READS THE COL *
      *            TRANSACTIONS, UPDATES THE COL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. DELINQUENT ACCOUNT COLLECTION   *
      *            QUEUES, PROMISES AND LETTERS                        *
      * INPUT    : COLMST (KSDS), COLTRN, BANKCTL                      *
      * OUTPUT   : COLOUT, CLLNORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CLTIER, CLELG           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-06-13 MKL  ORIGINAL                                       *
      * 1999-07-07 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-05-06 BHS  PAST-DUE EXCEPTIONS TO OPS QUEUE PRB3882       *
      * 2002-06-09 PQR  FEE NOTICE TEXT PER LEGAL CHG8309              *
      * 2002-03-08 JLM  PAST-DUE LIMIT REVISED CHG6539                 *
      * 2002-11-25 CRP  PROMISED STATUS CODES ADDED INC7914            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLLNOTC.
       AUTHOR. R J TRAVERS.

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
           SELECT REPORT-FILE ASSIGN TO "CLLNORPT"
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
                                           VALUE 21.000000.
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
      * WS-PAST-DUE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAST-DUE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000393000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000512000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000822000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000800000000660000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003300000000845000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008300000000577000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008400000000861000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008650000000248000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009650000000357500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012150000000833000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017150000000418500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018150000000498000'.
       01  WS-PAST-DUE-GRID-TBL REDEFINES WS-PAST-DUE-GRID-VALUES.
           05  WS-PAST-DUE-GRID-ROW OCCURS 12 TIMES.
               10  WS-PAST-DUE-GRID-LOW    PIC 9(09)V99.
               10  WS-PAST-DUE-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COLLECTE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COLLECTE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000648500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000551000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000906500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000484500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000439500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000461000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000496000'.
       01  WS-COLLECTE-SCHED-TBL REDEFINES WS-COLLECTE-SCHED-VALUES.
           05  WS-COLLECTE-SCHED-ROW OCCURS 7 TIMES.
               10  WS-COLLECTE-SCHED-LOW   PIC 9(09)V99.
               10  WS-COLLECTE-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAST-DUE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAST-DUE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000675000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000353000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000364000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000104000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014500000000488000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014550000000143000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014800000000151500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019800000000366000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020800000000470500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025800000000351500'.
       01  WS-PAST-DUE-BAND-TBL REDEFINES WS-PAST-DUE-BAND-VALUES.
           05  WS-PAST-DUE-BAND-ROW OCCURS 10 TIMES.
               10  WS-PAST-DUE-BAND-LOW    PIC 9(09)V99.
               10  WS-PAST-DUE-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CLLNOTC  COLLECTIONS QUEUE'.
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
       COPY CLELGP.

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
               PERFORM 3000-FLAG-PROMISED
               PERFORM 3010-LIMIT-COLLECTED
               PERFORM 3020-LIMIT-PAST-DUE
               PERFORM 3030-LIMIT-PAST-DUE
               PERFORM 3040-CHECK-NOTICE
               PERFORM 3050-PRORATE-COLLECTED
               PERFORM 3060-COUNT-ITEM
               PERFORM 3070-SPLIT-COLLECTED
               PERFORM 3080-EDIT-DESC
               PERFORM 3090-DESCRIBE-CYCLE
               PERFORM 3100-DESCRIBE-ACTION
               PERFORM 3110-COUNT-RETRY
               PERFORM 3120-FLAG-FEE
               PERFORM 3130-ACCUM-REASON
               PERFORM 3140-ACCUM-TRAN
               PERFORM 3150-SET-CHANNEL
               PERFORM 3160-COUNT-ITEM
               PERFORM 3170-ASSESS-NOTICE-SENT
               PERFORM 3180-SET-ACTION
               PERFORM 3190-SELECT-FEE-PCT
               PERFORM 3200-ACCRUE-COLLECTED
               PERFORM 3210-ACCRUE-FEE-PCT
               PERFORM 3220-ACCRUE-PROMISED
               PERFORM 3230-EDIT-CONTACT
               PERFORM 3240-CHECK-LAST-MAINT
               PERFORM 3250-AGE-NEXT-DUE
               PERFORM 3260-EDIT-CUST-ID
               PERFORM 3270-SET-ACTION
               PERFORM 3280-ASSESS-PREFERRED
               PERFORM 3290-SELECT-PENALTY-PCT
               PERFORM 3300-SELECT-PENALTY-PCT
               PERFORM 3310-ACCRUE-PAST-DUE
               PERFORM 3320-ASSESS-SUSPEND
               PERFORM 3330-PRORATE-FEE
               PERFORM 3340-ACCUM-STATUS
               PERFORM 3350-SPLIT-WRITEOFF
               PERFORM 3360-FLAG-PAST-DUE
               PERFORM 3370-CHECK-EFFECTIVE
               PERFORM 3380-SELECT-FEE-PCT
               PERFORM 3390-CHECK-NEXT-DUE
               PERFORM 3400-AGE-LAST-PAY
               PERFORM 3410-LIMIT-PAST-DUE
               PERFORM 3420-DESCRIBE-STATUS
               PERFORM 3430-ACCRUE-WRITEOFF
               PERFORM 3440-CHECK-CLOSE
               PERFORM 3450-PRORATE-PROMISED
               PERFORM 3460-SPLIT-COLLECTED
               PERFORM 3470-DESCRIBE-CYCLE
               PERFORM 3480-SPLIT-PAST-DUE
               PERFORM 3490-CHECK-POST
               PERFORM 3500-LIMIT-COLLECTED
               PERFORM 3510-ACCUM-CHANNEL
               PERFORM 3520-PRORATE-PROMISED
               PERFORM 3530-AGE-LAST-MAINT
               PERFORM 3540-SET-REASON
               PERFORM 3550-PRORATE-PAST-DUE
               PERFORM 3560-SPLIT-WRITEOFF
               PERFORM 3570-SET-STATUS
               PERFORM 3580-SET-ACTION
               PERFORM 3590-EDIT-CUST-ID
               PERFORM 3600-EDIT-CUST-ID
               PERFORM 3610-DESCRIBE-TRAN
               PERFORM 3620-CHECK-EFFECTIVE
               PERFORM 3630-DESCRIBE-CHANNEL
               PERFORM 3640-ACCUM-CHANNEL
               PERFORM 3650-ACCUM-REGION
               PERFORM 3660-AGE-CLOSE
               PERFORM 3670-COUNT-CYCLE
               PERFORM 3680-DESCRIBE-QUEUE
               PERFORM 3690-ASSESS-OVERRIDE
               PERFORM 3700-COUNT-CYCLE
               PERFORM 3710-EDIT-REF-NO
               PERFORM 3720-ACCRUE-PENALTY-PCT
               PERFORM 3730-SELECT-FEE-PCT
               PERFORM 3740-LIMIT-COLLECTED
               PERFORM 3750-DESCRIBE-QUEUE
               PERFORM 3760-FLAG-FEE
               PERFORM 3770-CHECK-EFF
               PERFORM 3780-EDIT-OPERATOR
               PERFORM 3790-LIMIT-PROMISED
               PERFORM 3800-AGE-CLOSE
               PERFORM 3810-COUNT-RETRY
               PERFORM 3820-DESCRIBE-REASON
               PERFORM 3830-ACCRUE-PAST-DUE
               PERFORM 3840-FLAG-WRITEOFF
               PERFORM 3850-SELECT-PENALTY-PCT
               PERFORM 3860-LIMIT-ITEM
               PERFORM 3870-LIMIT-COLLECTED
               PERFORM 3880-DESCRIBE-CYCLE
               PERFORM 3890-COUNT-SEQ
               PERFORM 3900-PRORATE-PROMISED
               PERFORM 3910-SPLIT-COLLECTED
               PERFORM 3920-SET-TRAN
               PERFORM 3930-ASSESS-SUSPEND
               PERFORM 3940-SET-QUEUE
               PERFORM 3950-AGE-OPEN
               PERFORM 3960-LIMIT-WRITEOFF
               PERFORM 3970-EDIT-DESC
               PERFORM 3980-SPLIT-COLLECTED
               PERFORM 3990-SELECT-PENALTY-PCT
               PERFORM 4000-SELECT-PENALTY-PCT
               PERFORM 4010-LIMIT-PAST-DUE
               PERFORM 4020-ACCRUE-COLLECTED
               PERFORM 4030-AGE-LAST-PAY
               PERFORM 4040-ACCRUE-PENALTY-PCT
               PERFORM 4050-EDIT-ADDR
               PERFORM 4060-DESCRIBE-CHANNEL
               PERFORM 4070-SET-REASON
               PERFORM 4080-SELECT-FEE-PCT
               PERFORM 4090-COUNT-RETRY
               PERFORM 4100-ASSESS-NOTICE-SENT
               PERFORM 4110-LIMIT-PAST-DUE
               PERFORM 4120-ACCUM-REASON
               PERFORM 4130-EDIT-REJECT
               PERFORM 4140-SELECT-FEE-PCT
               PERFORM 4150-COUNT-NOTICE
               PERFORM 4160-FLAG-ITEM
               PERFORM 4170-PRORATE-PAST-DUE
               PERFORM 4180-COUNT-SEQ
               PERFORM 4190-ACCRUE-PROMISED
               PERFORM 4200-EDIT-ADDR
               PERFORM 4210-AGE-NEXT-DUE
               PERFORM 4220-LIMIT-PROMISED
               PERFORM 4230-AGE-CLOSE
               PERFORM 4240-AGE-OPEN
               PERFORM 4250-SPLIT-PROMISED
               PERFORM 4260-ASSESS-REVERSAL
               PERFORM 4270-ASSESS-OVERRIDE
               PERFORM 4280-AGE-LAST-MAINT
               PERFORM 4290-CHECK-NEXT-DUE
               PERFORM 4300-SPLIT-FEE
               PERFORM 4310-SET-REGION
               PERFORM 4320-ASSESS-EXEMPT
               PERFORM 4330-COUNT-NOTICE
               PERFORM 4340-CHECK-NEXT-REVIEW
               PERFORM 4350-ACCUM-QUEUE
               PERFORM 4360-CHECK-EFF
               PERFORM 4370-PRORATE-PAST-DUE
               PERFORM 4380-AGE-CLOSE
               PERFORM 4390-SET-STATUS
               PERFORM 4400-SPLIT-PAST-DUE
               PERFORM 4410-FLAG-ITEM
               PERFORM 4420-SELECT-MARGIN
               PERFORM 4430-SELECT-MARGIN
               PERFORM 4440-ASSESS-OVERRIDE
               PERFORM 4450-ASSESS-EXEMPT
               PERFORM 4460-FLAG-ITEM
               PERFORM 4470-ACCRUE-FEE
               PERFORM 4480-CHECK-OPEN
               PERFORM 4490-SPLIT-FEE
               PERFORM 4500-ACCUM-CYCLE
               PERFORM 4510-COUNT-SEQ
               PERFORM 4520-AGE-NEXT-DUE
               PERFORM 4530-ACCUM-STATUS
               PERFORM 4540-COUNT-NOTICE
               PERFORM 4550-AGE-LAST-PAY
               PERFORM 4560-FLAG-COLLECTED
               PERFORM 4570-EDIT-MEMO
               PERFORM 4580-DESCRIBE-REASON
               PERFORM 4590-ACCUM-CHANNEL
               PERFORM 4600-SPLIT-COLLECTED
               PERFORM 4610-ACCUM-TRAN
               PERFORM 4620-CHECK-EFFECTIVE
               PERFORM 4630-SPLIT-PAST-DUE
               PERFORM 4640-ASSESS-REVERSAL
               PERFORM 4650-ACCRUE-FEE
               PERFORM 4660-CHECK-POST
               PERFORM 4670-CHECK-POST
               PERFORM 4680-EDIT-MEMO
               PERFORM 4690-FLAG-PAST-DUE
               PERFORM 4700-ACCRUE-PAST-DUE
               PERFORM 4710-ASSESS-REVIEW
               PERFORM 4720-AGE-NEXT-REVIEW
               PERFORM 4730-CHECK-LAST-PAY
               PERFORM 4740-CHECK-LAST-PAY
               PERFORM 4750-CHECK-NEXT-REVIEW
               PERFORM 4760-ACCRUE-FEE
               PERFORM 4770-EDIT-OPERATOR
               PERFORM 4780-COUNT-SEQ
               PERFORM 4790-AGE-OPEN
               PERFORM 7010-CALL-CLTIER
               PERFORM 7000-CALL-CLELG
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
      * PROMISED YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3000-FLAG-PROMISED.
           IF COLM-PROMISED-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLM-PROMISED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-COLLECTED.
           IF COLT-COLLECTED-AMT < 0.00
               MOVE 0.00 TO COLT-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-COLLECTED-AMT > 50000.00
               MOVE 50000.00 TO COLT-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3020-LIMIT-PAST-DUE.
           IF COLM-PAST-DUE-YTD < 50.00
               MOVE 50.00 TO COLM-PAST-DUE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PAST-DUE-YTD > 50000.00
               MOVE 50000.00 TO COLM-PAST-DUE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3030-LIMIT-PAST-DUE.
           IF COLT-PAST-DUE-AMT < 10.00
               MOVE 10.00 TO COLT-PAST-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-PAST-DUE-AMT > 50000.00
               MOVE 50000.00 TO COLT-PAST-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3040-CHECK-NOTICE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO COLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAST DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COLLECTED AMT.
      *----------------------------------------------------------------*
       3050-PRORATE-COLLECTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-COLLECTED-AMT ROUNDED =
                   COLT-PAST-DUE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-COLLECTED-AMT
           END-IF
           ADD COLM-COLLECTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3060-COUNT-ITEM.
           IF COLM-ITEM-CNT < 9999999
               ADD 1 TO COLM-ITEM-CNT
           END-IF
           IF COLM-ITEM-CNT NOT < 3
               MOVE 'Y' TO COLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3070-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE COLT-COLLECTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E527 DESC REQUIRED
      * E727 FEE AMT NEGATIVE
      * E836 FEE AMT OVER 99999
      * E108 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-DESC = SPACES
                   MOVE 'E527' TO WS-REJECT-CODE
               WHEN COLT-FEE-AMT < 0
                   MOVE 'E727' TO WS-REJECT-CODE
               WHEN COLT-FEE-AMT > 99999.00
                   MOVE 'E836' TO WS-REJECT-CODE
               WHEN COLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E108' TO WS-REJECT-CODE
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
       3090-DESCRIBE-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE ACTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3100-DESCRIBE-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO COLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3120-FLAG-FEE.
           IF COLM-FEE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3130-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REASON-CD
                   ADD COLT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3140-ACCUM-TRAN.
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
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3150-SET-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO COLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3160-COUNT-ITEM.
           IF COLM-ITEM-CNT < 9999999
               ADD 1 TO COLM-ITEM-CNT
           END-IF
           IF COLM-ITEM-CNT NOT < 6
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE PROMISED YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3170-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO COLM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   COMPUTE COLM-FEE-AMT ROUNDED = COLM-PROMISED-YTD *
                       COLM-FEE-PCT / 200
               WHEN COLM-REGION-CD = 'AT'
                   MOVE 20.00 TO COLM-FEE-AMT
               WHEN COLM-REGION-CD = 'AU'
                   MOVE 7.00 TO COLM-FEE-AMT
               WHEN OTHER
                   COMPUTE COLM-FEE-AMT ROUNDED = COLM-PROMISED-YTD *
                       COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-FEE-AMT > 0 AND COLM-FEE-AMT < 5.00
               MOVE 5.00 TO COLM-FEE-AMT
           END-IF
           IF COLM-FEE-AMT > 50.00
               MOVE 50.00 TO COLM-FEE-AMT
           END-IF
           ADD COLM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ACTION CD:
      * KJ -> TM WHEN NOTICE DATE IS PAST
      * TM -> VL WHEN NOTICE DATE IS PAST
      * VL -> ZS WHEN NOTICE DATE IS PAST
      * ZS -> KJ WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3180-SET-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TM'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VL' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VL'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZS' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZS'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJ' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAST DUE GRID TIER FOR THE WRITEOFF AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3190-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAST-DUE-GRID-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF COLT-WRITEOFF-AMT NOT < WS-PAST-DUE-GRID-LOW(WS-TX)
                   MOVE WS-PAST-DUE-GRID-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-COLLECTED.
           IF COLM-COLLECTED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-COLLECTED-AMT * COLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLT-PAST-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLM-PAST-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PROMISED AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3220-ACCRUE-PROMISED.
           IF COLM-PROMISED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PROMISED-AMT * COLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO COLT-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E802 CONTACT REQUIRED
      * E853 PAST DUE YTD NEGATIVE
      * E435 PAST DUE YTD OVER 25000
      * E917 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3230-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CONTACT = SPACES
                   MOVE 'E802' TO WS-REJECT-CODE
               WHEN COLM-PAST-DUE-YTD < 0
                   MOVE 'E853' TO WS-REJECT-CODE
               WHEN COLM-PAST-DUE-YTD > 25000.00
                   MOVE 'E435' TO WS-REJECT-CODE
               WHEN COLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E917' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-LAST-MAINT.
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
               MOVE 'Y' TO COLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3250-AGE-NEXT-DUE.
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
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-WRITEOFF-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E277 CUST ID REQUIRED
      * E348 FEE AMT NEGATIVE
      * E909 FEE AMT OVER 25000
      * E463 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CUST-ID = SPACES
                   MOVE 'E277' TO WS-REJECT-CODE
               WHEN COLT-FEE-AMT < 0
                   MOVE 'E348' TO WS-REJECT-CODE
               WHEN COLT-FEE-AMT > 25000.00
                   MOVE 'E909' TO WS-REJECT-CODE
               WHEN COLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E463' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ACTION CD:
      * KJ -> TM WHEN NEXT REVIEW DATE IS PAST
      * TM -> VL WHEN NEXT REVIEW DATE IS PAST
      * VL -> ZS WHEN NEXT REVIEW DATE IS PAST
      * ZS -> KJ WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3270-SET-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TM' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TM'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VL' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VL'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZS' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZS'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KJ' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE COLLECTED AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3280-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = COLT-COLLECTED-AMT *
                       COLM-FEE-PCT / 200
               WHEN COLT-TRAN-CD = 'PR'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN COLT-TRAN-CD = 'CL'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = COLT-COLLECTED-AMT *
                       COLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAST DUE BAND TIER FOR THE PROMISED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAST-DUE-BAND-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF COLM-PROMISED-AMT NOT < WS-PAST-DUE-BAND-LOW(WS-TX)
                   MOVE WS-PAST-DUE-BAND-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAST DUE GRID TIER FOR THE COLLECTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3300-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAST-DUE-GRID-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF COLM-COLLECTED-AMT NOT < WS-PAST-DUE-GRID-LOW(WS-TX)
                   MOVE WS-PAST-DUE-GRID-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3310-ACCRUE-PAST-DUE.
           IF COLM-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PAST-DUE-AMT * COLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO COLM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE COLLECTED AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3320-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = COLT-COLLECTED-AMT *
                       COLM-FEE-PCT / 200
               WHEN COLM-ACTION-CD = 'KJ'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN COLM-ACTION-CD = 'TM'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = COLT-COLLECTED-AMT *
                       COLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3330-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3340-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-STATUS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = COLM-WRITEOFF-YTD / 4
           COMPUTE WS-WORK-AMT-2 = COLM-WRITEOFF-YTD - (WS-WORK-AMT * 4)
           COMPUTE COLM-PROMISED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3360-FLAG-PAST-DUE.
           IF COLM-PAST-DUE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLM-PAST-DUE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3370-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO COLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAST DUE BAND TIER FOR THE PAST DUE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAST-DUE-BAND-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF COLM-PAST-DUE-YTD NOT < WS-PAST-DUE-BAND-LOW(WS-TX)
                   MOVE WS-PAST-DUE-BAND-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO COLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-LAST-PAY.
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
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAST DUE AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3410-LIMIT-PAST-DUE.
           IF COLM-PAST-DUE-AMT < 0.00
               MOVE 0.00 TO COLM-PAST-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PAST-DUE-AMT > 1000.00
               MOVE 1000.00 TO COLM-PAST-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3420-DESCRIBE-STATUS.
           EVALUATE COLM-STATUS-CD
               WHEN 'PS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WRITEOFF AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-WRITEOFF.
           IF COLM-WRITEOFF-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-WRITEOFF-AMT * COLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-PAST-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-CLOSE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO COLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3450-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PROMISED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3460-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLM-COLLECTED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = COLM-COLLECTED-AMT - (WS-WORK-AMT *
               2)
           COMPUTE COLM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3470-DESCRIBE-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAST DUE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3480-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = COLM-PAST-DUE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = COLM-PAST-DUE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE COLM-PROMISED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3490-CHECK-POST.
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
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3500-LIMIT-COLLECTED.
           IF COLT-COLLECTED-AMT < 50.00
               MOVE 50.00 TO COLT-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-COLLECTED-AMT > 50000.00
               MOVE 50000.00 TO COLT-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-CHANNEL-CD
                   ADD COLT-COLLECTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-COLLECTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PROMISED YTD.
      *----------------------------------------------------------------*
       3520-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-PROMISED-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-PROMISED-YTD
           END-IF
           ADD COLM-PROMISED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3530-AGE-LAST-MAINT.
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
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PROMISED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN EFFECTIVE DATE IS PAST
      * BJ -> GG WHEN EFFECTIVE DATE IS PAST
      * GG -> ZR WHEN EFFECTIVE DATE IS PAST
      * ZR -> AT WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3540-SET-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   IF COLM-EFFECTIVE-DATE > 0 AND COLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF COLM-EFFECTIVE-DATE > 0 AND COLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF COLM-EFFECTIVE-DATE > 0 AND COLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZR' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF COLM-EFFECTIVE-DATE > 0 AND COLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WRITEOFF YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAST DUE YTD.
      *----------------------------------------------------------------*
       3550-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-PAST-DUE-YTD ROUNDED =
                   COLM-WRITEOFF-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLM-PAST-DUE-YTD
           END-IF
           ADD COLM-PAST-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WRITEOFF AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3560-SPLIT-WRITEOFF.
           COMPUTE WS-WORK-AMT = COLT-WRITEOFF-AMT / 4
           COMPUTE WS-WORK-AMT-2 = COLT-WRITEOFF-AMT - (WS-WORK-AMT * 4)
           COMPUTE COLM-COLLECTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN NOTICE DATE IS PAST
      * UR -> XM WHEN NOTICE DATE IS PAST
      * XM -> YA WHEN NOTICE DATE IS PAST
      * YA -> PS WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-STATUS.
           EVALUATE COLM-STATUS-CD
               WHEN 'PS'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YA' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF COLM-NOTICE-DATE > 0 AND COLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ACTION CD:
      * KJ -> TM WHEN NEXT DUE DATE IS PAST
      * TM -> VL WHEN NEXT DUE DATE IS PAST
      * VL -> ZS WHEN NEXT DUE DATE IS PAST
      * ZS -> KJ WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3580-SET-ACTION.
           EVALUATE COLM-ACTION-CD
               WHEN 'KJ'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TM'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VL' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VL'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZS' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZS'
                   IF COLM-NEXT-DUE-DATE > 0 AND COLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJ' TO COLM-ACTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E946 CUST ID REQUIRED
      * E932 COLLECTED YTD NEGATIVE
      * E572 COLLECTED YTD OVER 10000
      * E620 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3590-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CUST-ID = SPACES
                   MOVE 'E946' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-YTD < 0
                   MOVE 'E932' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-YTD > 10000.00
                   MOVE 'E572' TO WS-REJECT-CODE
               WHEN COLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E620' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E115 CUST ID REQUIRED
      * E568 WRITEOFF AMT NEGATIVE
      * E432 WRITEOFF AMT OVER 99999
      * E306 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3600-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-CUST-ID = SPACES
                   MOVE 'E115' TO WS-REJECT-CODE
               WHEN COLT-WRITEOFF-AMT < 0
                   MOVE 'E568' TO WS-REJECT-CODE
               WHEN COLT-WRITEOFF-AMT > 99999.00
                   MOVE 'E432' TO WS-REJECT-CODE
               WHEN COLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E306' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3610-DESCRIBE-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WO'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3620-CHECK-EFFECTIVE.
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
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PROMISED YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3640-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-CHANNEL-CD
                   ADD COLM-PROMISED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-PROMISED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3650-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-REGION-CD
                   ADD COLT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PROMISED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-CYCLE.
           IF COLM-CYCLE-CNT < 9999999
               ADD 1 TO COLM-CYCLE-CNT
           END-IF
           IF COLM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO COLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE QUEUE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3680-DESCRIBE-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ER'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ME'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE COLLECTED YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3690-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO COLM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   COMPUTE COLM-FEE-YTD ROUNDED = COLM-COLLECTED-YTD *
                       COLM-PENALTY-PCT / 200
               WHEN COLT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO COLM-FEE-YTD
               WHEN COLT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO COLM-FEE-YTD
               WHEN OTHER
                   COMPUTE COLM-FEE-YTD ROUNDED = COLM-COLLECTED-YTD *
                       COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF COLM-FEE-YTD > 0 AND COLM-FEE-YTD < 10.00
               MOVE 10.00 TO COLM-FEE-YTD
           END-IF
           IF COLM-FEE-YTD > 250.00
               MOVE 250.00 TO COLM-FEE-YTD
           END-IF
           ADD COLM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-CYCLE.
           IF COLM-CYCLE-CNT < 9999999
               ADD 1 TO COLM-CYCLE-CNT
           END-IF
           IF COLM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E856 REF NO REQUIRED
      * E133 COLLECTED AMT NEGATIVE
      * E317 COLLECTED AMT OVER 50000
      * E761 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-REF-NO = SPACES
                   MOVE 'E856' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-AMT < 0
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN COLM-COLLECTED-AMT > 50000.00
                   MOVE 'E317' TO WS-REJECT-CODE
               WHEN COLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E761' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3720-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * COLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLM-COLLECTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAST DUE BAND TIER FOR THE COLLECTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3730-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAST-DUE-BAND-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF COLT-COLLECTED-AMT NOT < WS-PAST-DUE-BAND-LOW(WS-TX)
                   MOVE WS-PAST-DUE-BAND-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3740-LIMIT-COLLECTED.
           IF COLM-COLLECTED-AMT < 25.00
               MOVE 25.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-COLLECTED-AMT > 5000.00
               MOVE 5000.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE QUEUE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3750-DESCRIBE-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ER'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ME'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3760-FLAG-FEE.
           IF COLM-FEE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3770-CHECK-EFF.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO COLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E460 OPERATOR REQUIRED
      * E557 PAST DUE YTD NEGATIVE
      * E273 PAST DUE YTD OVER 50000
      * E249 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3780-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-OPERATOR = SPACES
                   MOVE 'E460' TO WS-REJECT-CODE
               WHEN COLM-PAST-DUE-YTD < 0
                   MOVE 'E557' TO WS-REJECT-CODE
               WHEN COLM-PAST-DUE-YTD > 50000.00
                   MOVE 'E273' TO WS-REJECT-CODE
               WHEN COLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E249' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PROMISED YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3790-LIMIT-PROMISED.
           IF COLM-PROMISED-YTD < 100.00
               MOVE 100.00 TO COLM-PROMISED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PROMISED-YTD > 500.00
               MOVE 500.00 TO COLM-PROMISED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3800-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3810-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO COLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3830-ACCRUE-PAST-DUE.
           IF COLM-PAST-DUE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PAST-DUE-YTD * COLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO COLM-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WRITEOFF YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3840-FLAG-WRITEOFF.
           IF COLM-WRITEOFF-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLM-WRITEOFF-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAST DUE GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3850-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAST-DUE-GRID-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF COLM-FEE-YTD NOT < WS-PAST-DUE-GRID-LOW(WS-TX)
                   MOVE WS-PAST-DUE-GRID-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3860-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COLLECTED AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3870-LIMIT-COLLECTED.
           IF COLM-COLLECTED-AMT < 50.00
               MOVE 50.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-COLLECTED-AMT > 1000.00
               MOVE 1000.00 TO COLM-COLLECTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-CYCLE.
           EVALUATE COLM-CYCLE-CD
               WHEN 'BE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3890-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 10
               MOVE 'Y' TO COLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3900-PRORATE-PROMISED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PROMISED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3910-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLM-COLLECTED-YTD / 12
           COMPUTE WS-WORK-AMT-2 = COLM-COLLECTED-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PR -> CL WHEN EFF DATE IS PAST
      * CL -> LT WHEN EFF DATE IS PAST
      * LT -> WO WHEN EFF DATE IS PAST
      * WO -> RF WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3920-SET-TRAN.
           EVALUATE COLT-TRAN-CD
               WHEN 'PR'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CL' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CL'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LT'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WO' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WO'
                   IF COLT-EFF-DATE > 0 AND COLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO COLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE FEE ON THE COLLECTED AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3930-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN COLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO COLT-PAST-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   COMPUTE COLT-PAST-DUE-AMT ROUNDED =
                       COLT-COLLECTED-AMT * COLM-PENALTY-PCT / 200
               WHEN COLM-QUEUE-CD = 'DL'
                   MOVE 15.00 TO COLT-PAST-DUE-AMT
               WHEN COLM-QUEUE-CD = 'ER'
                   MOVE 20.00 TO COLT-PAST-DUE-AMT
               WHEN OTHER
                   COMPUTE COLT-PAST-DUE-AMT ROUNDED =
                       COLT-COLLECTED-AMT * COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF COLT-PAST-DUE-AMT > 0 AND COLT-PAST-DUE-AMT < 5.00
               MOVE 5.00 TO COLT-PAST-DUE-AMT
           END-IF
           IF COLT-PAST-DUE-AMT > 35.00
               MOVE 35.00 TO COLT-PAST-DUE-AMT
           END-IF
           ADD COLT-PAST-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR QUEUE CD:
      * DL -> ER WHEN NEXT REVIEW DATE IS PAST
      * ER -> HA WHEN NEXT REVIEW DATE IS PAST
      * HA -> HW WHEN NEXT REVIEW DATE IS PAST
      * HW -> ME WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-QUEUE.
           EVALUATE COLM-QUEUE-CD
               WHEN 'DL'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ER' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ER'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HA' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HA'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HW' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HW'
                   IF COLM-NEXT-REVIEW-DATE > 0 AND
                       COLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ME' TO COLM-QUEUE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3950-AGE-OPEN.
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
               WHEN WS-DAYS NOT > 45
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WRITEOFF YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3960-LIMIT-WRITEOFF.
           IF COLM-WRITEOFF-YTD < 100.00
               MOVE 100.00 TO COLM-WRITEOFF-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-WRITEOFF-YTD > 50000.00
               MOVE 50000.00 TO COLM-WRITEOFF-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E848 DESC REQUIRED
      * E197 WORK AMT NEGATIVE
      * E796 WORK AMT OVER 99999
      * E171 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-DESC = SPACES
                   MOVE 'E848' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E197' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E796' TO WS-REJECT-CODE
               WHEN COLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E171' TO WS-REJECT-CODE
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
       3980-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLM-COLLECTED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = COLM-COLLECTED-AMT - (WS-WORK-AMT *
               2)
           COMPUTE COLM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAST DUE GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3990-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAST-DUE-GRID-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-PAST-DUE-GRID-LOW(WS-TX)
                   MOVE WS-PAST-DUE-GRID-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAST DUE GRID TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4000-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAST-DUE-GRID-RATE(1) TO COLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF COLT-FEE-AMT NOT < WS-PAST-DUE-GRID-LOW(WS-TX)
                   MOVE WS-PAST-DUE-GRID-RATE(WS-TX) TO COLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4010-LIMIT-PAST-DUE.
           IF COLT-PAST-DUE-AMT < 50.00
               MOVE 50.00 TO COLT-PAST-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLT-PAST-DUE-AMT > 10000.00
               MOVE 10000.00 TO COLT-PAST-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COLLECTED YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4020-ACCRUE-COLLECTED.
           IF COLM-COLLECTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-COLLECTED-YTD * COLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLT-PAST-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4030-AGE-LAST-PAY.
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
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4040-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * COLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO COLT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E791 ADDR REQUIRED
      * E590 COLLECTED AMT NEGATIVE
      * E063 COLLECTED AMT OVER 10000
      * E273 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-ADDR = SPACES
                   MOVE 'E791' TO WS-REJECT-CODE
               WHEN COLT-COLLECTED-AMT < 0
                   MOVE 'E590' TO WS-REJECT-CODE
               WHEN COLT-COLLECTED-AMT > 10000.00
                   MOVE 'E063' TO WS-REJECT-CODE
               WHEN COLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E273' TO WS-REJECT-CODE
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
       4060-DESCRIBE-CHANNEL.
           EVALUATE COLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AT -> BJ WHEN OPEN DATE IS PAST
      * BJ -> GG WHEN OPEN DATE IS PAST
      * GG -> ZR WHEN OPEN DATE IS PAST
      * ZR -> AT WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4070-SET-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BJ' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BJ'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GG' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GG'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZR' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZR'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE SCHED TIER FOR THE PAST DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4080-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-SCHED-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF COLM-PAST-DUE-AMT NOT < WS-COLLECTE-SCHED-LOW(WS-TX)
                   MOVE WS-COLLECTE-SCHED-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-RETRY.
           IF COLM-RETRY-CNT < 9999999
               ADD 1 TO COLM-RETRY-CNT
           END-IF
           IF COLM-RETRY-CNT NOT < 6
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WRITEOFF AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4100-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = COLM-WRITEOFF-AMT *
                       COLM-FEE-PCT / 200
               WHEN COLM-QUEUE-CD = 'DL'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN COLM-QUEUE-CD = 'ER'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = COLM-WRITEOFF-AMT *
                       COLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAST DUE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4110-LIMIT-PAST-DUE.
           IF COLM-PAST-DUE-AMT < 50.00
               MOVE 50.00 TO COLM-PAST-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PAST-DUE-AMT > 500.00
               MOVE 500.00 TO COLM-PAST-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WRITEOFF YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4120-ACCUM-REASON.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E353 REJECT CD REQUIRED
      * E877 FEE AMT NEGATIVE
      * E600 FEE AMT OVER 99999
      * E228 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4130-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-REJECT-CD = SPACES
                   MOVE 'E353' TO WS-REJECT-CODE
               WHEN COLM-FEE-AMT < 0
                   MOVE 'E877' TO WS-REJECT-CODE
               WHEN COLM-FEE-AMT > 99999.00
                   MOVE 'E600' TO WS-REJECT-CODE
               WHEN COLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E228' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAST DUE GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4140-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAST-DUE-GRID-RATE(1) TO COLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF COLM-FEE-YTD NOT < WS-PAST-DUE-GRID-LOW(WS-TX)
                   MOVE WS-PAST-DUE-GRID-RATE(WS-TX) TO COLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4150-COUNT-NOTICE.
           IF COLM-NOTICE-CNT < 9999999
               ADD 1 TO COLM-NOTICE-CNT
           END-IF
           IF COLM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4160-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PROMISED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAST DUE AMT.
      *----------------------------------------------------------------*
       4170-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLT-PAST-DUE-AMT ROUNDED =
                   COLM-PROMISED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO COLT-PAST-DUE-AMT
           END-IF
           ADD COLT-PAST-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4180-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 5
               MOVE 'Y' TO COLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PROMISED YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4190-ACCRUE-PROMISED.
           IF COLM-PROMISED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PROMISED-YTD * COLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLM-COLLECTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E521 ADDR REQUIRED
      * E687 WRITEOFF YTD NEGATIVE
      * E265 WRITEOFF YTD OVER 99999
      * E219 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLM-ADDR = SPACES
                   MOVE 'E521' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-YTD < 0
                   MOVE 'E687' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-YTD > 99999.00
                   MOVE 'E265' TO WS-REJECT-CODE
               WHEN COLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E219' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4210-AGE-NEXT-DUE.
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
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PROMISED YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4220-LIMIT-PROMISED.
           IF COLM-PROMISED-YTD < 50.00
               MOVE 50.00 TO COLM-PROMISED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF COLM-PROMISED-YTD > 10000.00
               MOVE 10000.00 TO COLM-PROMISED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4230-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4240-AGE-OPEN.
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
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLT-WRITEOFF-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PROMISED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4250-SPLIT-PROMISED.
           COMPUTE WS-WORK-AMT = COLM-PROMISED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = COLM-PROMISED-YTD - (WS-WORK-AMT * 2)
           COMPUTE COLT-WRITEOFF-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE FEE ON THE PROMISED AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4260-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO COLT-PAST-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   COMPUTE COLT-PAST-DUE-AMT ROUNDED = COLM-PROMISED-AMT
                       * COLM-FEE-PCT / 200
               WHEN COLT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO COLT-PAST-DUE-AMT
               WHEN COLT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO COLT-PAST-DUE-AMT
               WHEN OTHER
                   COMPUTE COLT-PAST-DUE-AMT ROUNDED = COLM-PROMISED-AMT
                       * COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLT-PAST-DUE-AMT > 0 AND COLT-PAST-DUE-AMT < 10.00
               MOVE 10.00 TO COLT-PAST-DUE-AMT
           END-IF
           IF COLT-PAST-DUE-AMT > 35.00
               MOVE 35.00 TO COLT-PAST-DUE-AMT
           END-IF
           ADD COLT-PAST-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED YTD FEE ON THE PROMISED AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4270-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO COLM-COLLECTED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-PREFERRED-FLAG = 'Y'
                   COMPUTE COLM-COLLECTED-YTD ROUNDED =
                       COLM-PROMISED-AMT * COLM-PENALTY-PCT / 200
               WHEN COLM-STATUS-CD = 'PS'
                   MOVE 3.00 TO COLM-COLLECTED-YTD
               WHEN COLM-STATUS-CD = 'UR'
                   MOVE 12.00 TO COLM-COLLECTED-YTD
               WHEN OTHER
                   COMPUTE COLM-COLLECTED-YTD ROUNDED =
                       COLM-PROMISED-AMT * COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF COLM-COLLECTED-YTD > 0 AND COLM-COLLECTED-YTD < 5.00
               MOVE 5.00 TO COLM-COLLECTED-YTD
           END-IF
           IF COLM-COLLECTED-YTD > 250.00
               MOVE 250.00 TO COLM-COLLECTED-YTD
           END-IF
           ADD COLM-COLLECTED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4280-AGE-LAST-MAINT.
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
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4290-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO COLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4300-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = COLM-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = COLM-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE COLM-COLLECTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * AT -> AU WHEN OPEN DATE IS PAST
      * AU -> XR WHEN OPEN DATE IS PAST
      * XR -> XU WHEN OPEN DATE IS PAST
      * XU -> AT WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4310-SET-REGION.
           EVALUATE COLM-REGION-CD
               WHEN 'AT'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AU'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XR' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XR'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XU' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XU'
                   IF COLM-OPEN-DATE > 0 AND COLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO COLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PROMISED YTD FEE ON THE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4320-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO COLM-PROMISED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-REVIEW-FLAG = 'Y'
                   COMPUTE COLM-PROMISED-YTD ROUNDED = COLM-FEE-AMT *
                       COLM-FEE-PCT / 200
               WHEN COLT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO COLM-PROMISED-YTD
               WHEN COLT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO COLM-PROMISED-YTD
               WHEN OTHER
                   COMPUTE COLM-PROMISED-YTD ROUNDED = COLM-FEE-AMT *
                       COLM-FEE-PCT / 100
           END-EVALUATE
           IF COLM-PROMISED-YTD > 0 AND COLM-PROMISED-YTD < 10.00
               MOVE 10.00 TO COLM-PROMISED-YTD
           END-IF
           IF COLM-PROMISED-YTD > 150.00
               MOVE 150.00 TO COLM-PROMISED-YTD
           END-IF
           ADD COLM-PROMISED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4330-COUNT-NOTICE.
           IF COLM-NOTICE-CNT < 9999999
               ADD 1 TO COLM-NOTICE-CNT
           END-IF
           IF COLM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4340-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY QUEUE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4350-ACCUM-QUEUE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-QUEUE-CD
                   ADD COLM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-QUEUE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4360-CHECK-EFF.
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
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAST DUE AMT.
      *----------------------------------------------------------------*
       4370-PRORATE-PAST-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE COLM-PAST-DUE-AMT ROUNDED =
                   COLT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO COLM-PAST-DUE-AMT
           END-IF
           ADD COLM-PAST-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4380-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE COLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-COLLECTED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * PS -> UR WHEN CLOSE DATE IS PAST
      * UR -> XM WHEN CLOSE DATE IS PAST
      * XM -> YA WHEN CLOSE DATE IS PAST
      * YA -> PS WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4390-SET-STATUS.
           EVALUATE COLM-STATUS-CD
               WHEN 'PS'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UR' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UR'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XM'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YA' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YA'
                   IF COLM-CLOSE-DATE > 0 AND COLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO COLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAST DUE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4400-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = COLM-PAST-DUE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = COLM-PAST-DUE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE COLM-WRITEOFF-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4410-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE SCHED TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4420-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-SCHED-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF COLT-FEE-AMT NOT < WS-COLLECTE-SCHED-LOW(WS-TX)
                   MOVE WS-COLLECTE-SCHED-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COLLECTE SCHED TIER FOR THE WRITEOFF AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4430-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COLLECTE-SCHED-RATE(1) TO COLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF COLT-WRITEOFF-AMT NOT < WS-COLLECTE-SCHED-LOW(WS-TX)
                   MOVE WS-COLLECTE-SCHED-RATE(WS-TX) TO COLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF COLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO COLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COLLECTED FEE ON THE FEE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4440-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO COLT-COLLECTED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   COMPUTE COLT-COLLECTED-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-PENALTY-PCT / 200
               WHEN COLM-REGION-CD = 'AT'
                   MOVE 10.00 TO COLT-COLLECTED-AMT
               WHEN COLM-REGION-CD = 'AU'
                   MOVE 5.00 TO COLT-COLLECTED-AMT
               WHEN OTHER
                   COMPUTE COLT-COLLECTED-AMT ROUNDED = COLM-FEE-YTD *
                       COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF COLT-COLLECTED-AMT > 0 AND COLT-COLLECTED-AMT < 2.00
               MOVE 2.00 TO COLT-COLLECTED-AMT
           END-IF
           IF COLT-COLLECTED-AMT > 250.00
               MOVE 250.00 TO COLT-COLLECTED-AMT
           END-IF
           ADD COLT-COLLECTED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PAST DUE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4450-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = COLM-PAST-DUE-AMT *
                       COLM-PENALTY-PCT / 200
               WHEN COLM-REGION-CD = 'AT'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN COLM-REGION-CD = 'AU'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = COLM-PAST-DUE-AMT *
                       COLM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4460-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4470-ACCRUE-FEE.
           IF COLT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-FEE-AMT * COLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO COLM-COLLECTED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4480-CHECK-OPEN.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO COLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4490-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE COLM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COLLECTED AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4500-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-CYCLE-CD
                   ADD COLT-COLLECTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-COLLECTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4510-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 3
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4520-AGE-NEXT-DUE.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAST DUE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4530-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLM-STATUS-CD
                   ADD COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4540-COUNT-NOTICE.
           IF COLM-NOTICE-CNT < 9999999
               ADD 1 TO COLM-NOTICE-CNT
           END-IF
           IF COLM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO COLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4550-AGE-LAST-PAY.
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
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-PAST-DUE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COLLECTED AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4560-FLAG-COLLECTED.
           IF COLM-COLLECTED-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE COLM-COLLECTED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E029 MEMO REQUIRED
      * E628 WRITEOFF AMT NEGATIVE
      * E113 WRITEOFF AMT OVER 5000
      * E069 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4570-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-MEMO = SPACES
                   MOVE 'E029' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-AMT < 0
                   MOVE 'E628' TO WS-REJECT-CODE
               WHEN COLM-WRITEOFF-AMT > 5000.00
                   MOVE 'E113' TO WS-REJECT-CODE
               WHEN COLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E069' TO WS-REJECT-CODE
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
       4580-DESCRIBE-REASON.
           EVALUATE COLM-REASON-CD
               WHEN 'AT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4590-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COLLECTED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4600-SPLIT-COLLECTED.
           COMPUTE WS-WORK-AMT = COLM-COLLECTED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = COLM-COLLECTED-YTD - (WS-WORK-AMT *
               2)
           COMPUTE COLM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAST DUE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4610-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = COLT-TRAN-CD
                   ADD COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE COLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE COLT-PAST-DUE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4620-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO COLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAST DUE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4630-SPLIT-PAST-DUE.
           COMPUTE WS-WORK-AMT = COLM-PAST-DUE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = COLM-PAST-DUE-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAST DUE YTD FEE ON THE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4640-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN COLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO COLM-PAST-DUE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE COLM-PAST-DUE-YTD ROUNDED = COLT-FEE-AMT *
                       COLM-MARGIN / 200
               WHEN COLM-ACTION-CD = 'KJ'
                   MOVE 12.00 TO COLM-PAST-DUE-YTD
               WHEN COLM-ACTION-CD = 'TM'
                   MOVE 12.00 TO COLM-PAST-DUE-YTD
               WHEN OTHER
                   COMPUTE COLM-PAST-DUE-YTD ROUNDED = COLT-FEE-AMT *
                       COLM-MARGIN / 100
           END-EVALUATE
           IF COLM-PAST-DUE-YTD > 0 AND COLM-PAST-DUE-YTD < 5.00
               MOVE 5.00 TO COLM-PAST-DUE-YTD
           END-IF
           IF COLM-PAST-DUE-YTD > 150.00
               MOVE 150.00 TO COLM-PAST-DUE-YTD
           END-IF
           ADD COLM-PAST-DUE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4650-ACCRUE-FEE.
           IF COLT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLT-FEE-AMT * COLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO COLM-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4660-CHECK-POST.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO COLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4670-CHECK-POST.
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
               MOVE 'Y' TO COLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E729 MEMO REQUIRED
      * E385 FEE YTD NEGATIVE
      * E353 FEE YTD OVER 10000
      * E383 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4680-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-MEMO = SPACES
                   MOVE 'E729' TO WS-REJECT-CODE
               WHEN COLM-FEE-YTD < 0
                   MOVE 'E385' TO WS-REJECT-CODE
               WHEN COLM-FEE-YTD > 10000.00
                   MOVE 'E353' TO WS-REJECT-CODE
               WHEN COLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E383' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAST DUE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4690-FLAG-PAST-DUE.
           IF COLM-PAST-DUE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE COLM-PAST-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAST DUE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4700-ACCRUE-PAST-DUE.
           IF COLM-PAST-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-PAST-DUE-AMT * COLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO COLM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE FEE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4710-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN COLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN COLM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = COLM-FEE-AMT *
                       COLM-FEE-PCT / 200
               WHEN COLM-QUEUE-CD = 'DL'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN COLM-QUEUE-CD = 'ER'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = COLM-FEE-AMT *
                       COLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4720-AGE-NEXT-REVIEW.
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
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4730-CHECK-LAST-PAY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO COLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4740-CHECK-LAST-PAY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4750-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO COLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4760-ACCRUE-FEE.
           IF COLM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   COLM-FEE-YTD * COLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO COLT-COLLECTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E196 OPERATOR REQUIRED
      * E549 WRITEOFF AMT NEGATIVE
      * E534 WRITEOFF AMT OVER 5000
      * E524 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4770-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN COLT-OPERATOR = SPACES
                   MOVE 'E196' TO WS-REJECT-CODE
               WHEN COLT-WRITEOFF-AMT < 0
                   MOVE 'E549' TO WS-REJECT-CODE
               WHEN COLT-WRITEOFF-AMT > 5000.00
                   MOVE 'E534' TO WS-REJECT-CODE
               WHEN COLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E524' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4780-COUNT-SEQ.
           IF COLT-SEQ < 9999999
               ADD 1 TO COLT-SEQ
           END-IF
           IF COLT-SEQ NOT < 10
               MOVE 'Y' TO COLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4790-AGE-OPEN.
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
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD COLT-COLLECTED-AMT TO WS-AGE-AMT(5)
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
                   MOVE CLTIER-PROMISED-YTD
                     TO COLM-PROMISED-YTD
                   MOVE CLTIER-EXEMPT-FLAG
                     TO COLM-EXEMPT-FLAG
                   MOVE CLTIER-SUSPEND-FLAG
                     TO COLM-SUSPEND-FLAG
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
      * PASS THE COL FIELDS TO CLELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-CLELG.
           INITIALIZE CLELG-PARMS
           MOVE COLM-WRITEOFF-AMT
             TO CLELG-WRITEOFF-AMT
           MOVE COLM-COLLECTED-AMT
             TO CLELG-COLLECTED-AMT
           MOVE COLM-FEE-AMT
             TO CLELG-FEE-AMT
           MOVE COLM-WRITEOFF-YTD
             TO CLELG-WRITEOFF-YTD
           MOVE COLM-PAST-DUE-AMT
             TO CLELG-PAST-DUE-AMT
           MOVE COLM-FEE-YTD
             TO CLELG-FEE-YTD
           MOVE COLM-EFFECTIVE-DATE
             TO CLELG-EFFECTIVE-DATE
           MOVE COLM-EXPIRY-DATE
             TO CLELG-EXPIRY-DATE
           MOVE COLM-LAST-MAINT-DATE
             TO CLELG-LAST-MAINT-DATE
           MOVE COLM-NEXT-DUE-DATE
             TO CLELG-NEXT-DUE-DATE
           MOVE COLM-CYCLE-CD
             TO CLELG-CYCLE-CD
           MOVE COLM-QUEUE-CD
             TO CLELG-QUEUE-CD
           MOVE COLM-STATUS-CD
             TO CLELG-STATUS-CD
           MOVE COLM-REASON-CD
             TO CLELG-REASON-CD
           MOVE COLM-NOTICE-SENT-FLAG
             TO CLELG-NOTICE-SENT-FLAG
           MOVE COLM-EXEMPT-FLAG
             TO CLELG-EXEMPT-FLAG
           MOVE COLM-OVERRIDE-FLAG
             TO CLELG-OVERRIDE-FLAG
           MOVE COLM-NOTICE-CNT
             TO CLELG-NOTICE-CNT
           MOVE COLM-ITEM-CNT
             TO CLELG-ITEM-CNT
           MOVE COLM-MARGIN
             TO CLELG-MARGIN
           MOVE COLM-FEE-PCT
             TO CLELG-FEE-PCT
           MOVE COLM-DESC
             TO CLELG-DESC
           MOVE COLM-ADDR
             TO CLELG-ADDR
           CALL 'CLELG' USING CLELG-PARMS
           EVALUATE CLELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CLELG-WRITEOFF-YTD
                     TO COLM-WRITEOFF-YTD
                   MOVE CLELG-OVERRIDE-FLAG
                     TO COLM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-CLELG' TO ABND-PARAGRAPH
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
           DISPLAY 'CLLNOTC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CLLNOTC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
