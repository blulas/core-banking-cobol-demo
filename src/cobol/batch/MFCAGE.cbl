      *================================================================*
      * PROGRAM  : MFCAGE                                              *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MICROFICHE STATEMENTS - AGING. READS THE MFC        *
      *            TRANSACTIONS, UPDATES THE MFC MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. STATEMENT MICROFICHE INDEXING,  *
      *            RETENTION AND RESEARCH FEES                         *
      * INPUT    : MFCMST (KSDS), MFCTRN, BANKCTL                      *
      * OUTPUT   : MFCOUT, MFCAGRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, MFLIM, MFELG            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-11-17 LMB  ORIGINAL                                       *
      * 2002-01-14 RJT  COPY-FEE ROUNDING - HALF UP CHG1442            *
      * 2002-05-03 GAF  AUDIT FINDING - POSTAGE CONTROL TOTALS PRB5925 *
      * 2003-09-01 LMB  COPY-FEE EXCEPTIONS TO OPS QUEUE REQ9443       *
      * 2008-02-16 GAF  POSTAGE EXCEPTIONS TO OPS QUEUE INC0410        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFCAGE.
       AUTHOR. M K LOWE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "MFCMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS MFCM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "MFCTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "MFCOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "MFCAGRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY MFCMST.
       FD  TRAN-FILE.
       COPY MFCTRN.
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
      * WS-COPY-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COPY-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000466000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000626000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000876000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002250000000114000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000384500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000756000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003750000000360000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008750000000691500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018750000000709000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019000000000345500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019250000000724500'.
       01  WS-COPY-FEE-TIER-TBL REDEFINES WS-COPY-FEE-TIER-VALUES.
           05  WS-COPY-FEE-TIER-ROW OCCURS 11 TIMES.
               10  WS-COPY-FEE-TIER-LOW    PIC 9(09)V99.
               10  WS-COPY-FEE-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STORAGE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000199500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000356500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000786500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000176000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011600000000804000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000236000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014600000000072000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015600000000409500'.
       01  WS-STORAGE-GRID-TBL REDEFINES WS-STORAGE-GRID-VALUES.
           05  WS-STORAGE-GRID-ROW OCCURS 8 TIMES.
               10  WS-STORAGE-GRID-LOW     PIC 9(09)V99.
               10  WS-STORAGE-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-POSTAGE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-POSTAGE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000246500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000890000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000109500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000865500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002050000000307500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000695000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000414500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012650000000300000'.
       01  WS-POSTAGE-SCHED-TBL REDEFINES WS-POSTAGE-SCHED-VALUES.
           05  WS-POSTAGE-SCHED-ROW OCCURS 8 TIMES.
               10  WS-POSTAGE-SCHED-LOW    PIC 9(09)V99.
               10  WS-POSTAGE-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RESEARCH-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RESEARCH-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000086500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000329000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000175000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000240000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016100000000103500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026100000000145500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031100000000452000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031350000000011000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033850000000240500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033950000000675000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00038950000000463000'.
       01  WS-RESEARCH-GRID-TBL REDEFINES WS-RESEARCH-GRID-VALUES.
           05  WS-RESEARCH-GRID-ROW OCCURS 11 TIMES.
               10  WS-RESEARCH-GRID-LOW    PIC 9(09)V99.
               10  WS-RESEARCH-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  MFCAGE  MICROFICHE STATEMENTS'.
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
       COPY MFLIMP.
       COPY MFELGP.

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
               MOVE 'MFCMST' TO ABND-FILE-NAME
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
           MOVE MFCT-KEY TO MFCM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SELECT-MARGIN
               PERFORM 3010-SELECT-DISCOUNT
               PERFORM 3020-ACCUM-CHANNEL
               PERFORM 3030-ASSESS-HOLD
               PERFORM 3040-EDIT-REF-NO
               PERFORM 3050-SPLIT-RESEARCH
               PERFORM 3060-CHECK-EXPIRY
               PERFORM 3070-ASSESS-HOLD
               PERFORM 3080-FLAG-RESEARCH
               PERFORM 3090-DESCRIBE-STATUS
               PERFORM 3100-ACCUM-BRANCH
               PERFORM 3110-SPLIT-COPY-FEE
               PERFORM 3120-AGE-MATURITY
               PERFORM 3130-SPLIT-ITEM
               PERFORM 3140-ACCRUE-STORAGE
               PERFORM 3150-CHECK-NEXT-DUE
               PERFORM 3160-SET-STATUS
               PERFORM 3170-EDIT-CONTACT
               PERFORM 3180-LIMIT-RESEARCH
               PERFORM 3190-SPLIT-POSTAGE
               PERFORM 3200-EDIT-CONTACT
               PERFORM 3210-ASSESS-AUTO-RENEW
               PERFORM 3220-SET-CHANNEL
               PERFORM 3230-ACCUM-RETENTION
               PERFORM 3240-ACCRUE-RESEARCH
               PERFORM 3250-ASSESS-STOP
               PERFORM 3260-ASSESS-REVERSAL
               PERFORM 3270-SELECT-MARGIN
               PERFORM 3280-SELECT-MARGIN
               PERFORM 3290-ACCUM-BRANCH
               PERFORM 3300-COUNT-PAYMENT
               PERFORM 3310-DESCRIBE-RETENTION
               PERFORM 3320-AGE-POST
               PERFORM 3330-SELECT-MARGIN
               PERFORM 3340-PRORATE-POSTAGE
               PERFORM 3350-CHECK-MATURITY
               PERFORM 3360-SET-CHANNEL
               PERFORM 3370-ACCUM-CHANNEL
               PERFORM 3380-ACCUM-TRAN
               PERFORM 3390-DESCRIBE-CHANNEL
               PERFORM 3400-CHECK-EFF
               PERFORM 3410-AGE-LAST-STMT
               PERFORM 3420-COUNT-SEQ
               PERFORM 3430-DESCRIBE-BRANCH
               PERFORM 3440-CHECK-LAST-STMT
               PERFORM 3450-ACCRUE-RESEARCH
               PERFORM 3460-DESCRIBE-STATUS
               PERFORM 3470-SET-RETENTION
               PERFORM 3480-PRORATE-POSTAGE
               PERFORM 3490-PRORATE-POSTAGE
               PERFORM 3500-CHECK-EXPIRY
               PERFORM 3510-COUNT-SEQ
               PERFORM 3520-ASSESS-STOP
               PERFORM 3530-CHECK-CLOSE
               PERFORM 3540-SET-FICHE-TYPE
               PERFORM 3550-ACCUM-TYPE
               PERFORM 3560-FLAG-POSTAGE
               PERFORM 3570-LIMIT-ITEM
               PERFORM 3580-EDIT-REJECT
               PERFORM 3590-COUNT-PAYMENT
               PERFORM 3600-ASSESS-REVIEW
               PERFORM 3610-FLAG-STORAGE
               PERFORM 3620-EDIT-ADDR
               PERFORM 3630-ACCUM-FICHE-TYPE
               PERFORM 3640-SET-TYPE
               PERFORM 3650-SPLIT-RESEARCH
               PERFORM 3660-FLAG-POSTAGE
               PERFORM 3670-SPLIT-RESEARCH
               PERFORM 3680-DESCRIBE-CHANNEL
               PERFORM 3690-DESCRIBE-CHANNEL
               PERFORM 3700-DESCRIBE-RETENTION
               PERFORM 3710-SELECT-MARGIN
               PERFORM 3720-SPLIT-RESEARCH
               PERFORM 3730-CHECK-LAST-PAY
               PERFORM 3740-DESCRIBE-FICHE-TYPE
               PERFORM 3750-ACCRUE-RESEARCH
               PERFORM 3760-SPLIT-COPY-FEE
               PERFORM 3770-ACCUM-CHANNEL
               PERFORM 3780-LIMIT-RESEARCH
               PERFORM 3790-EDIT-DESC
               PERFORM 3800-ACCRUE-STORAGE
               PERFORM 3810-LIMIT-STORAGE
               PERFORM 3820-FLAG-ITEM
               PERFORM 3830-PRORATE-RESEARCH
               PERFORM 3840-ACCUM-BRANCH
               PERFORM 3850-SPLIT-RESEARCH
               PERFORM 3860-FLAG-COPY-FEE
               PERFORM 3870-ACCUM-RETENTION
               PERFORM 3880-DESCRIBE-CHANNEL
               PERFORM 3890-ACCUM-CHANNEL
               PERFORM 3900-FLAG-POSTAGE
               PERFORM 3910-PRORATE-POSTAGE
               PERFORM 3920-ACCUM-CHANNEL
               PERFORM 3930-ASSESS-EXEMPT
               PERFORM 3940-DESCRIBE-RETENTION
               PERFORM 3950-LIMIT-STORAGE
               PERFORM 3960-FLAG-RESEARCH
               PERFORM 3970-EDIT-ADDR
               PERFORM 3980-ASSESS-STOP
               PERFORM 3990-ACCRUE-RESEARCH
               PERFORM 4000-SELECT-MARGIN
               PERFORM 4010-ACCUM-BRANCH
               PERFORM 4020-AGE-CLOSE
               PERFORM 4030-SPLIT-COPY-FEE
               PERFORM 4040-COUNT-PAYMENT
               PERFORM 4050-ASSESS-EXEMPT
               PERFORM 4060-EDIT-MEMO
               PERFORM 4070-PRORATE-POSTAGE
               PERFORM 4080-AGE-MATURITY
               PERFORM 4090-SELECT-DISCOUNT
               PERFORM 4100-SET-TYPE
               PERFORM 4110-COUNT-PAYMENT
               PERFORM 4120-AGE-CLOSE
               PERFORM 4130-COUNT-PAYMENT
               PERFORM 4140-DESCRIBE-TYPE
               PERFORM 4150-PRORATE-POSTAGE
               PERFORM 4160-FLAG-COPY-FEE
               PERFORM 4170-SET-CHANNEL
               PERFORM 4180-EDIT-MEMO
               PERFORM 4190-ACCUM-RETENTION
               PERFORM 4200-PRORATE-STORAGE
               PERFORM 4210-ACCRUE-RESEARCH
               PERFORM 4220-ACCRUE-COPY-FEE
               PERFORM 4230-ASSESS-EXEMPT
               PERFORM 4240-DESCRIBE-TYPE
               PERFORM 4250-ACCRUE-RESEARCH
               PERFORM 4260-COUNT-ITEM
               PERFORM 4270-FLAG-POSTAGE
               PERFORM 4280-FLAG-POSTAGE
               PERFORM 4290-AGE-LAST-STMT
               PERFORM 4300-SET-STATUS
               PERFORM 4310-DESCRIBE-FICHE-TYPE
               PERFORM 4320-ACCUM-CHANNEL
               PERFORM 4330-ACCRUE-COPY-FEE
               PERFORM 4340-SELECT-MARGIN
               PERFORM 4350-CHECK-LAST-PAY
               PERFORM 4360-ASSESS-AUTO-RENEW
               PERFORM 4370-SET-BRANCH
               PERFORM 4380-COUNT-PAYMENT
               PERFORM 4390-EDIT-CUST-ID
               PERFORM 4400-ACCRUE-COPY-FEE
               PERFORM 4410-PRORATE-COPY-FEE
               PERFORM 4420-DESCRIBE-TRAN
               PERFORM 4430-COUNT-PAYMENT
               PERFORM 4440-SPLIT-RESEARCH
               PERFORM 4450-PRORATE-COPY-FEE
               PERFORM 4460-CHECK-OPEN
               PERFORM 4470-ASSESS-EXEMPT
               PERFORM 4480-LIMIT-COPY-FEE
               PERFORM 4490-PRORATE-COPY-FEE
               PERFORM 4500-ASSESS-REVERSAL
               PERFORM 4510-EDIT-NAME
               PERFORM 4520-ACCUM-FICHE-TYPE
               PERFORM 4530-SELECT-MARGIN
               PERFORM 4540-AGE-EFF
               PERFORM 4550-DESCRIBE-BRANCH
               PERFORM 7030-CALL-MFLIM
               PERFORM 7040-CALL-MFELG
               REWRITE MFC-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'MFCMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM MFC-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH GRID TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3000-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-GRID-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF MFCT-RESEARCH-AMT NOT < WS-RESEARCH-GRID-LOW(WS-TX)
                   MOVE WS-RESEARCH-GRID-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH GRID TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-GRID-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF MFCM-COPY-FEE-YTD NOT < WS-RESEARCH-GRID-LOW(WS-TX)
                   MOVE WS-RESEARCH-GRID-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3020-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCT-CHANNEL-CD
                   ADD MFCM-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE COPY FEE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3030-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN MFCM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = MFCM-COPY-FEE-AMT *
                       MFCM-MARGIN / 200
               WHEN MFCM-FICHE-TYPE-CD = 'AW'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN MFCM-FICHE-TYPE-CD = 'JM'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = MFCM-COPY-FEE-AMT *
                       MFCM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E629 REF NO REQUIRED
      * E075 RESEARCH AMT NEGATIVE
      * E901 RESEARCH AMT OVER 1000000
      * E875 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3040-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-REF-NO = SPACES
                   MOVE 'E629' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-AMT < 0
                   MOVE 'E075' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-AMT > 1000000.00
                   MOVE 'E901' TO WS-REJECT-CODE
               WHEN MFCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E875' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE MFCT-RESEARCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MFCM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO MFCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH FEE ON THE STORAGE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3070-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN MFCM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO MFCT-RESEARCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-STOP-FLAG = 'Y'
                   COMPUTE MFCT-RESEARCH-AMT ROUNDED = MFCM-STORAGE-AMT
                       * MFCM-FEE-PCT / 200
               WHEN MFCM-STATUS-CD = 'AST'
                   MOVE 15.00 TO MFCT-RESEARCH-AMT
               WHEN MFCM-STATUS-CD = 'JXN'
                   MOVE 20.00 TO MFCT-RESEARCH-AMT
               WHEN OTHER
                   COMPUTE MFCT-RESEARCH-AMT ROUNDED = MFCM-STORAGE-AMT
                       * MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCT-RESEARCH-AMT > 0 AND MFCT-RESEARCH-AMT < 5.00
               MOVE 5.00 TO MFCT-RESEARCH-AMT
           END-IF
           IF MFCT-RESEARCH-AMT > 150.00
               MOVE 150.00 TO MFCT-RESEARCH-AMT
           END-IF
           ADD MFCT-RESEARCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RESEARCH AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3080-FLAG-RESEARCH.
           IF MFCM-RESEARCH-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCM-RESEARCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3090-DESCRIBE-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JXN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KJJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TZL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XZM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-BRANCH-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COPY FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = MFCT-COPY-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MFCT-COPY-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE MFCM-COPY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3130-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3140-ACCRUE-STORAGE.
           IF MFCT-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-STORAGE-AMT * MFCM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF MFCM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AST -> JXN WHEN LAST STMT DATE IS PAST
      * JXN -> KJJ WHEN LAST STMT DATE IS PAST
      * KJJ -> TZL WHEN LAST STMT DATE IS PAST
      * TZL -> XZM WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3160-SET-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JXN' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JXN'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJJ' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KJJ'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TZL' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TZL'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZM' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E543 CONTACT REQUIRED
      * E331 RESEARCH YTD NEGATIVE
      * E253 RESEARCH YTD OVER 1000000
      * E669 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3170-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CONTACT = SPACES
                   MOVE 'E543' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-YTD < 0
                   MOVE 'E331' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-YTD > 1000000.00
                   MOVE 'E253' TO WS-REJECT-CODE
               WHEN MFCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E669' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3180-LIMIT-RESEARCH.
           IF MFCT-RESEARCH-AMT < 100.00
               MOVE 100.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-RESEARCH-AMT > 50000.00
               MOVE 50000.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3190-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFCT-POSTAGE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MFCT-POSTAGE-AMT - (WS-WORK-AMT * 2)
           COMPUTE MFCM-RESEARCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E955 CONTACT REQUIRED
      * E344 RESEARCH AMT NEGATIVE
      * E624 RESEARCH AMT OVER 250000
      * E340 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3200-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CONTACT = SPACES
                   MOVE 'E955' TO WS-REJECT-CODE
               WHEN MFCT-RESEARCH-AMT < 0
                   MOVE 'E344' TO WS-REJECT-CODE
               WHEN MFCT-RESEARCH-AMT > 250000.00
                   MOVE 'E624' TO WS-REJECT-CODE
               WHEN MFCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E340' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE FEE ON THE STORAGE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3210-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MFCT-POSTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   COMPUTE MFCT-POSTAGE-AMT ROUNDED = MFCM-STORAGE-YTD *
                       MFCM-MARGIN / 200
               WHEN MFCT-TRAN-CD = 'IX'
                   MOVE 7.00 TO MFCT-POSTAGE-AMT
               WHEN MFCT-TRAN-CD = 'RS'
                   MOVE 10.00 TO MFCT-POSTAGE-AMT
               WHEN OTHER
                   COMPUTE MFCT-POSTAGE-AMT ROUNDED = MFCM-STORAGE-YTD *
                       MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCT-POSTAGE-AMT > 0 AND MFCT-POSTAGE-AMT < 2.00
               MOVE 2.00 TO MFCT-POSTAGE-AMT
           END-IF
           IF MFCT-POSTAGE-AMT > 50.00
               MOVE 50.00 TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN LAST MAINT DATE IS PAST
      * GL -> HH WHEN LAST MAINT DATE IS PAST
      * HH -> LM WHEN LAST MAINT DATE IS PAST
      * LM -> MD WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3220-SET-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE YTD BY RETENTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3230-ACCUM-RETENTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-RETENTION-CD
                   ADD MFCM-STORAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-RETENTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-STORAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-RESEARCH.
           IF MFCT-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-RESEARCH-AMT * MFCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MFCM-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE FEE ON THE POSTAGE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3250-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MFCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MFCT-POSTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE MFCT-POSTAGE-AMT ROUNDED = MFCM-POSTAGE-YTD *
                       MFCM-MARGIN / 200
               WHEN MFCM-STATUS-CD = 'AST'
                   MOVE 7.00 TO MFCT-POSTAGE-AMT
               WHEN MFCM-STATUS-CD = 'JXN'
                   MOVE 12.00 TO MFCT-POSTAGE-AMT
               WHEN OTHER
                   COMPUTE MFCT-POSTAGE-AMT ROUNDED = MFCM-POSTAGE-YTD *
                       MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCT-POSTAGE-AMT > 0 AND MFCT-POSTAGE-AMT < 10.00
               MOVE 10.00 TO MFCT-POSTAGE-AMT
           END-IF
           IF MFCT-POSTAGE-AMT > 100.00
               MOVE 100.00 TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE RESEARCH AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3260-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = MFCM-RESEARCH-AMT *
                       MFCM-DISCOUNT / 200
               WHEN MFCT-TRAN-CD = 'IX'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN MFCT-TRAN-CD = 'RS'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = MFCM-RESEARCH-AMT *
                       MFCM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH GRID TIER FOR THE RESEARCH YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-GRID-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF MFCM-RESEARCH-YTD NOT < WS-RESEARCH-GRID-LOW(WS-TX)
                   MOVE WS-RESEARCH-GRID-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE POSTAGE SCHED TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-POSTAGE-SCHED-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MFCM-COPY-FEE-YTD NOT < WS-POSTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-POSTAGE-SCHED-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COPY FEE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3290-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-BRANCH-CD
                   ADD MFCT-COPY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCT-COPY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-PAYMENT.
           IF MFCM-PAYMENT-CNT < 9999999
               ADD 1 TO MFCM-PAYMENT-CNT
           END-IF
           IF MFCM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO MFCM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF MFCT-POST-DATE > 0 AND MFCT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFCM-RESEARCH-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MFCM-RESEARCH-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MFCM-RESEARCH-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MFCM-RESEARCH-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-RESEARCH-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE POSTAGE SCHED TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-POSTAGE-SCHED-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MFCM-RESEARCH-AMT NOT < WS-POSTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-POSTAGE-SCHED-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       3340-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-POSTAGE-AMT ROUNDED =
                   MFCM-STORAGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF MFCM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO MFCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST STMT DATE IS PAST
      * BAT -> MAL WHEN LAST STMT DATE IS PAST
      * MAL -> TEL WHEN LAST STMT DATE IS PAST
      * TEL -> ONL WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3360-SET-CHANNEL.
           EVALUATE MFCT-CHANNEL-CD
               WHEN 'BRN'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-CHANNEL-CD
                   ADD MFCM-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COPY FEE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3380-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCT-TRAN-CD
                   ADD MFCM-COPY-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-COPY-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3390-DESCRIBE-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ND'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3400-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF MFCT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO MFCM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-SEQ.
           IF MFCT-SEQ < 9999999
               ADD 1 TO MFCT-SEQ
           END-IF
           IF MFCT-SEQ NOT < 6
               MOVE 'Y' TO MFCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3430-DESCRIBE-BRANCH.
           EVALUATE MFCM-BRANCH-CD
               WHEN 'AT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'US'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MFCM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3450-ACCRUE-RESEARCH.
           IF MFCM-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-RESEARCH-AMT * MFCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MFCT-RESEARCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3460-DESCRIBE-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JXN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KJJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TZL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XZM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR RETENTION CD:
      * KT -> SJ WHEN OPEN DATE IS PAST
      * SJ -> WV WHEN OPEN DATE IS PAST
      * WV -> KT WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3470-SET-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SJ' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SJ'
                   IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       3480-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-POSTAGE-AMT ROUNDED =
                   MFCM-POSTAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       3490-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-POSTAGE-AMT ROUNDED =
                   MFCM-POSTAGE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3500-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MFCM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO MFCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-SEQ.
           IF MFCT-SEQ < 9999999
               ADD 1 TO MFCT-SEQ
           END-IF
           IF MFCT-SEQ NOT < 5
               MOVE 'Y' TO MFCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE FEE ON THE POSTAGE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3520-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MFCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MFCT-POSTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   COMPUTE MFCT-POSTAGE-AMT ROUNDED = MFCM-POSTAGE-YTD *
                       MFCM-DISCOUNT / 200
               WHEN MFCT-TRAN-CD = 'IX'
                   MOVE 15.00 TO MFCT-POSTAGE-AMT
               WHEN MFCT-TRAN-CD = 'RS'
                   MOVE 10.00 TO MFCT-POSTAGE-AMT
               WHEN OTHER
                   COMPUTE MFCT-POSTAGE-AMT ROUNDED = MFCM-POSTAGE-YTD *
                       MFCM-DISCOUNT / 100
           END-EVALUATE
           IF MFCT-POSTAGE-AMT > 0 AND MFCT-POSTAGE-AMT < 5.00
               MOVE 5.00 TO MFCT-POSTAGE-AMT
           END-IF
           IF MFCT-POSTAGE-AMT > 75.00
               MOVE 75.00 TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFCM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO MFCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR FICHE TYPE CD:
      * AW -> JM WHEN MATURITY DATE IS PAST
      * JM -> KV WHEN MATURITY DATE IS PAST
      * KV -> MD WHEN MATURITY DATE IS PAST
      * MD -> NR WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3540-SET-FICHE-TYPE.
           EVALUATE MFCM-FICHE-TYPE-CD
               WHEN 'AW'
                   IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MD'
                   IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NR' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE COPY FEE YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3550-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-TYPE-CD
                   ADD MFCM-COPY-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-COPY-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3560-FLAG-POSTAGE.
           IF MFCT-POSTAGE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFCT-POSTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3570-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E093 REJECT CD REQUIRED
      * E716 STORAGE AMT NEGATIVE
      * E304 STORAGE AMT OVER 25000
      * E589 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3580-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-REJECT-CD = SPACES
                   MOVE 'E093' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-AMT < 0
                   MOVE 'E716' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-AMT > 25000.00
                   MOVE 'E304' TO WS-REJECT-CODE
               WHEN MFCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E589' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-PAYMENT.
           IF MFCM-PAYMENT-CNT < 9999999
               ADD 1 TO MFCM-PAYMENT-CNT
           END-IF
           IF MFCM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO MFCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE YTD FEE ON THE COPY FEE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3600-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN MFCM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO MFCM-POSTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE MFCM-POSTAGE-YTD ROUNDED = MFCM-COPY-FEE-YTD
                       * MFCM-DISCOUNT / 200
               WHEN MFCT-TRAN-CD = 'IX'
                   MOVE 12.00 TO MFCM-POSTAGE-YTD
               WHEN MFCT-TRAN-CD = 'RS'
                   MOVE 20.00 TO MFCM-POSTAGE-YTD
               WHEN OTHER
                   COMPUTE MFCM-POSTAGE-YTD ROUNDED = MFCM-COPY-FEE-YTD
                       * MFCM-DISCOUNT / 100
           END-EVALUATE
           IF MFCM-POSTAGE-YTD > 0 AND MFCM-POSTAGE-YTD < 1.00
               MOVE 1.00 TO MFCM-POSTAGE-YTD
           END-IF
           IF MFCM-POSTAGE-YTD > 35.00
               MOVE 35.00 TO MFCM-POSTAGE-YTD
           END-IF
           ADD MFCM-POSTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3610-FLAG-STORAGE.
           IF MFCM-STORAGE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFCM-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E495 ADDR REQUIRED
      * E331 WORK AMT 2 NEGATIVE
      * E512 WORK AMT 2 OVER 10000
      * E615 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3620-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-ADDR = SPACES
                   MOVE 'E495' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E331' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E512' TO WS-REJECT-CODE
               WHEN MFCM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE POSTAGE AMT BY FICHE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3630-ACCUM-FICHE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-FICHE-TYPE-CD
                   ADD MFCT-POSTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-FICHE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCT-POSTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * G -> K WHEN NOTICE DATE IS PAST
      * K -> W WHEN NOTICE DATE IS PAST
      * W -> Y WHEN NOTICE DATE IS PAST
      * Y -> G WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3640-SET-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   IF MFCM-NOTICE-DATE > 0 AND MFCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF MFCM-NOTICE-DATE > 0 AND MFCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF MFCM-NOTICE-DATE > 0 AND MFCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF MFCM-NOTICE-DATE > 0 AND MFCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3650-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE MFCM-RESEARCH-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3660-FLAG-POSTAGE.
           IF MFCM-POSTAGE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MFCM-POSTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3670-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCM-RESEARCH-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MFCM-RESEARCH-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3680-DESCRIBE-CHANNEL.
           EVALUATE MFCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3690-DESCRIBE-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ND'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3700-DESCRIBE-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE GRID TIER FOR THE POSTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3710-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-GRID-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MFCM-POSTAGE-YTD NOT < WS-STORAGE-GRID-LOW(WS-TX)
                   MOVE WS-STORAGE-GRID-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3720-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCT-RESEARCH-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MFCT-RESEARCH-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3730-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MFCM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE FICHE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3740-DESCRIBE-FICHE-TYPE.
           EVALUATE MFCM-FICHE-TYPE-CD
               WHEN 'AW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3750-ACCRUE-RESEARCH.
           IF MFCT-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-RESEARCH-AMT * MFCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO MFCT-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COPY FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = MFCT-COPY-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MFCT-COPY-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE MFCM-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE POSTAGE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3770-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCT-CHANNEL-CD
                   ADD MFCM-POSTAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-POSTAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-RESEARCH.
           IF MFCM-RESEARCH-YTD < 10.00
               MOVE 10.00 TO MFCM-RESEARCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-RESEARCH-YTD > 50000.00
               MOVE 50000.00 TO MFCM-RESEARCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E253 DESC REQUIRED
      * E095 POSTAGE AMT NEGATIVE
      * E409 POSTAGE AMT OVER 50000
      * E928 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-DESC = SPACES
                   MOVE 'E253' TO WS-REJECT-CODE
               WHEN MFCT-POSTAGE-AMT < 0
                   MOVE 'E095' TO WS-REJECT-CODE
               WHEN MFCT-POSTAGE-AMT > 50000.00
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN MFCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E928' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3800-ACCRUE-STORAGE.
           IF MFCM-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-STORAGE-AMT * MFCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MFCM-COPY-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3810-LIMIT-STORAGE.
           IF MFCM-STORAGE-AMT < 25.00
               MOVE 25.00 TO MFCM-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-STORAGE-AMT > 1000.00
               MOVE 1000.00 TO MFCM-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3820-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESEARCH YTD.
      *----------------------------------------------------------------*
       3830-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-RESEARCH-YTD ROUNDED =
                   MFCT-COPY-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCM-RESEARCH-YTD
           END-IF
           ADD MFCM-RESEARCH-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE COPY FEE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3840-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-BRANCH-CD
                   ADD MFCM-COPY-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-COPY-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3850-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE MFCM-RESEARCH-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3860-FLAG-COPY-FEE.
           IF MFCT-COPY-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCT-COPY-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE YTD BY RETENTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3870-ACCUM-RETENTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-RETENTION-CD
                   ADD MFCM-STORAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-RETENTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-STORAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ND'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-CHANNEL-CD
                   ADD MFCT-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCT-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3900-FLAG-POSTAGE.
           IF MFCM-POSTAGE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCM-POSTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       3910-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-POSTAGE-AMT ROUNDED =
                   MFCM-RESEARCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3920-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCT-CHANNEL-CD
                   ADD MFCM-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH YTD FEE ON THE STORAGE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3930-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MFCM-RESEARCH-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = MFCM-STORAGE-AMT
                       * MFCM-MARGIN / 200
               WHEN MFCT-TRAN-CD = 'IX'
                   MOVE 10.00 TO MFCM-RESEARCH-YTD
               WHEN MFCT-TRAN-CD = 'RS'
                   MOVE 10.00 TO MFCM-RESEARCH-YTD
               WHEN OTHER
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = MFCM-STORAGE-AMT
                       * MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCM-RESEARCH-YTD > 0 AND MFCM-RESEARCH-YTD < 10.00
               MOVE 10.00 TO MFCM-RESEARCH-YTD
           END-IF
           IF MFCM-RESEARCH-YTD > 25.00
               MOVE 25.00 TO MFCM-RESEARCH-YTD
           END-IF
           ADD MFCM-RESEARCH-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3940-DESCRIBE-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3950-LIMIT-STORAGE.
           IF MFCM-STORAGE-AMT < 10.00
               MOVE 10.00 TO MFCM-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-STORAGE-AMT > 50000.00
               MOVE 50000.00 TO MFCM-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3960-FLAG-RESEARCH.
           IF MFCM-RESEARCH-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCM-RESEARCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E417 ADDR REQUIRED
      * E213 POSTAGE AMT NEGATIVE
      * E931 POSTAGE AMT OVER 10000
      * E130 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-ADDR = SPACES
                   MOVE 'E417' TO WS-REJECT-CODE
               WHEN MFCM-POSTAGE-AMT < 0
                   MOVE 'E213' TO WS-REJECT-CODE
               WHEN MFCM-POSTAGE-AMT > 10000.00
                   MOVE 'E931' TO WS-REJECT-CODE
               WHEN MFCM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E130' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE FEE ON THE RESEARCH YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3980-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MFCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MFCT-POSTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE MFCT-POSTAGE-AMT ROUNDED = MFCM-RESEARCH-YTD
                       * MFCM-MARGIN / 200
               WHEN MFCM-STATUS-CD = 'AST'
                   MOVE 3.00 TO MFCT-POSTAGE-AMT
               WHEN MFCM-STATUS-CD = 'JXN'
                   MOVE 3.00 TO MFCT-POSTAGE-AMT
               WHEN OTHER
                   COMPUTE MFCT-POSTAGE-AMT ROUNDED = MFCM-RESEARCH-YTD
                       * MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCT-POSTAGE-AMT > 0 AND MFCT-POSTAGE-AMT < 10.00
               MOVE 10.00 TO MFCT-POSTAGE-AMT
           END-IF
           IF MFCT-POSTAGE-AMT > 25.00
               MOVE 25.00 TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3990-ACCRUE-RESEARCH.
           IF MFCT-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-RESEARCH-AMT * MFCM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO MFCT-POSTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE GRID TIER FOR THE POSTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4000-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-GRID-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MFCM-POSTAGE-YTD NOT < WS-STORAGE-GRID-LOW(WS-TX)
                   MOVE WS-STORAGE-GRID-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE POSTAGE YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-BRANCH-CD
                   ADD MFCM-POSTAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-POSTAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4020-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COPY FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = MFCM-COPY-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MFCM-COPY-FEE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE MFCM-COPY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4040-COUNT-PAYMENT.
           IF MFCM-PAYMENT-CNT < 9999999
               ADD 1 TO MFCM-PAYMENT-CNT
           END-IF
           IF MFCM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO MFCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4050-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       MFCM-MARGIN / 200
               WHEN MFCM-FICHE-TYPE-CD = 'AW'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN MFCM-FICHE-TYPE-CD = 'JM'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = WS-WORK-AMT *
                       MFCM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E422 MEMO REQUIRED
      * E161 RESEARCH AMT NEGATIVE
      * E089 RESEARCH AMT OVER 250000
      * E503 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4060-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-MEMO = SPACES
                   MOVE 'E422' TO WS-REJECT-CODE
               WHEN MFCT-RESEARCH-AMT < 0
                   MOVE 'E161' TO WS-REJECT-CODE
               WHEN MFCT-RESEARCH-AMT > 250000.00
                   MOVE 'E089' TO WS-REJECT-CODE
               WHEN MFCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E503' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       4070-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-POSTAGE-AMT ROUNDED =
                   MFCM-COPY-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4080-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE GRID TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4090-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-GRID-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MFCM-STORAGE-AMT NOT < WS-STORAGE-GRID-LOW(WS-TX)
                   MOVE WS-STORAGE-GRID-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * G -> K WHEN MATURITY DATE IS PAST
      * K -> W WHEN MATURITY DATE IS PAST
      * W -> Y WHEN MATURITY DATE IS PAST
      * Y -> G WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4100-SET-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4110-COUNT-PAYMENT.
           IF MFCM-PAYMENT-CNT < 9999999
               ADD 1 TO MFCM-PAYMENT-CNT
           END-IF
           IF MFCM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO MFCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4120-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4130-COUNT-PAYMENT.
           IF MFCM-PAYMENT-CNT < 9999999
               ADD 1 TO MFCM-PAYMENT-CNT
           END-IF
           IF MFCM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO MFCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4140-DESCRIBE-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'K'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-POSTAGE-AMT ROUNDED =
                   MFCM-COPY-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * COPY FEE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4160-FLAG-COPY-FEE.
           IF MFCM-COPY-FEE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MFCM-COPY-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4170-SET-CHANNEL.
           EVALUATE MFCT-CHANNEL-CD
               WHEN 'BRN'
                   IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E244 MEMO REQUIRED
      * E011 WORK AMT 2 NEGATIVE
      * E906 WORK AMT 2 OVER 99999
      * E643 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4180-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-MEMO = SPACES
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E011' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 99999.00
                   MOVE 'E906' TO WS-REJECT-CODE
               WHEN MFCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E643' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY RETENTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4190-ACCUM-RETENTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-RETENTION-CD
                   ADD MFCM-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-RETENTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       4200-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-STORAGE-AMT ROUNDED =
                   MFCT-POSTAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCT-STORAGE-AMT
           END-IF
           ADD MFCT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4210-ACCRUE-RESEARCH.
           IF MFCT-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-RESEARCH-AMT * MFCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COPY FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4220-ACCRUE-COPY-FEE.
           IF MFCT-COPY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-COPY-FEE-AMT * MFCM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO MFCM-STORAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH FEE ON THE COPY FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4230-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MFCT-RESEARCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-STOP-FLAG = 'Y'
                   COMPUTE MFCT-RESEARCH-AMT ROUNDED = MFCM-COPY-FEE-AMT
                       * MFCM-MARGIN / 200
               WHEN MFCM-STATUS-CD = 'AST'
                   MOVE 5.00 TO MFCT-RESEARCH-AMT
               WHEN MFCM-STATUS-CD = 'JXN'
                   MOVE 7.00 TO MFCT-RESEARCH-AMT
               WHEN OTHER
                   COMPUTE MFCT-RESEARCH-AMT ROUNDED = MFCM-COPY-FEE-AMT
                       * MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCT-RESEARCH-AMT > 0 AND MFCT-RESEARCH-AMT < 5.00
               MOVE 5.00 TO MFCT-RESEARCH-AMT
           END-IF
           IF MFCT-RESEARCH-AMT > 250.00
               MOVE 250.00 TO MFCT-RESEARCH-AMT
           END-IF
           ADD MFCT-RESEARCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4240-DESCRIBE-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-RESEARCH.
           IF MFCM-RESEARCH-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-RESEARCH-YTD * MFCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4260-COUNT-ITEM.
           IF MFCM-ITEM-CNT < 9999999
               ADD 1 TO MFCM-ITEM-CNT
           END-IF
           IF MFCM-ITEM-CNT NOT < 10
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4270-FLAG-POSTAGE.
           IF MFCT-POSTAGE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFCT-POSTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4280-FLAG-POSTAGE.
           IF MFCM-POSTAGE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCM-POSTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4290-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AST -> JXN WHEN LAST MAINT DATE IS PAST
      * JXN -> KJJ WHEN LAST MAINT DATE IS PAST
      * KJJ -> TZL WHEN LAST MAINT DATE IS PAST
      * TZL -> XZM WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4300-SET-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JXN' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JXN'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KJJ' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KJJ'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TZL' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TZL'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XZM' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE FICHE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4310-DESCRIBE-FICHE-TYPE.
           EVALUATE MFCM-FICHE-TYPE-CD
               WHEN 'AW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4320-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCT-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COPY FEE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4330-ACCRUE-COPY-FEE.
           IF MFCM-COPY-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-COPY-FEE-YTD * MFCM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO MFCT-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE GRID TIER FOR THE COPY FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4340-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-GRID-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MFCM-COPY-FEE-AMT NOT < WS-STORAGE-GRID-LOW(WS-TX)
                   MOVE WS-STORAGE-GRID-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4350-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MFCM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO MFCM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH YTD FEE ON THE POSTAGE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4360-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MFCM-RESEARCH-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = MFCM-POSTAGE-YTD
                       * MFCM-DISCOUNT / 200
               WHEN MFCM-CHANNEL-CD = 'FV'
                   MOVE 7.00 TO MFCM-RESEARCH-YTD
               WHEN MFCM-CHANNEL-CD = 'GL'
                   MOVE 15.00 TO MFCM-RESEARCH-YTD
               WHEN OTHER
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = MFCM-POSTAGE-YTD
                       * MFCM-DISCOUNT / 100
           END-EVALUATE
           IF MFCM-RESEARCH-YTD > 0 AND MFCM-RESEARCH-YTD < 5.00
               MOVE 5.00 TO MFCM-RESEARCH-YTD
           END-IF
           IF MFCM-RESEARCH-YTD > 50.00
               MOVE 50.00 TO MFCM-RESEARCH-YTD
           END-IF
           ADD MFCM-RESEARCH-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * AT -> KR WHEN POST DATE IS PAST
      * KR -> LK WHEN POST DATE IS PAST
      * LK -> US WHEN POST DATE IS PAST
      * US -> AT WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4370-SET-BRANCH.
           EVALUATE MFCM-BRANCH-CD
               WHEN 'AT'
                   IF MFCT-POST-DATE > 0 AND MFCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF MFCT-POST-DATE > 0 AND MFCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LK' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LK'
                   IF MFCT-POST-DATE > 0 AND MFCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF MFCT-POST-DATE > 0 AND MFCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4380-COUNT-PAYMENT.
           IF MFCM-PAYMENT-CNT < 9999999
               ADD 1 TO MFCM-PAYMENT-CNT
           END-IF
           IF MFCM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E545 CUST ID REQUIRED
      * E955 POSTAGE AMT NEGATIVE
      * E924 POSTAGE AMT OVER 1000000
      * E618 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4390-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CUST-ID = SPACES
                   MOVE 'E545' TO WS-REJECT-CODE
               WHEN MFCM-POSTAGE-AMT < 0
                   MOVE 'E955' TO WS-REJECT-CODE
               WHEN MFCM-POSTAGE-AMT > 1000000.00
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN MFCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E618' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COPY FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4400-ACCRUE-COPY-FEE.
           IF MFCT-COPY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-COPY-FEE-AMT * MFCM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO MFCM-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COPY FEE YTD.
      *----------------------------------------------------------------*
       4410-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-COPY-FEE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCM-COPY-FEE-YTD
           END-IF
           ADD MFCM-COPY-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4420-DESCRIBE-TRAN.
           EVALUATE MFCT-TRAN-CD
               WHEN 'IX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4430-COUNT-PAYMENT.
           IF MFCM-PAYMENT-CNT < 9999999
               ADD 1 TO MFCM-PAYMENT-CNT
           END-IF
           IF MFCM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO MFCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4440-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCT-RESEARCH-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MFCT-RESEARCH-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COPY FEE AMT.
      *----------------------------------------------------------------*
       4450-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-COPY-FEE-AMT ROUNDED =
                   MFCM-COPY-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-COPY-FEE-AMT
           END-IF
           ADD MFCT-COPY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4460-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MFCM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO MFCM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE STORAGE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4470-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MFCT-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MFCT-STORAGE-AMT ROUNDED = MFCM-STORAGE-AMT *
                       MFCM-MARGIN / 200
               WHEN MFCM-STATUS-CD = 'AST'
                   MOVE 3.00 TO MFCT-STORAGE-AMT
               WHEN MFCM-STATUS-CD = 'JXN'
                   MOVE 15.00 TO MFCT-STORAGE-AMT
               WHEN OTHER
                   COMPUTE MFCT-STORAGE-AMT ROUNDED = MFCM-STORAGE-AMT *
                       MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCT-STORAGE-AMT > 0 AND MFCT-STORAGE-AMT < 1.00
               MOVE 1.00 TO MFCT-STORAGE-AMT
           END-IF
           IF MFCT-STORAGE-AMT > 150.00
               MOVE 150.00 TO MFCT-STORAGE-AMT
           END-IF
           ADD MFCT-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * COPY FEE YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4480-LIMIT-COPY-FEE.
           IF MFCM-COPY-FEE-YTD < 50.00
               MOVE 50.00 TO MFCM-COPY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-COPY-FEE-YTD > 2500.00
               MOVE 2500.00 TO MFCM-COPY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COPY FEE AMT.
      *----------------------------------------------------------------*
       4490-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-COPY-FEE-AMT ROUNDED =
                   MFCM-RESEARCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCM-COPY-FEE-AMT
           END-IF
           ADD MFCM-COPY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH FEE ON THE STORAGE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4500-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO MFCT-RESEARCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE MFCT-RESEARCH-AMT ROUNDED = MFCT-STORAGE-AMT
                       * MFCM-FEE-PCT / 200
               WHEN MFCM-BRANCH-CD = 'AT'
                   MOVE 12.00 TO MFCT-RESEARCH-AMT
               WHEN MFCM-BRANCH-CD = 'KR'
                   MOVE 7.00 TO MFCT-RESEARCH-AMT
               WHEN OTHER
                   COMPUTE MFCT-RESEARCH-AMT ROUNDED = MFCT-STORAGE-AMT
                       * MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCT-RESEARCH-AMT > 0 AND MFCT-RESEARCH-AMT < 1.00
               MOVE 1.00 TO MFCT-RESEARCH-AMT
           END-IF
           IF MFCT-RESEARCH-AMT > 50.00
               MOVE 50.00 TO MFCT-RESEARCH-AMT
           END-IF
           ADD MFCT-RESEARCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E756 NAME REQUIRED
      * E086 COPY FEE YTD NEGATIVE
      * E900 COPY FEE YTD OVER 10000
      * E232 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4510-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-NAME = SPACES
                   MOVE 'E756' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-YTD < 0
                   MOVE 'E086' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-YTD > 10000.00
                   MOVE 'E900' TO WS-REJECT-CODE
               WHEN MFCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E232' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COPY FEE AMT BY FICHE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4520-ACCUM-FICHE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-FICHE-TYPE-CD
                   ADD MFCM-COPY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-FICHE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-COPY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH GRID TIER FOR THE STORAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4530-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-GRID-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF MFCM-STORAGE-YTD NOT < WS-RESEARCH-GRID-LOW(WS-TX)
                   MOVE WS-RESEARCH-GRID-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4540-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4550-DESCRIBE-BRANCH.
           EVALUATE MFCM-BRANCH-CD
               WHEN 'AT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'US'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE MFC FIELDS TO MFLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-MFLIM.
           INITIALIZE MFLIM-PARMS
           MOVE MFCM-COPY-FEE-YTD
             TO MFLIM-COPY-FEE-YTD
           MOVE MFCM-STORAGE-AMT
             TO MFLIM-STORAGE-AMT
           MOVE MFCM-STORAGE-YTD
             TO MFLIM-STORAGE-YTD
           MOVE MFCM-RESEARCH-YTD
             TO MFLIM-RESEARCH-YTD
           MOVE MFCM-POSTAGE-AMT
             TO MFLIM-POSTAGE-AMT
           MOVE MFCM-POSTAGE-YTD
             TO MFLIM-POSTAGE-YTD
           MOVE MFCM-CLOSE-DATE
             TO MFLIM-CLOSE-DATE
           MOVE MFCM-EXPIRY-DATE
             TO MFLIM-EXPIRY-DATE
           MOVE MFCM-LAST-PAY-DATE
             TO MFLIM-LAST-PAY-DATE
           MOVE MFCM-OPEN-DATE
             TO MFLIM-OPEN-DATE
           MOVE MFCM-TYPE-CD
             TO MFLIM-TYPE-CD
           MOVE MFCM-RETENTION-CD
             TO MFLIM-RETENTION-CD
           MOVE MFCM-CHANNEL-CD
             TO MFLIM-CHANNEL-CD
           MOVE MFCM-BRANCH-CD
             TO MFLIM-BRANCH-CD
           MOVE MFCM-REVIEW-FLAG
             TO MFLIM-REVIEW-FLAG
           MOVE MFCM-STOP-FLAG
             TO MFLIM-STOP-FLAG
           MOVE MFCM-NOTICE-SENT-FLAG
             TO MFLIM-NOTICE-SENT-FLAG
           MOVE MFCM-CYCLE-CNT
             TO MFLIM-CYCLE-CNT
           MOVE MFCM-PAYMENT-CNT
             TO MFLIM-PAYMENT-CNT
           MOVE MFCM-DISCOUNT
             TO MFLIM-DISCOUNT
           MOVE MFCM-MARGIN
             TO MFLIM-MARGIN
           MOVE MFCM-ADDR
             TO MFLIM-ADDR
           MOVE MFCM-CONTACT
             TO MFLIM-CONTACT
           CALL 'MFLIM' USING MFLIM-PARMS
           EVALUATE MFLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE MFLIM-COPY-FEE-YTD
                     TO MFCM-COPY-FEE-YTD
                   MOVE MFLIM-STORAGE-AMT
                     TO MFCM-STORAGE-AMT
                   MOVE MFLIM-STORAGE-YTD
                     TO MFCM-STORAGE-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-MFLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE MFC FIELDS TO MFELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-MFELG.
           INITIALIZE MFELG-PARMS
           MOVE MFCM-POSTAGE-YTD
             TO MFELG-POSTAGE-YTD
           MOVE MFCM-POSTAGE-AMT
             TO MFELG-POSTAGE-AMT
           MOVE MFCM-RESEARCH-AMT
             TO MFELG-RESEARCH-AMT
           MOVE MFCM-COPY-FEE-AMT
             TO MFELG-COPY-FEE-AMT
           MOVE MFCM-STORAGE-AMT
             TO MFELG-STORAGE-AMT
           MOVE MFCM-COPY-FEE-YTD
             TO MFELG-COPY-FEE-YTD
           MOVE MFCM-NOTICE-DATE
             TO MFELG-NOTICE-DATE
           MOVE MFCM-LAST-STMT-DATE
             TO MFELG-LAST-STMT-DATE
           MOVE MFCM-CLOSE-DATE
             TO MFELG-CLOSE-DATE
           MOVE MFCM-MATURITY-DATE
             TO MFELG-MATURITY-DATE
           MOVE MFCM-BRANCH-CD
             TO MFELG-BRANCH-CD
           MOVE MFCM-CHANNEL-CD
             TO MFELG-CHANNEL-CD
           MOVE MFCM-TYPE-CD
             TO MFELG-TYPE-CD
           MOVE MFCM-FICHE-TYPE-CD
             TO MFELG-FICHE-TYPE-CD
           MOVE MFCM-EXEMPT-FLAG
             TO MFELG-EXEMPT-FLAG
           MOVE MFCM-NOTICE-SENT-FLAG
             TO MFELG-NOTICE-SENT-FLAG
           MOVE MFCM-REVIEW-FLAG
             TO MFELG-REVIEW-FLAG
           MOVE MFCM-ITEM-CNT
             TO MFELG-ITEM-CNT
           MOVE MFCM-PAYMENT-CNT
             TO MFELG-PAYMENT-CNT
           MOVE MFCM-DISCOUNT
             TO MFELG-DISCOUNT
           MOVE MFCM-MARGIN
             TO MFELG-MARGIN
           MOVE MFCM-ADDR
             TO MFELG-ADDR
           MOVE MFCM-CUST-ID
             TO MFELG-CUST-ID
           CALL 'MFELG' USING MFELG-PARMS
           EVALUATE MFELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE MFELG-POSTAGE-YTD
                     TO MFCM-POSTAGE-YTD
                   MOVE MFELG-RESEARCH-AMT
                     TO MFCM-RESEARCH-AMT
                   MOVE MFELG-COPY-FEE-AMT
                     TO MFCM-COPY-FEE-AMT
                   MOVE MFELG-STORAGE-AMT
                     TO MFCM-STORAGE-AMT
                   MOVE MFELG-NOTICE-SENT-FLAG
                     TO MFCM-NOTICE-SENT-FLAG
                   MOVE MFELG-REVIEW-FLAG
                     TO MFCM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-MFELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE MFCT-KEY TO RD-KEY
           MOVE MFCT-TRAN-CD TO RD-CODE
           MOVE MFCT-STORAGE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE MFCT-KEY TO RD-KEY
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
           DISPLAY 'MFCAGE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'MFCAGE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
