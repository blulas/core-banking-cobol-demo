      *================================================================*
      * PROGRAM  : MFCAUDT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MICROFICHE STATEMENTS - AUDIT TRAIL. READS THE MFC  *
      *            TRANSACTIONS, UPDATES THE MFC MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. STATEMENT MICROFICHE INDEXING,  *
      *            RETENTION AND RESEARCH FEES                         *
      * INPUT    : MFCMST (KSDS), MFCTRN, BANKCTL                      *
      * OUTPUT   : MFCOUT, MFCAURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, MFDTE, MFRISK, MFLIM    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-12-15 SKN  ORIGINAL                                       *
      * 1999-04-09 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-04-22 PQR  RESEARCH RERUN PROTECTION INC0827              *
      * 2000-03-08 DPS  COPY-FEE NOTICE TEXT PER LEGAL PRB3572         *
      * 2005-06-28 JLM  STORAGE LIMIT REVISED REQ7071                  *
      * 2007-10-10 MKL  RESEARCH STATUS CODES ADDED INC9490            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFCAUDT.
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
           SELECT REPORT-FILE ASSIGN TO "MFCAURPT"
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
                                           VALUE 16.000000.
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
      * WS-STORAGE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000856500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000372500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000611000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000521000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000436500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000329000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021500000000736500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031500000000289000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031600000000491000'.
       01  WS-STORAGE-SCHED-TBL REDEFINES WS-STORAGE-SCHED-VALUES.
           05  WS-STORAGE-SCHED-ROW OCCURS 9 TIMES.
               10  WS-STORAGE-SCHED-LOW    PIC 9(09)V99.
               10  WS-STORAGE-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COPY-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COPY-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000737000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000222000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000344000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000400000000752000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000713000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000335000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001550000000254000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011550000000028000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012050000000900000'.
       01  WS-COPY-FEE-TIER-TBL REDEFINES WS-COPY-FEE-TIER-VALUES.
           05  WS-COPY-FEE-TIER-ROW OCCURS 9 TIMES.
               10  WS-COPY-FEE-TIER-LOW    PIC 9(09)V99.
               10  WS-COPY-FEE-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RESEARCH-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RESEARCH-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000461000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000753500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000048000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000703000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014000000000726000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024000000000412000'.
       01  WS-RESEARCH-SCHED-TBL REDEFINES WS-RESEARCH-SCHED-VALUES.
           05  WS-RESEARCH-SCHED-ROW OCCURS 6 TIMES.
               10  WS-RESEARCH-SCHED-LOW   PIC 9(09)V99.
               10  WS-RESEARCH-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STORAGE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000522500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000397500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000595500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000410000'.
       01  WS-STORAGE-BAND-TBL REDEFINES WS-STORAGE-BAND-VALUES.
           05  WS-STORAGE-BAND-ROW OCCURS 4 TIMES.
               10  WS-STORAGE-BAND-LOW     PIC 9(09)V99.
               10  WS-STORAGE-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  MFCAUDT  MICROFICHE STATEMENTS'.
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
       COPY MFDTEP.
       COPY MFRISKP.
       COPY MFLIMP.

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
               PERFORM 3000-ACCRUE-POSTAGE
               PERFORM 3010-CHECK-OPEN
               PERFORM 3020-ACCRUE-COPY-FEE
               PERFORM 3030-ASSESS-HOLD
               PERFORM 3040-ASSESS-STOP
               PERFORM 3050-AGE-CLOSE
               PERFORM 3060-LIMIT-COPY-FEE
               PERFORM 3070-PRORATE-STORAGE
               PERFORM 3080-DESCRIBE-STATUS
               PERFORM 3090-SET-RETENTION
               PERFORM 3100-CHECK-NOTICE
               PERFORM 3110-PRORATE-RESEARCH
               PERFORM 3120-SPLIT-RESEARCH
               PERFORM 3130-LIMIT-STORAGE
               PERFORM 3140-LIMIT-STORAGE
               PERFORM 3150-SPLIT-RESEARCH
               PERFORM 3160-PRORATE-RESEARCH
               PERFORM 3170-SET-RETENTION
               PERFORM 3180-LIMIT-ITEM
               PERFORM 3190-LIMIT-POSTAGE
               PERFORM 3200-ACCUM-RETENTION
               PERFORM 3210-ASSESS-HOLD
               PERFORM 3220-COUNT-CYCLE
               PERFORM 3230-LIMIT-POSTAGE
               PERFORM 3240-ACCUM-TYPE
               PERFORM 3250-ASSESS-NOTICE-SENT
               PERFORM 3260-ASSESS-AUTO-RENEW
               PERFORM 3270-PRORATE-POSTAGE
               PERFORM 3280-FLAG-STORAGE
               PERFORM 3290-PRORATE-RESEARCH
               PERFORM 3300-ACCUM-STATUS
               PERFORM 3310-DESCRIBE-TYPE
               PERFORM 3320-FLAG-ITEM
               PERFORM 3330-CHECK-EFF
               PERFORM 3340-ACCUM-TYPE
               PERFORM 3350-FLAG-STORAGE
               PERFORM 3360-DESCRIBE-CHANNEL
               PERFORM 3370-CHECK-LAST-MAINT
               PERFORM 3380-LIMIT-STORAGE
               PERFORM 3390-SET-CHANNEL
               PERFORM 3400-SPLIT-ITEM
               PERFORM 3410-SPLIT-RESEARCH
               PERFORM 3420-SELECT-FEE-PCT
               PERFORM 3430-LIMIT-RESEARCH
               PERFORM 3440-ACCRUE-MARGIN
               PERFORM 3450-PRORATE-COPY-FEE
               PERFORM 3460-CHECK-MATURITY
               PERFORM 3470-ASSESS-NOTICE-SENT
               PERFORM 3480-DESCRIBE-RETENTION
               PERFORM 3490-EDIT-ADDR
               PERFORM 3500-EDIT-ADDR
               PERFORM 3510-FLAG-POSTAGE
               PERFORM 3520-EDIT-NAME
               PERFORM 3530-SELECT-DISCOUNT
               PERFORM 3540-ACCUM-CHANNEL
               PERFORM 3550-COUNT-CYCLE
               PERFORM 3560-COUNT-NOTICE
               PERFORM 3570-ACCUM-CHANNEL
               PERFORM 3580-SPLIT-COPY-FEE
               PERFORM 3590-SET-RETENTION
               PERFORM 3600-ACCUM-FICHE-TYPE
               PERFORM 3610-ACCUM-CHANNEL
               PERFORM 3620-PRORATE-POSTAGE
               PERFORM 3630-COUNT-ITEM
               PERFORM 3640-CHECK-MATURITY
               PERFORM 3650-DESCRIBE-RETENTION
               PERFORM 3660-EDIT-CONTACT
               PERFORM 3670-LIMIT-STORAGE
               PERFORM 3680-DESCRIBE-FICHE-TYPE
               PERFORM 3690-EDIT-DESC
               PERFORM 3700-AGE-LAST-PAY
               PERFORM 3710-EDIT-REJECT
               PERFORM 3720-SELECT-FEE-PCT
               PERFORM 3730-PRORATE-STORAGE
               PERFORM 3740-LIMIT-RESEARCH
               PERFORM 3750-PRORATE-RESEARCH
               PERFORM 3760-LIMIT-COPY-FEE
               PERFORM 3770-PRORATE-POSTAGE
               PERFORM 3780-AGE-OPEN
               PERFORM 3790-SET-BRANCH
               PERFORM 3800-EDIT-MEMO
               PERFORM 3810-AGE-MATURITY
               PERFORM 3820-FLAG-COPY-FEE
               PERFORM 3830-LIMIT-RESEARCH
               PERFORM 3840-COUNT-ITEM
               PERFORM 3850-LIMIT-RESEARCH
               PERFORM 3860-SET-TRAN
               PERFORM 3870-ASSESS-NOTICE-SENT
               PERFORM 3880-AGE-LAST-MAINT
               PERFORM 3890-SELECT-DISCOUNT
               PERFORM 3900-SELECT-FEE-PCT
               PERFORM 3910-SPLIT-RESEARCH
               PERFORM 3920-SET-TYPE
               PERFORM 3930-SELECT-MARGIN
               PERFORM 3940-SELECT-DISCOUNT
               PERFORM 3950-ACCUM-BRANCH
               PERFORM 3960-SELECT-DISCOUNT
               PERFORM 3970-SELECT-FEE-PCT
               PERFORM 3980-DESCRIBE-FICHE-TYPE
               PERFORM 3990-SPLIT-RESEARCH
               PERFORM 4000-ASSESS-HOLD
               PERFORM 4010-SET-FICHE-TYPE
               PERFORM 4020-DESCRIBE-RETENTION
               PERFORM 4030-LIMIT-RESEARCH
               PERFORM 4040-AGE-CLOSE
               PERFORM 4050-EDIT-NAME
               PERFORM 4060-SELECT-MARGIN
               PERFORM 4070-ACCUM-STATUS
               PERFORM 7020-CALL-MFDTE
               PERFORM 7010-CALL-MFRISK
               PERFORM 7030-CALL-MFLIM
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
      * DAILY INTEREST ON POSTAGE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3000-ACCRUE-POSTAGE.
           IF MFCM-POSTAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-POSTAGE-YTD * MFCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3010-CHECK-OPEN.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO MFCM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COPY FEE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3020-ACCRUE-COPY-FEE.
           IF MFCT-COPY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-COPY-FEE-AMT * MFCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MFCM-RESEARCH-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COPY FEE YTD FEE ON THE RESEARCH AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3030-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN MFCM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO MFCM-COPY-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   COMPUTE MFCM-COPY-FEE-YTD ROUNDED = MFCT-RESEARCH-AMT
                       * MFCM-MARGIN / 200
               WHEN MFCM-RETENTION-CD = 'KT'
                   MOVE 20.00 TO MFCM-COPY-FEE-YTD
               WHEN MFCM-RETENTION-CD = 'SJ'
                   MOVE 20.00 TO MFCM-COPY-FEE-YTD
               WHEN OTHER
                   COMPUTE MFCM-COPY-FEE-YTD ROUNDED = MFCT-RESEARCH-AMT
                       * MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCM-COPY-FEE-YTD > 0 AND MFCM-COPY-FEE-YTD < 5.00
               MOVE 5.00 TO MFCM-COPY-FEE-YTD
           END-IF
           IF MFCM-COPY-FEE-YTD > 100.00
               MOVE 100.00 TO MFCM-COPY-FEE-YTD
           END-IF
           ADD MFCM-COPY-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE COPY FEE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3040-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MFCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MFCM-COPY-FEE-AMT *
                       MFCM-MARGIN / 200
               WHEN MFCM-FICHE-TYPE-CD = 'AW'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN MFCM-FICHE-TYPE-CD = 'JM'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MFCM-COPY-FEE-AMT *
                       MFCM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3050-AGE-CLOSE.
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
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COPY FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3060-LIMIT-COPY-FEE.
           IF MFCM-COPY-FEE-AMT < 100.00
               MOVE 100.00 TO MFCM-COPY-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-COPY-FEE-AMT > 5000.00
               MOVE 5000.00 TO MFCM-COPY-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3070-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-STORAGE-AMT ROUNDED =
                   MFCM-RESEARCH-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-STORAGE-AMT
           END-IF
           ADD MFCT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3080-DESCRIBE-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JXN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KJJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TZL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XZM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR RETENTION CD:
      * KT -> SJ WHEN LAST STMT DATE IS PAST
      * SJ -> WV WHEN LAST STMT DATE IS PAST
      * WV -> KT WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SJ' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SJ'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF MFCM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO MFCM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESEARCH AMT.
      *----------------------------------------------------------------*
       3110-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-RESEARCH-AMT ROUNDED =
                   MFCM-COPY-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-RESEARCH-AMT
           END-IF
           ADD MFCT-RESEARCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3120-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCM-RESEARCH-YTD / 4
           COMPUTE WS-WORK-AMT-2 = MFCM-RESEARCH-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3130-LIMIT-STORAGE.
           IF MFCT-STORAGE-AMT < 0.00
               MOVE 0.00 TO MFCT-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-STORAGE-AMT > 500.00
               MOVE 500.00 TO MFCT-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3140-LIMIT-STORAGE.
           IF MFCT-STORAGE-AMT < 100.00
               MOVE 100.00 TO MFCT-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-STORAGE-AMT > 500.00
               MOVE 500.00 TO MFCT-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3150-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE MFCT-RESEARCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESEARCH YTD.
      *----------------------------------------------------------------*
       3160-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-RESEARCH-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCM-RESEARCH-YTD
           END-IF
           ADD MFCM-RESEARCH-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR RETENTION CD:
      * KT -> SJ WHEN LAST STMT DATE IS PAST
      * SJ -> WV WHEN LAST STMT DATE IS PAST
      * WV -> KT WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3170-SET-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SJ' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SJ'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3180-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3190-LIMIT-POSTAGE.
           IF MFCT-POSTAGE-AMT < 100.00
               MOVE 100.00 TO MFCT-POSTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-POSTAGE-AMT > 5000.00
               MOVE 5000.00 TO MFCT-POSTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COPY FEE YTD BY RETENTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-RETENTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-RETENTION-CD
                   ADD MFCM-COPY-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-RETENTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-COPY-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3210-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN MFCM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO MFCM-RESEARCH-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-STOP-FLAG = 'Y'
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = WS-WORK-AMT-2 *
                       MFCM-FEE-PCT / 200
               WHEN MFCM-TYPE-CD = 'G'
                   MOVE 15.00 TO MFCM-RESEARCH-YTD
               WHEN MFCM-TYPE-CD = 'K'
                   MOVE 3.00 TO MFCM-RESEARCH-YTD
               WHEN OTHER
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = WS-WORK-AMT-2 *
                       MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCM-RESEARCH-YTD > 0 AND MFCM-RESEARCH-YTD < 1.00
               MOVE 1.00 TO MFCM-RESEARCH-YTD
           END-IF
           IF MFCM-RESEARCH-YTD > 35.00
               MOVE 35.00 TO MFCM-RESEARCH-YTD
           END-IF
           ADD MFCM-RESEARCH-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3220-COUNT-CYCLE.
           IF MFCM-CYCLE-CNT < 9999999
               ADD 1 TO MFCM-CYCLE-CNT
           END-IF
           IF MFCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3230-LIMIT-POSTAGE.
           IF MFCM-POSTAGE-YTD < 25.00
               MOVE 25.00 TO MFCM-POSTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-POSTAGE-YTD > 50000.00
               MOVE 50000.00 TO MFCM-POSTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE POSTAGE YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3240-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-TYPE-CD
                   ADD MFCM-POSTAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-POSTAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH FEE ON THE STORAGE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3250-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN MFCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO MFCM-RESEARCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MFCM-RESEARCH-AMT ROUNDED = MFCM-STORAGE-AMT
                       * MFCM-MARGIN / 200
               WHEN MFCM-FICHE-TYPE-CD = 'AW'
                   MOVE 5.00 TO MFCM-RESEARCH-AMT
               WHEN MFCM-FICHE-TYPE-CD = 'JM'
                   MOVE 5.00 TO MFCM-RESEARCH-AMT
               WHEN OTHER
                   COMPUTE MFCM-RESEARCH-AMT ROUNDED = MFCM-STORAGE-AMT
                       * MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCM-RESEARCH-AMT > 0 AND MFCM-RESEARCH-AMT < 5.00
               MOVE 5.00 TO MFCM-RESEARCH-AMT
           END-IF
           IF MFCM-RESEARCH-AMT > 75.00
               MOVE 75.00 TO MFCM-RESEARCH-AMT
           END-IF
           ADD MFCM-RESEARCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE STORAGE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3260-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MFCM-STORAGE-AMT *
                       MFCM-MARGIN / 200
               WHEN MFCM-CHANNEL-CD = 'FV'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN MFCM-CHANNEL-CD = 'GL'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MFCM-STORAGE-AMT *
                       MFCM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       3270-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-POSTAGE-AMT ROUNDED =
                   MFCM-COPY-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCM-POSTAGE-AMT
           END-IF
           ADD MFCM-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STORAGE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3280-FLAG-STORAGE.
           IF MFCM-STORAGE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFCM-STORAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESEARCH AMT.
      *----------------------------------------------------------------*
       3290-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-RESEARCH-AMT ROUNDED =
                   MFCM-STORAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCM-RESEARCH-AMT
           END-IF
           ADD MFCM-RESEARCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE RESEARCH YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-STATUS-CD
                   ADD MFCM-RESEARCH-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-RESEARCH-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   MOVE 'ACTIVE' TO WS-DESC
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
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3320-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-EFF.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO MFCM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESEARCH AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3340-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-TYPE-CD
                   ADD MFCT-RESEARCH-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCT-RESEARCH-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3350-FLAG-STORAGE.
           IF MFCM-STORAGE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MFCM-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3360-DESCRIBE-CHANNEL.
           EVALUATE MFCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3370-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MFCM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO MFCM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3380-LIMIT-STORAGE.
           IF MFCM-STORAGE-YTD < 50.00
               MOVE 50.00 TO MFCM-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-STORAGE-YTD > 50000.00
               MOVE 50000.00 TO MFCM-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN LAST MAINT DATE IS PAST
      * GL -> HH WHEN LAST MAINT DATE IS PAST
      * HH -> LM WHEN LAST MAINT DATE IS PAST
      * LM -> MD WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3390-SET-CHANNEL.
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
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCM-RESEARCH-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MFCM-RESEARCH-AMT - (WS-WORK-AMT * 2)
           COMPUTE MFCM-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE TIER TIER FOR THE STORAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3420-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-TIER-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MFCM-STORAGE-YTD NOT < WS-COPY-FEE-TIER-LOW(WS-TX)
                   MOVE WS-COPY-FEE-TIER-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3430-LIMIT-RESEARCH.
           IF MFCT-RESEARCH-AMT < 50.00
               MOVE 50.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-RESEARCH-AMT > 5000.00
               MOVE 5000.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * MFCM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO MFCT-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3450-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MFCT-COPY-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3460-CHECK-MATURITY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO MFCM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE STORAGE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3470-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN MFCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO MFCM-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MFCM-STORAGE-AMT ROUNDED = MFCM-STORAGE-YTD *
                       MFCM-MARGIN / 200
               WHEN MFCM-FICHE-TYPE-CD = 'AW'
                   MOVE 7.00 TO MFCM-STORAGE-AMT
               WHEN MFCM-FICHE-TYPE-CD = 'JM'
                   MOVE 5.00 TO MFCM-STORAGE-AMT
               WHEN OTHER
                   COMPUTE MFCM-STORAGE-AMT ROUNDED = MFCM-STORAGE-YTD *
                       MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCM-STORAGE-AMT > 0 AND MFCM-STORAGE-AMT < 5.00
               MOVE 5.00 TO MFCM-STORAGE-AMT
           END-IF
           IF MFCM-STORAGE-AMT > 250.00
               MOVE 250.00 TO MFCM-STORAGE-AMT
           END-IF
           ADD MFCM-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3480-DESCRIBE-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E608 ADDR REQUIRED
      * E673 POSTAGE AMT NEGATIVE
      * E289 POSTAGE AMT OVER 5000
      * E285 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3490-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-ADDR = SPACES
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN MFCT-POSTAGE-AMT < 0
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN MFCT-POSTAGE-AMT > 5000.00
                   MOVE 'E289' TO WS-REJECT-CODE
               WHEN MFCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E285' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E389 ADDR REQUIRED
      * E610 COPY FEE AMT NEGATIVE
      * E606 COPY FEE AMT OVER 25000
      * E676 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3500-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-ADDR = SPACES
                   MOVE 'E389' TO WS-REJECT-CODE
               WHEN MFCT-COPY-FEE-AMT < 0
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN MFCT-COPY-FEE-AMT > 25000.00
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN MFCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E676' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3510-FLAG-POSTAGE.
           IF MFCM-POSTAGE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCM-POSTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E445 NAME REQUIRED
      * E335 COPY FEE YTD NEGATIVE
      * E089 COPY FEE YTD OVER 10000
      * E037 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3520-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-NAME = SPACES
                   MOVE 'E445' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-YTD < 0
                   MOVE 'E335' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-YTD > 10000.00
                   MOVE 'E089' TO WS-REJECT-CODE
               WHEN MFCM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E037' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE TIER TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3530-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-TIER-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MFCT-STORAGE-AMT NOT < WS-COPY-FEE-TIER-LOW(WS-TX)
                   MOVE WS-COPY-FEE-TIER-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3540-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCT-CHANNEL-CD
                   ADD MFCM-STORAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-STORAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3550-COUNT-CYCLE.
           IF MFCM-CYCLE-CNT < 9999999
               ADD 1 TO MFCM-CYCLE-CNT
           END-IF
           IF MFCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3560-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO MFCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COPY FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3580-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = MFCM-COPY-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MFCM-COPY-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE MFCM-POSTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR RETENTION CD:
      * KT -> SJ WHEN EXPIRY DATE IS PAST
      * SJ -> WV WHEN EXPIRY DATE IS PAST
      * WV -> KT WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3590-SET-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SJ' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SJ'
                   IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RESEARCH AMT BY FICHE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3600-ACCUM-FICHE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-FICHE-TYPE-CD
                   ADD MFCM-RESEARCH-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-FICHE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-RESEARCH-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3610-ACCUM-CHANNEL.
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
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       3620-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-POSTAGE-AMT ROUNDED =
                   MFCM-RESEARCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-ITEM.
           IF MFCM-ITEM-CNT < 9999999
               ADD 1 TO MFCM-ITEM-CNT
           END-IF
           IF MFCM-ITEM-CNT NOT < 6
               MOVE 'Y' TO MFCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3640-CHECK-MATURITY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO MFCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3650-DESCRIBE-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E427 CONTACT REQUIRED
      * E988 WORK AMT NEGATIVE
      * E081 WORK AMT OVER 10000
      * E005 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CONTACT = SPACES
                   MOVE 'E427' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E988' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E081' TO WS-REJECT-CODE
               WHEN MFCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E005' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3670-LIMIT-STORAGE.
           IF MFCM-STORAGE-AMT < 25.00
               MOVE 25.00 TO MFCM-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-STORAGE-AMT > 1000.00
               MOVE 1000.00 TO MFCM-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE FICHE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3680-DESCRIBE-FICHE-TYPE.
           EVALUATE MFCM-FICHE-TYPE-CD
               WHEN 'AW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E102 DESC REQUIRED
      * E656 WORK AMT 2 NEGATIVE
      * E935 WORK AMT 2 OVER 50000
      * E914 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3690-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-DESC = SPACES
                   MOVE 'E102' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E656' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E935' TO WS-REJECT-CODE
               WHEN MFCM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E914' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E669 REJECT CD REQUIRED
      * E142 WORK AMT 2 NEGATIVE
      * E940 WORK AMT 2 OVER 50000
      * E026 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-REJECT-CD = SPACES
                   MOVE 'E669' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E142' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E940' TO WS-REJECT-CODE
               WHEN MFCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E026' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH SCHED TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3720-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-SCHED-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF MFCM-STORAGE-AMT NOT < WS-RESEARCH-SCHED-LOW(WS-TX)
                   MOVE WS-RESEARCH-SCHED-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3730-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-STORAGE-AMT ROUNDED =
                   MFCM-RESEARCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCT-STORAGE-AMT
           END-IF
           ADD MFCT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RESEARCH AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3740-LIMIT-RESEARCH.
           IF MFCT-RESEARCH-AMT < 10.00
               MOVE 10.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-RESEARCH-AMT > 500.00
               MOVE 500.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESEARCH AMT.
      *----------------------------------------------------------------*
       3750-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-RESEARCH-AMT ROUNDED =
                   MFCM-STORAGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCT-RESEARCH-AMT
           END-IF
           ADD MFCT-RESEARCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * COPY FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3760-LIMIT-COPY-FEE.
           IF MFCM-COPY-FEE-YTD < 25.00
               MOVE 25.00 TO MFCM-COPY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-COPY-FEE-YTD > 50000.00
               MOVE 50000.00 TO MFCM-COPY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       3770-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-POSTAGE-AMT ROUNDED =
                   MFCT-STORAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3780-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFCM-RESEARCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFCM-RESEARCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFCM-RESEARCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCM-RESEARCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-RESEARCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * AT -> KR WHEN POST DATE IS PAST
      * KR -> LK WHEN POST DATE IS PAST
      * LK -> US WHEN POST DATE IS PAST
      * US -> AT WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-BRANCH.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E929 MEMO REQUIRED
      * E181 RESEARCH AMT NEGATIVE
      * E387 RESEARCH AMT OVER 50000
      * E446 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-MEMO = SPACES
                   MOVE 'E929' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-AMT < 0
                   MOVE 'E181' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-AMT > 50000.00
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN MFCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E446' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3810-AGE-MATURITY.
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
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COPY FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3820-FLAG-COPY-FEE.
           IF MFCT-COPY-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFCT-COPY-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3830-LIMIT-RESEARCH.
           IF MFCM-RESEARCH-AMT < 50.00
               MOVE 50.00 TO MFCM-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-RESEARCH-AMT > 50000.00
               MOVE 50000.00 TO MFCM-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-ITEM.
           IF MFCM-ITEM-CNT < 9999999
               ADD 1 TO MFCM-ITEM-CNT
           END-IF
           IF MFCM-ITEM-CNT NOT < 3
               MOVE 'Y' TO MFCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3850-LIMIT-RESEARCH.
           IF MFCM-RESEARCH-AMT < 25.00
               MOVE 25.00 TO MFCM-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-RESEARCH-AMT > 50000.00
               MOVE 50000.00 TO MFCM-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IX -> RS WHEN LAST PAY DATE IS PAST
      * RS -> CP WHEN LAST PAY DATE IS PAST
      * CP -> PG WHEN LAST PAY DATE IS PAST
      * PG -> IX WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3860-SET-TRAN.
           EVALUATE MFCT-TRAN-CD
               WHEN 'IX'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RS' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RS'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PG' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'IX' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH FEE ON THE POSTAGE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3870-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN MFCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO MFCM-RESEARCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-REVIEW-FLAG = 'Y'
                   COMPUTE MFCM-RESEARCH-AMT ROUNDED = MFCM-POSTAGE-YTD
                       * MFCM-MARGIN / 200
               WHEN MFCM-STATUS-CD = 'AST'
                   MOVE 7.00 TO MFCM-RESEARCH-AMT
               WHEN MFCM-STATUS-CD = 'JXN'
                   MOVE 7.00 TO MFCM-RESEARCH-AMT
               WHEN OTHER
                   COMPUTE MFCM-RESEARCH-AMT ROUNDED = MFCM-POSTAGE-YTD
                       * MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCM-RESEARCH-AMT > 0 AND MFCM-RESEARCH-AMT < 1.00
               MOVE 1.00 TO MFCM-RESEARCH-AMT
           END-IF
           IF MFCM-RESEARCH-AMT > 75.00
               MOVE 75.00 TO MFCM-RESEARCH-AMT
           END-IF
           ADD MFCM-RESEARCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3880-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE POSTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3890-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MFCM-POSTAGE-AMT NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3900-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-SCHED-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT NOT < WS-RESEARCH-SCHED-LOW(WS-TX)
                   MOVE WS-RESEARCH-SCHED-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3910-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCT-RESEARCH-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MFCT-RESEARCH-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * G -> K WHEN NOTICE DATE IS PAST
      * K -> W WHEN NOTICE DATE IS PAST
      * W -> Y WHEN NOTICE DATE IS PAST
      * Y -> G WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3920-SET-TYPE.
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
      * SELECT THE STORAGE SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE TIER TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-TIER-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MFCT-RESEARCH-AMT NOT < WS-COPY-FEE-TIER-LOW(WS-TX)
                   MOVE WS-COPY-FEE-TIER-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3950-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-BRANCH-CD
                   ADD MFCM-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MFCT-STORAGE-AMT NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE BAND TIER FOR THE POSTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-BAND-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MFCM-POSTAGE-YTD NOT < WS-STORAGE-BAND-LOW(WS-TX)
                   MOVE WS-STORAGE-BAND-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE FICHE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3980-DESCRIBE-FICHE-TYPE.
           EVALUATE MFCM-FICHE-TYPE-CD
               WHEN 'AW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCM-RESEARCH-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MFCM-RESEARCH-AMT - (WS-WORK-AMT *
               12)
           COMPUTE MFCT-POSTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COPY FEE FEE ON THE COPY FEE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4000-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN MFCM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO MFCT-COPY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MFCT-COPY-FEE-AMT ROUNDED = MFCM-COPY-FEE-YTD
                       * MFCM-MARGIN / 200
               WHEN MFCM-STATUS-CD = 'AST'
                   MOVE 5.00 TO MFCT-COPY-FEE-AMT
               WHEN MFCM-STATUS-CD = 'JXN'
                   MOVE 3.00 TO MFCT-COPY-FEE-AMT
               WHEN OTHER
                   COMPUTE MFCT-COPY-FEE-AMT ROUNDED = MFCM-COPY-FEE-YTD
                       * MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCT-COPY-FEE-AMT > 0 AND MFCT-COPY-FEE-AMT < 5.00
               MOVE 5.00 TO MFCT-COPY-FEE-AMT
           END-IF
           IF MFCT-COPY-FEE-AMT > 100.00
               MOVE 100.00 TO MFCT-COPY-FEE-AMT
           END-IF
           ADD MFCT-COPY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR FICHE TYPE CD:
      * AW -> JM WHEN CLOSE DATE IS PAST
      * JM -> KV WHEN CLOSE DATE IS PAST
      * KV -> MD WHEN CLOSE DATE IS PAST
      * MD -> NR WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4010-SET-FICHE-TYPE.
           EVALUATE MFCM-FICHE-TYPE-CD
               WHEN 'AW'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MD'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NR' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4020-DESCRIBE-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RESEARCH AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4030-LIMIT-RESEARCH.
           IF MFCT-RESEARCH-AMT < 25.00
               MOVE 25.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-RESEARCH-AMT > 500.00
               MOVE 500.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4040-AGE-CLOSE.
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
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E070 NAME REQUIRED
      * E800 COPY FEE YTD NEGATIVE
      * E587 COPY FEE YTD OVER 99999
      * E993 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-NAME = SPACES
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-YTD < 0
                   MOVE 'E800' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-YTD > 99999.00
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN MFCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E993' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE COPY FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4060-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF MFCM-COPY-FEE-AMT NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESEARCH AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-STATUS-CD
                   ADD MFCM-RESEARCH-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-RESEARCH-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE MFC FIELDS TO MFDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-MFDTE.
           INITIALIZE MFDTE-PARMS
           MOVE MFCM-RESEARCH-AMT
             TO MFDTE-RESEARCH-AMT
           MOVE MFCM-STORAGE-YTD
             TO MFDTE-STORAGE-YTD
           MOVE MFCM-COPY-FEE-YTD
             TO MFDTE-COPY-FEE-YTD
           MOVE MFCM-RESEARCH-YTD
             TO MFDTE-RESEARCH-YTD
           MOVE MFCM-POSTAGE-YTD
             TO MFDTE-POSTAGE-YTD
           MOVE MFCM-POSTAGE-AMT
             TO MFDTE-POSTAGE-AMT
           MOVE MFCM-LAST-PAY-DATE
             TO MFDTE-LAST-PAY-DATE
           MOVE MFCM-LAST-MAINT-DATE
             TO MFDTE-LAST-MAINT-DATE
           MOVE MFCM-EXPIRY-DATE
             TO MFDTE-EXPIRY-DATE
           MOVE MFCM-CLOSE-DATE
             TO MFDTE-CLOSE-DATE
           MOVE MFCM-TYPE-CD
             TO MFDTE-TYPE-CD
           MOVE MFCM-FICHE-TYPE-CD
             TO MFDTE-FICHE-TYPE-CD
           MOVE MFCM-RETENTION-CD
             TO MFDTE-RETENTION-CD
           MOVE MFCM-CHANNEL-CD
             TO MFDTE-CHANNEL-CD
           MOVE MFCM-EXEMPT-FLAG
             TO MFDTE-EXEMPT-FLAG
           MOVE MFCM-AUTO-RENEW-FLAG
             TO MFDTE-AUTO-RENEW-FLAG
           MOVE MFCM-STOP-FLAG
             TO MFDTE-STOP-FLAG
           MOVE MFCM-NOTICE-CNT
             TO MFDTE-NOTICE-CNT
           MOVE MFCM-CYCLE-CNT
             TO MFDTE-CYCLE-CNT
           MOVE MFCM-DISCOUNT
             TO MFDTE-DISCOUNT
           MOVE MFCM-MARGIN
             TO MFDTE-MARGIN
           MOVE MFCM-ADDR
             TO MFDTE-ADDR
           MOVE MFCM-CUST-ID
             TO MFDTE-CUST-ID
           CALL 'MFDTE' USING MFDTE-PARMS
           EVALUATE MFDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE MFDTE-STORAGE-YTD
                     TO MFCM-STORAGE-YTD
                   MOVE MFDTE-RESEARCH-YTD
                     TO MFCM-RESEARCH-YTD
                   MOVE MFDTE-POSTAGE-AMT
                     TO MFCM-POSTAGE-AMT
                   MOVE MFDTE-EXEMPT-FLAG
                     TO MFCM-EXEMPT-FLAG
                   MOVE MFDTE-AUTO-RENEW-FLAG
                     TO MFCM-AUTO-RENEW-FLAG
                   MOVE MFDTE-STOP-FLAG
                     TO MFCM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-MFDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE MFC FIELDS TO MFRISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-MFRISK.
           INITIALIZE MFRISK-PARMS
           MOVE MFCM-COPY-FEE-YTD
             TO MFRISK-COPY-FEE-YTD
           MOVE MFCM-COPY-FEE-AMT
             TO MFRISK-COPY-FEE-AMT
           MOVE MFCM-RESEARCH-YTD
             TO MFRISK-RESEARCH-YTD
           MOVE MFCM-RESEARCH-AMT
             TO MFRISK-RESEARCH-AMT
           MOVE MFCM-STORAGE-AMT
             TO MFRISK-STORAGE-AMT
           MOVE MFCM-STORAGE-YTD
             TO MFRISK-STORAGE-YTD
           MOVE MFCM-NOTICE-DATE
             TO MFRISK-NOTICE-DATE
           MOVE MFCM-LAST-PAY-DATE
             TO MFRISK-LAST-PAY-DATE
           MOVE MFCM-OPEN-DATE
             TO MFRISK-OPEN-DATE
           MOVE MFCM-LAST-MAINT-DATE
             TO MFRISK-LAST-MAINT-DATE
           MOVE MFCM-CHANNEL-CD
             TO MFRISK-CHANNEL-CD
           MOVE MFCM-BRANCH-CD
             TO MFRISK-BRANCH-CD
           MOVE MFCM-RETENTION-CD
             TO MFRISK-RETENTION-CD
           MOVE MFCM-STATUS-CD
             TO MFRISK-STATUS-CD
           MOVE MFCM-REVIEW-FLAG
             TO MFRISK-REVIEW-FLAG
           MOVE MFCM-AUTO-RENEW-FLAG
             TO MFRISK-AUTO-RENEW-FLAG
           MOVE MFCM-STOP-FLAG
             TO MFRISK-STOP-FLAG
           MOVE MFCM-CYCLE-CNT
             TO MFRISK-CYCLE-CNT
           MOVE MFCM-NOTICE-CNT
             TO MFRISK-NOTICE-CNT
           MOVE MFCM-MARGIN
             TO MFRISK-MARGIN
           MOVE MFCM-FEE-PCT
             TO MFRISK-FEE-PCT
           MOVE MFCM-NAME
             TO MFRISK-NAME
           MOVE MFCM-ADDR
             TO MFRISK-ADDR
           CALL 'MFRISK' USING MFRISK-PARMS
           EVALUATE MFRISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE MFRISK-COPY-FEE-YTD
                     TO MFCM-COPY-FEE-YTD
                   MOVE MFRISK-RESEARCH-YTD
                     TO MFCM-RESEARCH-YTD
                   MOVE MFRISK-RESEARCH-AMT
                     TO MFCM-RESEARCH-AMT
                   MOVE MFRISK-AUTO-RENEW-FLAG
                     TO MFCM-AUTO-RENEW-FLAG
                   MOVE MFRISK-STOP-FLAG
                     TO MFCM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-MFRISK' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
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
                   MOVE MFLIM-POSTAGE-YTD
                     TO MFCM-POSTAGE-YTD
                   MOVE MFLIM-REVIEW-FLAG
                     TO MFCM-REVIEW-FLAG
                   MOVE MFLIM-STOP-FLAG
                     TO MFCM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-MFLIM' TO ABND-PARAGRAPH
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
           DISPLAY 'MFCAUDT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'MFCAUDT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
