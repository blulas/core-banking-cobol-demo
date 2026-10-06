      *================================================================*
      * PROGRAM  : MFCPOST                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MICROFICHE STATEMENTS - DAILY POSTING. READS THE MFC*
      *            TRANSACTIONS, UPDATES THE MFC MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. STATEMENT MICROFICHE INDEXING,  *
      *            RETENTION AND RESEARCH FEES                         *
      * INPUT    : MFCMST (KSDS), MFCTRN, BANKCTL                      *
      * OUTPUT   : MFCOUT, MFCPORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, MFRISK, MFDTE           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-12-07 JLM  ORIGINAL                                       *
      * 1998-01-19 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-10-22 MKL  STORAGE CUTOFF MOVED REQ4245                   *
      * 1999-05-07 SKN  NEW RESEARCH REPORT COLUMNS PRB8381            *
      * 2003-11-14 RJT  STORAGE NOTICE TEXT PER LEGAL PRB1298          *
      * 2004-07-08 LMB  STORAGE STATUS CODES ADDED INC6548             *
      * 2004-03-20 PQR  STORAGE LIMIT REVISED PRB4965                  *
      * 2005-10-25 DPS  NEW RESEARCH REPORT COLUMNS REQ2328            *
      * 2008-05-07 TWH  AUDIT FINDING - POSTAGE CONTROL TOTALS CHG8406 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFCPOST.
       AUTHOR. B H SANDERS.

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
           SELECT REPORT-FILE ASSIGN TO "MFCPORPT"
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
                                           VALUE 24.000000.
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
      * WS-COPY-FEE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COPY-FEE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000562500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000275000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000248500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000239000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000372500'.
       01  WS-COPY-FEE-BAND-TBL REDEFINES WS-COPY-FEE-BAND-VALUES.
           05  WS-COPY-FEE-BAND-ROW OCCURS 5 TIMES.
               10  WS-COPY-FEE-BAND-LOW    PIC 9(09)V99.
               10  WS-COPY-FEE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COPY-FEE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COPY-FEE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000292500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000082500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000014000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000819500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000035500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004100000000230000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004150000000216000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004650000000790500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004700000000816000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004950000000519000'.
       01  WS-COPY-FEE-SCHED-TBL REDEFINES WS-COPY-FEE-SCHED-VALUES.
           05  WS-COPY-FEE-SCHED-ROW OCCURS 10 TIMES.
               10  WS-COPY-FEE-SCHED-LOW   PIC 9(09)V99.
               10  WS-COPY-FEE-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RESEARCH-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RESEARCH-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000759000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000181500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000035000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003750000000159500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004250000000335000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004300000000158000'.
       01  WS-RESEARCH-BAND-TBL REDEFINES WS-RESEARCH-BAND-VALUES.
           05  WS-RESEARCH-BAND-ROW OCCURS 6 TIMES.
               10  WS-RESEARCH-BAND-LOW    PIC 9(09)V99.
               10  WS-RESEARCH-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  MFCPOST  MICROFICHE STATEMENTS'.
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
       COPY MFRISKP.
       COPY MFDTEP.

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
               PERFORM 3000-SET-FICHE-TYPE
               PERFORM 3010-ACCRUE-RESEARCH
               PERFORM 3020-EDIT-CONTACT
               PERFORM 3030-CHECK-EFF
               PERFORM 3040-ACCRUE-STORAGE
               PERFORM 3050-COUNT-NOTICE
               PERFORM 3060-AGE-EXPIRY
               PERFORM 3070-SET-STATUS
               PERFORM 3080-EDIT-CUST-ID
               PERFORM 3090-SET-FICHE-TYPE
               PERFORM 3100-COUNT-NOTICE
               PERFORM 3110-COUNT-CYCLE
               PERFORM 3120-COUNT-NOTICE
               PERFORM 3130-FLAG-POSTAGE
               PERFORM 3140-SET-TRAN
               PERFORM 3150-SELECT-DISCOUNT
               PERFORM 3160-SPLIT-POSTAGE
               PERFORM 3170-SELECT-DISCOUNT
               PERFORM 3180-PRORATE-STORAGE
               PERFORM 3190-CHECK-POST
               PERFORM 3200-ACCRUE-STORAGE
               PERFORM 3210-COUNT-ITEM
               PERFORM 3220-COUNT-ITEM
               PERFORM 3230-COUNT-PAYMENT
               PERFORM 3240-SPLIT-RESEARCH
               PERFORM 3250-PRORATE-STORAGE
               PERFORM 3260-EDIT-REJECT
               PERFORM 3270-LIMIT-POSTAGE
               PERFORM 3280-SET-CHANNEL
               PERFORM 3290-SELECT-DISCOUNT
               PERFORM 3300-DESCRIBE-TYPE
               PERFORM 3310-SET-BRANCH
               PERFORM 3320-AGE-LAST-STMT
               PERFORM 3330-SPLIT-POSTAGE
               PERFORM 3340-FLAG-STORAGE
               PERFORM 3350-FLAG-POSTAGE
               PERFORM 3360-ACCUM-BRANCH
               PERFORM 3370-LIMIT-RESEARCH
               PERFORM 3380-SELECT-DISCOUNT
               PERFORM 3390-SET-BRANCH
               PERFORM 3400-ASSESS-STOP
               PERFORM 3410-COUNT-NOTICE
               PERFORM 3420-EDIT-REJECT
               PERFORM 3430-AGE-NEXT-DUE
               PERFORM 3440-SELECT-DISCOUNT
               PERFORM 3450-AGE-POST
               PERFORM 3460-SET-CHANNEL
               PERFORM 3470-DESCRIBE-RETENTION
               PERFORM 3480-DESCRIBE-BRANCH
               PERFORM 3490-ACCUM-RETENTION
               PERFORM 3500-PRORATE-STORAGE
               PERFORM 3510-COUNT-ITEM
               PERFORM 3520-CHECK-LAST-MAINT
               PERFORM 3530-SPLIT-STORAGE
               PERFORM 3540-PRORATE-POSTAGE
               PERFORM 3550-SPLIT-POSTAGE
               PERFORM 3560-CHECK-EXPIRY
               PERFORM 3570-LIMIT-POSTAGE
               PERFORM 3580-SET-RETENTION
               PERFORM 3590-CHECK-MATURITY
               PERFORM 3600-SELECT-FEE-PCT
               PERFORM 3610-ACCUM-CHANNEL
               PERFORM 3620-COUNT-ITEM
               PERFORM 3630-EDIT-CONTACT
               PERFORM 3640-AGE-POST
               PERFORM 3650-ACCRUE-STORAGE
               PERFORM 3660-COUNT-NOTICE
               PERFORM 3670-LIMIT-COPY-FEE
               PERFORM 3680-COUNT-NOTICE
               PERFORM 3690-CHECK-LAST-STMT
               PERFORM 3700-AGE-EXPIRY
               PERFORM 3710-LIMIT-POSTAGE
               PERFORM 3720-AGE-NOTICE
               PERFORM 3730-EDIT-MEMO
               PERFORM 3740-PRORATE-COPY-FEE
               PERFORM 3750-LIMIT-STORAGE
               PERFORM 3760-ASSESS-REVIEW
               PERFORM 3770-ASSESS-AUTO-RENEW
               PERFORM 3780-AGE-CLOSE
               PERFORM 3790-FLAG-RESEARCH
               PERFORM 3800-EDIT-CONTACT
               PERFORM 3810-SPLIT-POSTAGE
               PERFORM 3820-ACCRUE-POSTAGE
               PERFORM 3830-AGE-LAST-MAINT
               PERFORM 3840-LIMIT-POSTAGE
               PERFORM 3850-CHECK-LAST-STMT
               PERFORM 3860-SPLIT-STORAGE
               PERFORM 3870-SPLIT-COPY-FEE
               PERFORM 3880-EDIT-NAME
               PERFORM 3890-EDIT-REJECT
               PERFORM 3900-LIMIT-COPY-FEE
               PERFORM 3910-CHECK-CLOSE
               PERFORM 3920-CHECK-EXPIRY
               PERFORM 3930-EDIT-MEMO
               PERFORM 3940-PRORATE-STORAGE
               PERFORM 3950-EDIT-REJECT
               PERFORM 3960-EDIT-REF-NO
               PERFORM 3970-ASSESS-EXEMPT
               PERFORM 3980-LIMIT-POSTAGE
               PERFORM 3990-SELECT-FEE-PCT
               PERFORM 4000-SELECT-FEE-PCT
               PERFORM 4010-AGE-OPEN
               PERFORM 4020-LIMIT-ITEM
               PERFORM 4030-ACCUM-STATUS
               PERFORM 4040-ACCUM-BRANCH
               PERFORM 4050-DESCRIBE-RETENTION
               PERFORM 4060-DESCRIBE-BRANCH
               PERFORM 4070-EDIT-ADDR
               PERFORM 4080-SET-STATUS
               PERFORM 4090-DESCRIBE-FICHE-TYPE
               PERFORM 4100-ACCRUE-MARGIN
               PERFORM 4110-SPLIT-STORAGE
               PERFORM 4120-SET-RETENTION
               PERFORM 4130-SPLIT-RESEARCH
               PERFORM 4140-SELECT-FEE-PCT
               PERFORM 4150-CHECK-MATURITY
               PERFORM 4160-CHECK-MATURITY
               PERFORM 4170-ASSESS-HOLD
               PERFORM 4180-ACCUM-FICHE-TYPE
               PERFORM 4190-CHECK-EFF
               PERFORM 4200-ACCRUE-RESEARCH
               PERFORM 4210-LIMIT-RESEARCH
               PERFORM 4220-SET-TYPE
               PERFORM 4230-ACCRUE-POSTAGE
               PERFORM 4240-SELECT-MARGIN
               PERFORM 4250-ACCUM-FICHE-TYPE
               PERFORM 4260-LIMIT-POSTAGE
               PERFORM 4270-PRORATE-POSTAGE
               PERFORM 4280-AGE-OPEN
               PERFORM 4290-EDIT-ADDR
               PERFORM 4300-COUNT-NOTICE
               PERFORM 4310-SELECT-DISCOUNT
               PERFORM 4320-SET-CHANNEL
               PERFORM 4330-ASSESS-REVIEW
               PERFORM 4340-SET-TRAN
               PERFORM 4350-CHECK-NOTICE
               PERFORM 4360-LIMIT-RESEARCH
               PERFORM 4370-FLAG-ITEM
               PERFORM 4380-DESCRIBE-FICHE-TYPE
               PERFORM 4390-SET-STATUS
               PERFORM 4400-ASSESS-HOLD
               PERFORM 4410-CHECK-NEXT-DUE
               PERFORM 4420-PRORATE-POSTAGE
               PERFORM 4430-LIMIT-COPY-FEE
               PERFORM 4440-SPLIT-POSTAGE
               PERFORM 4450-CHECK-LAST-STMT
               PERFORM 4460-LIMIT-POSTAGE
               PERFORM 4470-ASSESS-REVIEW
               PERFORM 4480-COUNT-CYCLE
               PERFORM 4490-SPLIT-RESEARCH
               PERFORM 4500-FLAG-COPY-FEE
               PERFORM 4510-COUNT-NOTICE
               PERFORM 4520-ACCRUE-RESEARCH
               PERFORM 4530-FLAG-STORAGE
               PERFORM 4540-SELECT-DISCOUNT
               PERFORM 4550-ACCRUE-POSTAGE
               PERFORM 4560-EDIT-ADDR
               PERFORM 4570-AGE-NOTICE
               PERFORM 4580-DESCRIBE-TRAN
               PERFORM 4590-SELECT-DISCOUNT
               PERFORM 4600-ACCRUE-RESEARCH
               PERFORM 4610-ASSESS-REVERSAL
               PERFORM 4620-DESCRIBE-CHANNEL
               PERFORM 4630-DESCRIBE-CHANNEL
               PERFORM 4640-SET-TYPE
               PERFORM 4650-DESCRIBE-FICHE-TYPE
               PERFORM 4660-SET-TYPE
               PERFORM 4670-ASSESS-REVIEW
               PERFORM 4680-CHECK-NOTICE
               PERFORM 4690-ASSESS-NOTICE-SENT
               PERFORM 4700-FLAG-STORAGE
               PERFORM 4710-LIMIT-STORAGE
               PERFORM 4720-CHECK-POST
               PERFORM 4730-FLAG-POSTAGE
               PERFORM 4740-PRORATE-RESEARCH
               PERFORM 4750-FLAG-RESEARCH
               PERFORM 4760-CHECK-OPEN
               PERFORM 4770-EDIT-MEMO
               PERFORM 4780-ASSESS-AUTO-RENEW
               PERFORM 7010-CALL-MFRISK
               PERFORM 7020-CALL-MFDTE
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
      * STATUS TRANSITIONS FOR FICHE TYPE CD:
      * AW -> JM WHEN EFF DATE IS PAST
      * JM -> KV WHEN EFF DATE IS PAST
      * KV -> MD WHEN EFF DATE IS PAST
      * MD -> NR WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3000-SET-FICHE-TYPE.
           EVALUATE MFCM-FICHE-TYPE-CD
               WHEN 'AW'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MD'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NR' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-RESEARCH.
           IF MFCT-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-RESEARCH-AMT * MFCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO MFCT-COPY-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E188 CONTACT REQUIRED
      * E862 COPY FEE AMT NEGATIVE
      * E175 COPY FEE AMT OVER 99999
      * E695 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CONTACT = SPACES
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-AMT < 0
                   MOVE 'E862' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-AMT > 99999.00
                   MOVE 'E175' TO WS-REJECT-CODE
               WHEN MFCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E695' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3030-CHECK-EFF.
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
               MOVE 'Y' TO MFCM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3040-ACCRUE-STORAGE.
           IF MFCM-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-STORAGE-AMT * MFCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3050-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO MFCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3060-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AST -> JXN WHEN LAST MAINT DATE IS PAST
      * JXN -> KJJ WHEN LAST MAINT DATE IS PAST
      * KJJ -> TZL WHEN LAST MAINT DATE IS PAST
      * TZL -> XZM WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-STATUS.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E940 CUST ID REQUIRED
      * E711 RESEARCH AMT NEGATIVE
      * E574 RESEARCH AMT OVER 1000000
      * E156 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CUST-ID = SPACES
                   MOVE 'E940' TO WS-REJECT-CODE
               WHEN MFCT-RESEARCH-AMT < 0
                   MOVE 'E711' TO WS-REJECT-CODE
               WHEN MFCT-RESEARCH-AMT > 1000000.00
                   MOVE 'E574' TO WS-REJECT-CODE
               WHEN MFCM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR FICHE TYPE CD:
      * AW -> JM WHEN OPEN DATE IS PAST
      * JM -> KV WHEN OPEN DATE IS PAST
      * KV -> MD WHEN OPEN DATE IS PAST
      * MD -> NR WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-FICHE-TYPE.
           EVALUATE MFCM-FICHE-TYPE-CD
               WHEN 'AW'
                   IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MD'
                   IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NR' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO MFCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-CYCLE.
           IF MFCM-CYCLE-CNT < 9999999
               ADD 1 TO MFCM-CYCLE-CNT
           END-IF
           IF MFCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO MFCM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3120-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO MFCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3130-FLAG-POSTAGE.
           IF MFCM-POSTAGE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFCM-POSTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IX -> RS WHEN LAST MAINT DATE IS PAST
      * RS -> CP WHEN LAST MAINT DATE IS PAST
      * CP -> PG WHEN LAST MAINT DATE IS PAST
      * PG -> IX WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3140-SET-TRAN.
           EVALUATE MFCT-TRAN-CD
               WHEN 'IX'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RS' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RS'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CP' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PG' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'IX' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE BAND TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-BAND-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MFCM-RESEARCH-AMT NOT < WS-COPY-FEE-BAND-LOW(WS-TX)
                   MOVE WS-COPY-FEE-BAND-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3160-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFCM-POSTAGE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = MFCM-POSTAGE-YTD - (WS-WORK-AMT * 12)
           COMPUTE MFCM-COPY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH BAND TIER FOR THE COPY FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3170-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-BAND-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF MFCT-COPY-FEE-AMT NOT < WS-RESEARCH-BAND-LOW(WS-TX)
                   MOVE WS-RESEARCH-BAND-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3180-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-STORAGE-AMT ROUNDED =
                   MFCT-POSTAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCT-STORAGE-AMT
           END-IF
           ADD MFCT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF MFCT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-STORAGE.
           IF MFCM-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-STORAGE-AMT * MFCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MFCT-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-ITEM.
           IF MFCM-ITEM-CNT < 9999999
               ADD 1 TO MFCM-ITEM-CNT
           END-IF
           IF MFCM-ITEM-CNT NOT < 10
               MOVE 'Y' TO MFCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3220-COUNT-ITEM.
           IF MFCM-ITEM-CNT < 9999999
               ADD 1 TO MFCM-ITEM-CNT
           END-IF
           IF MFCM-ITEM-CNT NOT < 6
               MOVE 'Y' TO MFCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-PAYMENT.
           IF MFCM-PAYMENT-CNT < 9999999
               ADD 1 TO MFCM-PAYMENT-CNT
           END-IF
           IF MFCM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO MFCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3240-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCT-RESEARCH-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MFCT-RESEARCH-AMT - (WS-WORK-AMT * 2)
           COMPUTE MFCM-COPY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3250-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-STORAGE-AMT ROUNDED =
                   MFCM-STORAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-STORAGE-AMT
           END-IF
           ADD MFCT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E365 REJECT CD REQUIRED
      * E101 STORAGE AMT NEGATIVE
      * E741 STORAGE AMT OVER 1000000
      * E980 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-REJECT-CD = SPACES
                   MOVE 'E365' TO WS-REJECT-CODE
               WHEN MFCT-STORAGE-AMT < 0
                   MOVE 'E101' TO WS-REJECT-CODE
               WHEN MFCT-STORAGE-AMT > 1000000.00
                   MOVE 'E741' TO WS-REJECT-CODE
               WHEN MFCM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E980' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3270-LIMIT-POSTAGE.
           IF MFCM-POSTAGE-AMT < 100.00
               MOVE 100.00 TO MFCM-POSTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-POSTAGE-AMT > 50000.00
               MOVE 50000.00 TO MFCM-POSTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN POST DATE IS PAST
      * GL -> HH WHEN POST DATE IS PAST
      * HH -> LM WHEN POST DATE IS PAST
      * LM -> MD WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3280-SET-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   IF MFCT-POST-DATE > 0 AND MFCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFCT-POST-DATE > 0 AND MFCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFCT-POST-DATE > 0 AND MFCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFCT-POST-DATE > 0 AND MFCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE SCHED TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-SCHED-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF MFCM-COPY-FEE-YTD NOT < WS-COPY-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-COPY-FEE-SCHED-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3300-DESCRIBE-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * AT -> KR WHEN EFF DATE IS PAST
      * KR -> LK WHEN EFF DATE IS PAST
      * LK -> US WHEN EFF DATE IS PAST
      * US -> AT WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3310-SET-BRANCH.
           EVALUATE MFCM-BRANCH-CD
               WHEN 'AT'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LK' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LK'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-LAST-STMT.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3330-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFCM-POSTAGE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MFCM-POSTAGE-YTD - (WS-WORK-AMT * 2)
           COMPUTE MFCT-RESEARCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3340-FLAG-STORAGE.
           IF MFCM-STORAGE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFCM-STORAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3350-FLAG-POSTAGE.
           IF MFCM-POSTAGE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MFCM-POSTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3360-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-BRANCH-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3370-LIMIT-RESEARCH.
           IF MFCT-RESEARCH-AMT < 25.00
               MOVE 25.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-RESEARCH-AMT > 500.00
               MOVE 500.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE BAND TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-BAND-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MFCT-STORAGE-AMT NOT < WS-COPY-FEE-BAND-LOW(WS-TX)
                   MOVE WS-COPY-FEE-BAND-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * AT -> KR WHEN LAST PAY DATE IS PAST
      * KR -> LK WHEN LAST PAY DATE IS PAST
      * LK -> US WHEN LAST PAY DATE IS PAST
      * US -> AT WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3390-SET-BRANCH.
           EVALUATE MFCM-BRANCH-CD
               WHEN 'AT'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LK' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LK'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE STORAGE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3400-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MFCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MFCT-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-REVIEW-FLAG = 'Y'
                   COMPUTE MFCT-STORAGE-AMT ROUNDED = MFCM-STORAGE-AMT *
                       MFCM-MARGIN / 200
               WHEN MFCM-CHANNEL-CD = 'FV'
                   MOVE 12.00 TO MFCT-STORAGE-AMT
               WHEN MFCM-CHANNEL-CD = 'GL'
                   MOVE 10.00 TO MFCT-STORAGE-AMT
               WHEN OTHER
                   COMPUTE MFCT-STORAGE-AMT ROUNDED = MFCM-STORAGE-AMT *
                       MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCT-STORAGE-AMT > 0 AND MFCT-STORAGE-AMT < 1.00
               MOVE 1.00 TO MFCT-STORAGE-AMT
           END-IF
           IF MFCT-STORAGE-AMT > 100.00
               MOVE 100.00 TO MFCT-STORAGE-AMT
           END-IF
           ADD MFCT-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3410-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E491 REJECT CD REQUIRED
      * E208 POSTAGE AMT NEGATIVE
      * E871 POSTAGE AMT OVER 50000
      * E312 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-REJECT-CD = SPACES
                   MOVE 'E491' TO WS-REJECT-CODE
               WHEN MFCM-POSTAGE-AMT < 0
                   MOVE 'E208' TO WS-REJECT-CODE
               WHEN MFCM-POSTAGE-AMT > 50000.00
                   MOVE 'E871' TO WS-REJECT-CODE
               WHEN MFCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E312' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH BAND TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3440-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-BAND-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF MFCM-STORAGE-AMT NOT < WS-RESEARCH-BAND-LOW(WS-TX)
                   MOVE WS-RESEARCH-BAND-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3450-AGE-POST.
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
                   ADD MFCM-COPY-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MFCM-COPY-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MFCM-COPY-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFCM-COPY-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-COPY-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN LAST PAY DATE IS PAST
      * GL -> HH WHEN LAST PAY DATE IS PAST
      * HH -> LM WHEN LAST PAY DATE IS PAST
      * LM -> MD WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3460-SET-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3470-DESCRIBE-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3480-DESCRIBE-BRANCH.
           EVALUATE MFCM-BRANCH-CD
               WHEN 'AT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'US'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE COPY FEE AMT BY RETENTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3490-ACCUM-RETENTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-RETENTION-CD
                   ADD MFCM-COPY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-RETENTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-COPY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE YTD.
      *----------------------------------------------------------------*
       3500-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-STORAGE-YTD ROUNDED =
                   MFCT-POSTAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCM-STORAGE-YTD
           END-IF
           ADD MFCM-STORAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-ITEM.
           IF MFCM-ITEM-CNT < 9999999
               ADD 1 TO MFCM-ITEM-CNT
           END-IF
           IF MFCM-ITEM-CNT NOT < 6
               MOVE 'Y' TO MFCM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3520-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO MFCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = MFCT-STORAGE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MFCT-STORAGE-AMT - (WS-WORK-AMT * 12)
           COMPUTE MFCM-COPY-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       3540-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-POSTAGE-AMT ROUNDED =
                   MFCM-COPY-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3550-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFCM-POSTAGE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MFCM-POSTAGE-AMT - (WS-WORK-AMT * 12)
           COMPUTE MFCT-COPY-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3560-CHECK-EXPIRY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3570-LIMIT-POSTAGE.
           IF MFCM-POSTAGE-YTD < 100.00
               MOVE 100.00 TO MFCM-POSTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-POSTAGE-YTD > 50000.00
               MOVE 50000.00 TO MFCM-POSTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR RETENTION CD:
      * KT -> SJ WHEN OPEN DATE IS PAST
      * SJ -> WV WHEN OPEN DATE IS PAST
      * WV -> KT WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3580-SET-RETENTION.
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
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-MATURITY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE SCHED TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3600-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-SCHED-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF MFCT-RESEARCH-AMT NOT < WS-COPY-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-COPY-FEE-SCHED-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COPY FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3610-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCT-CHANNEL-CD
                   ADD MFCM-COPY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-COPY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-ITEM.
           IF MFCM-ITEM-CNT < 9999999
               ADD 1 TO MFCM-ITEM-CNT
           END-IF
           IF MFCM-ITEM-CNT NOT < 3
               MOVE 'Y' TO MFCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E301 CONTACT REQUIRED
      * E787 COPY FEE AMT NEGATIVE
      * E220 COPY FEE AMT OVER 250000
      * E107 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CONTACT = SPACES
                   MOVE 'E301' TO WS-REJECT-CODE
               WHEN MFCT-COPY-FEE-AMT < 0
                   MOVE 'E787' TO WS-REJECT-CODE
               WHEN MFCT-COPY-FEE-AMT > 250000.00
                   MOVE 'E220' TO WS-REJECT-CODE
               WHEN MFCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E107' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3640-AGE-POST.
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
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-STORAGE.
           IF MFCT-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-STORAGE-AMT * MFCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO MFCM-POSTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO MFCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3670-LIMIT-COPY-FEE.
           IF MFCM-COPY-FEE-AMT < 50.00
               MOVE 50.00 TO MFCM-COPY-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-COPY-FEE-AMT > 10000.00
               MOVE 10000.00 TO MFCM-COPY-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3680-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3690-CHECK-LAST-STMT.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO MFCM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * POSTAGE YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3710-LIMIT-POSTAGE.
           IF MFCM-POSTAGE-YTD < 50.00
               MOVE 50.00 TO MFCM-POSTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-POSTAGE-YTD > 1000.00
               MOVE 1000.00 TO MFCM-POSTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3720-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF MFCM-NOTICE-DATE > 0 AND MFCM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E199 MEMO REQUIRED
      * E615 WORK AMT 2 NEGATIVE
      * E933 WORK AMT 2 OVER 250000
      * E429 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3730-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-MEMO = SPACES
                   MOVE 'E199' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E933' TO WS-REJECT-CODE
               WHEN MFCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E429' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COPY FEE AMT.
      *----------------------------------------------------------------*
       3740-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-COPY-FEE-AMT ROUNDED =
                   MFCM-POSTAGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCM-COPY-FEE-AMT
           END-IF
           ADD MFCM-COPY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3750-LIMIT-STORAGE.
           IF MFCM-STORAGE-AMT < 50.00
               MOVE 50.00 TO MFCM-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-STORAGE-AMT > 10000.00
               MOVE 10000.00 TO MFCM-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH FEE ON THE COPY FEE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3760-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN MFCM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO MFCT-RESEARCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-EXEMPT-FLAG = 'Y'
                   COMPUTE MFCT-RESEARCH-AMT ROUNDED = MFCM-COPY-FEE-YTD
                       * MFCM-MARGIN / 200
               WHEN MFCM-TYPE-CD = 'G'
                   MOVE 7.00 TO MFCT-RESEARCH-AMT
               WHEN MFCM-TYPE-CD = 'K'
                   MOVE 3.00 TO MFCT-RESEARCH-AMT
               WHEN OTHER
                   COMPUTE MFCT-RESEARCH-AMT ROUNDED = MFCM-COPY-FEE-YTD
                       * MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCT-RESEARCH-AMT > 0 AND MFCT-RESEARCH-AMT < 1.00
               MOVE 1.00 TO MFCT-RESEARCH-AMT
           END-IF
           IF MFCT-RESEARCH-AMT > 50.00
               MOVE 50.00 TO MFCT-RESEARCH-AMT
           END-IF
           ADD MFCT-RESEARCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE COPY FEE YTD FEE ON THE STORAGE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3770-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MFCM-COPY-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-REVIEW-FLAG = 'Y'
                   COMPUTE MFCM-COPY-FEE-YTD ROUNDED = MFCM-STORAGE-YTD
                       * MFCM-DISCOUNT / 200
               WHEN MFCM-FICHE-TYPE-CD = 'AW'
                   MOVE 20.00 TO MFCM-COPY-FEE-YTD
               WHEN MFCM-FICHE-TYPE-CD = 'JM'
                   MOVE 7.00 TO MFCM-COPY-FEE-YTD
               WHEN OTHER
                   COMPUTE MFCM-COPY-FEE-YTD ROUNDED = MFCM-STORAGE-YTD
                       * MFCM-DISCOUNT / 100
           END-EVALUATE
           IF MFCM-COPY-FEE-YTD > 0 AND MFCM-COPY-FEE-YTD < 2.00
               MOVE 2.00 TO MFCM-COPY-FEE-YTD
           END-IF
           IF MFCM-COPY-FEE-YTD > 75.00
               MOVE 75.00 TO MFCM-COPY-FEE-YTD
           END-IF
           ADD MFCM-COPY-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3780-AGE-CLOSE.
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
                   ADD MFCM-POSTAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFCM-POSTAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MFCM-POSTAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD MFCM-POSTAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-POSTAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RESEARCH AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3790-FLAG-RESEARCH.
           IF MFCT-RESEARCH-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCT-RESEARCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E382 CONTACT REQUIRED
      * E778 POSTAGE AMT NEGATIVE
      * E289 POSTAGE AMT OVER 99999
      * E108 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CONTACT = SPACES
                   MOVE 'E382' TO WS-REJECT-CODE
               WHEN MFCM-POSTAGE-AMT < 0
                   MOVE 'E778' TO WS-REJECT-CODE
               WHEN MFCM-POSTAGE-AMT > 99999.00
                   MOVE 'E289' TO WS-REJECT-CODE
               WHEN MFCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E108' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3810-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFCM-POSTAGE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MFCM-POSTAGE-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3820-ACCRUE-POSTAGE.
           IF MFCM-POSTAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-POSTAGE-YTD * MFCM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO MFCM-RESEARCH-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-LAST-MAINT.
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
                   ADD MFCM-RESEARCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MFCM-RESEARCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MFCM-RESEARCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCM-RESEARCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-RESEARCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * POSTAGE YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3840-LIMIT-POSTAGE.
           IF MFCM-POSTAGE-YTD < 0.00
               MOVE 0.00 TO MFCM-POSTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-POSTAGE-YTD > 10000.00
               MOVE 10000.00 TO MFCM-POSTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-LAST-STMT.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO MFCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3860-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = MFCM-STORAGE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MFCM-STORAGE-AMT - (WS-WORK-AMT * 2)
           COMPUTE MFCT-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COPY FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3870-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = MFCM-COPY-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MFCM-COPY-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE MFCM-POSTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E366 NAME REQUIRED
      * E915 RESEARCH YTD NEGATIVE
      * E246 RESEARCH YTD OVER 5000
      * E400 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3880-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-NAME = SPACES
                   MOVE 'E366' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-YTD < 0
                   MOVE 'E915' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-YTD > 5000.00
                   MOVE 'E246' TO WS-REJECT-CODE
               WHEN MFCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E400' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E494 REJECT CD REQUIRED
      * E745 STORAGE AMT NEGATIVE
      * E137 STORAGE AMT OVER 10000
      * E764 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-REJECT-CD = SPACES
                   MOVE 'E494' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-AMT < 0
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-AMT > 10000.00
                   MOVE 'E137' TO WS-REJECT-CODE
               WHEN MFCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E764' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3900-LIMIT-COPY-FEE.
           IF MFCM-COPY-FEE-YTD < 50.00
               MOVE 50.00 TO MFCM-COPY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-COPY-FEE-YTD > 1000.00
               MOVE 1000.00 TO MFCM-COPY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3910-CHECK-CLOSE.
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
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-EXPIRY.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E555 MEMO REQUIRED
      * E606 COPY FEE AMT NEGATIVE
      * E673 COPY FEE AMT OVER 5000
      * E353 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3930-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-MEMO = SPACES
                   MOVE 'E555' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-AMT < 0
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-AMT > 5000.00
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN MFCM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E353' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3940-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MFCT-STORAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E876 REJECT CD REQUIRED
      * E016 RESEARCH YTD NEGATIVE
      * E272 RESEARCH YTD OVER 10000
      * E595 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3950-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-REJECT-CD = SPACES
                   MOVE 'E876' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-YTD < 0
                   MOVE 'E016' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-YTD > 10000.00
                   MOVE 'E272' TO WS-REJECT-CODE
               WHEN MFCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E595' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E475 REF NO REQUIRED
      * E592 POSTAGE AMT NEGATIVE
      * E501 POSTAGE AMT OVER 25000
      * E899 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-REF-NO = SPACES
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN MFCM-POSTAGE-AMT < 0
                   MOVE 'E592' TO WS-REJECT-CODE
               WHEN MFCM-POSTAGE-AMT > 25000.00
                   MOVE 'E501' TO WS-REJECT-CODE
               WHEN MFCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E899' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH YTD FEE ON THE COPY FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3970-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MFCM-RESEARCH-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = MFCT-COPY-FEE-AMT
                       * MFCM-FEE-PCT / 200
               WHEN MFCM-BRANCH-CD = 'AT'
                   MOVE 3.00 TO MFCM-RESEARCH-YTD
               WHEN MFCM-BRANCH-CD = 'KR'
                   MOVE 12.00 TO MFCM-RESEARCH-YTD
               WHEN OTHER
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = MFCT-COPY-FEE-AMT
                       * MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCM-RESEARCH-YTD > 0 AND MFCM-RESEARCH-YTD < 2.00
               MOVE 2.00 TO MFCM-RESEARCH-YTD
           END-IF
           IF MFCM-RESEARCH-YTD > 75.00
               MOVE 75.00 TO MFCM-RESEARCH-YTD
           END-IF
           ADD MFCM-RESEARCH-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * POSTAGE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3980-LIMIT-POSTAGE.
           IF MFCM-POSTAGE-AMT < 0.00
               MOVE 0.00 TO MFCM-POSTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-POSTAGE-AMT > 10000.00
               MOVE 10000.00 TO MFCM-POSTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH BAND TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3990-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-BAND-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF MFCM-COPY-FEE-YTD NOT < WS-RESEARCH-BAND-LOW(WS-TX)
                   MOVE WS-RESEARCH-BAND-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE BAND TIER FOR THE RESEARCH YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4000-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-BAND-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MFCM-RESEARCH-YTD NOT < WS-COPY-FEE-BAND-LOW(WS-TX)
                   MOVE WS-COPY-FEE-BAND-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4010-AGE-OPEN.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4020-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESEARCH AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-STATUS.
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
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4040-ACCUM-BRANCH.
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
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4050-DESCRIBE-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4060-DESCRIBE-BRANCH.
           EVALUATE MFCM-BRANCH-CD
               WHEN 'AT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LK'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'US'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E416 ADDR REQUIRED
      * E915 COPY FEE YTD NEGATIVE
      * E782 COPY FEE YTD OVER 25000
      * E274 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4070-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-ADDR = SPACES
                   MOVE 'E416' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-YTD < 0
                   MOVE 'E915' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-YTD > 25000.00
                   MOVE 'E782' TO WS-REJECT-CODE
               WHEN MFCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E274' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AST -> JXN WHEN NEXT DUE DATE IS PAST
      * JXN -> KJJ WHEN NEXT DUE DATE IS PAST
      * KJJ -> TZL WHEN NEXT DUE DATE IS PAST
      * TZL -> XZM WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4080-SET-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JXN' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JXN'
                   IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJJ' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KJJ'
                   IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TZL' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TZL'
                   IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZM' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE FICHE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4090-DESCRIBE-FICHE-TYPE.
           EVALUATE MFCM-FICHE-TYPE-CD
               WHEN 'AW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4100-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * MFCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO MFCM-COPY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4110-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = MFCM-STORAGE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MFCM-STORAGE-YTD - (WS-WORK-AMT * 2)
           COMPUTE MFCT-COPY-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR RETENTION CD:
      * KT -> SJ WHEN CLOSE DATE IS PAST
      * SJ -> WV WHEN CLOSE DATE IS PAST
      * WV -> KT WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4120-SET-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SJ' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SJ'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4130-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCM-RESEARCH-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MFCM-RESEARCH-AMT - (WS-WORK-AMT * 2)
           COMPUTE MFCM-RESEARCH-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE BAND TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4140-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-BAND-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MFCM-COPY-FEE-YTD NOT < WS-COPY-FEE-BAND-LOW(WS-TX)
                   MOVE WS-COPY-FEE-BAND-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4150-CHECK-MATURITY.
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
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4160-CHECK-MATURITY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH FEE ON THE POSTAGE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4170-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN MFCM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO MFCM-RESEARCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MFCM-RESEARCH-AMT ROUNDED = MFCM-POSTAGE-AMT
                       * MFCM-FEE-PCT / 200
               WHEN MFCT-TRAN-CD = 'IX'
                   MOVE 5.00 TO MFCM-RESEARCH-AMT
               WHEN MFCT-TRAN-CD = 'RS'
                   MOVE 7.00 TO MFCM-RESEARCH-AMT
               WHEN OTHER
                   COMPUTE MFCM-RESEARCH-AMT ROUNDED = MFCM-POSTAGE-AMT
                       * MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCM-RESEARCH-AMT > 0 AND MFCM-RESEARCH-AMT < 5.00
               MOVE 5.00 TO MFCM-RESEARCH-AMT
           END-IF
           IF MFCM-RESEARCH-AMT > 50.00
               MOVE 50.00 TO MFCM-RESEARCH-AMT
           END-IF
           ADD MFCM-RESEARCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE RESEARCH YTD BY FICHE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4180-ACCUM-FICHE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-FICHE-TYPE-CD
                   ADD MFCM-RESEARCH-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-FICHE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-RESEARCH-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4190-CHECK-EFF.
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
               MOVE 'Y' TO MFCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4200-ACCRUE-RESEARCH.
           IF MFCM-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-RESEARCH-AMT * MFCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MFCM-COPY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4210-LIMIT-RESEARCH.
           IF MFCM-RESEARCH-YTD < 100.00
               MOVE 100.00 TO MFCM-RESEARCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-RESEARCH-YTD > 10000.00
               MOVE 10000.00 TO MFCM-RESEARCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * G -> K WHEN NEXT DUE DATE IS PAST
      * K -> W WHEN NEXT DUE DATE IS PAST
      * W -> Y WHEN NEXT DUE DATE IS PAST
      * Y -> G WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4220-SET-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4230-ACCRUE-POSTAGE.
           IF MFCM-POSTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-POSTAGE-AMT * MFCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE BAND TIER FOR THE STORAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4240-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-BAND-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MFCM-STORAGE-YTD NOT < WS-COPY-FEE-BAND-LOW(WS-TX)
                   MOVE WS-COPY-FEE-BAND-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY FICHE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4250-ACCUM-FICHE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-FICHE-TYPE-CD
                   ADD MFCT-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-FICHE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCT-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4260-LIMIT-POSTAGE.
           IF MFCM-POSTAGE-AMT < 25.00
               MOVE 25.00 TO MFCM-POSTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-POSTAGE-AMT > 5000.00
               MOVE 5000.00 TO MFCM-POSTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO POSTAGE YTD.
      *----------------------------------------------------------------*
       4270-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-POSTAGE-YTD ROUNDED =
                   MFCM-COPY-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCM-POSTAGE-YTD
           END-IF
           ADD MFCM-POSTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4280-AGE-OPEN.
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
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E082 ADDR REQUIRED
      * E885 STORAGE YTD NEGATIVE
      * E707 STORAGE YTD OVER 99999
      * E274 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4290-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-ADDR = SPACES
                   MOVE 'E082' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-YTD < 0
                   MOVE 'E885' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-YTD > 99999.00
                   MOVE 'E707' TO WS-REJECT-CODE
               WHEN MFCM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E274' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4300-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO MFCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE SCHED TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4310-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-SCHED-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF MFCM-RESEARCH-AMT NOT < WS-COPY-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-COPY-FEE-SCHED-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4320-SET-CHANNEL.
           EVALUATE MFCT-CHANNEL-CD
               WHEN 'BRN'
                   IF MFCT-POST-DATE > 0 AND MFCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF MFCT-POST-DATE > 0 AND MFCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF MFCT-POST-DATE > 0 AND MFCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF MFCT-POST-DATE > 0 AND MFCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE POSTAGE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4330-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN MFCM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO MFCM-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MFCM-STORAGE-AMT ROUNDED = MFCM-POSTAGE-AMT *
                       MFCM-MARGIN / 200
               WHEN MFCM-FICHE-TYPE-CD = 'AW'
                   MOVE 7.00 TO MFCM-STORAGE-AMT
               WHEN MFCM-FICHE-TYPE-CD = 'JM'
                   MOVE 5.00 TO MFCM-STORAGE-AMT
               WHEN OTHER
                   COMPUTE MFCM-STORAGE-AMT ROUNDED = MFCM-POSTAGE-AMT *
                       MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCM-STORAGE-AMT > 0 AND MFCM-STORAGE-AMT < 2.00
               MOVE 2.00 TO MFCM-STORAGE-AMT
           END-IF
           IF MFCM-STORAGE-AMT > 100.00
               MOVE 100.00 TO MFCM-STORAGE-AMT
           END-IF
           ADD MFCM-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IX -> RS WHEN EFF DATE IS PAST
      * RS -> CP WHEN EFF DATE IS PAST
      * CP -> PG WHEN EFF DATE IS PAST
      * PG -> IX WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4340-SET-TRAN.
           EVALUATE MFCT-TRAN-CD
               WHEN 'IX'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RS' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RS'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PG' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'IX' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4350-CHECK-NOTICE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO MFCM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4360-LIMIT-RESEARCH.
           IF MFCM-RESEARCH-AMT < 50.00
               MOVE 50.00 TO MFCM-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-RESEARCH-AMT > 1000.00
               MOVE 1000.00 TO MFCM-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4370-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE FICHE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4380-DESCRIBE-FICHE-TYPE.
           EVALUATE MFCM-FICHE-TYPE-CD
               WHEN 'AW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AST -> JXN WHEN CLOSE DATE IS PAST
      * JXN -> KJJ WHEN CLOSE DATE IS PAST
      * KJJ -> TZL WHEN CLOSE DATE IS PAST
      * TZL -> XZM WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4390-SET-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JXN' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JXN'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJJ' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KJJ'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TZL' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TZL'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZM' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE FEE ON THE STORAGE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4400-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN MFCM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO MFCM-POSTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-STOP-FLAG = 'Y'
                   COMPUTE MFCM-POSTAGE-AMT ROUNDED = MFCT-STORAGE-AMT *
                       MFCM-FEE-PCT / 200
               WHEN MFCT-TRAN-CD = 'IX'
                   MOVE 10.00 TO MFCM-POSTAGE-AMT
               WHEN MFCT-TRAN-CD = 'RS'
                   MOVE 5.00 TO MFCM-POSTAGE-AMT
               WHEN OTHER
                   COMPUTE MFCM-POSTAGE-AMT ROUNDED = MFCT-STORAGE-AMT *
                       MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCM-POSTAGE-AMT > 0 AND MFCM-POSTAGE-AMT < 10.00
               MOVE 10.00 TO MFCM-POSTAGE-AMT
           END-IF
           IF MFCM-POSTAGE-AMT > 75.00
               MOVE 75.00 TO MFCM-POSTAGE-AMT
           END-IF
           ADD MFCM-POSTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4410-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO MFCM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       4420-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-POSTAGE-AMT ROUNDED =
                   MFCM-RESEARCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCM-POSTAGE-AMT
           END-IF
           ADD MFCM-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * COPY FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4430-LIMIT-COPY-FEE.
           IF MFCT-COPY-FEE-AMT < 50.00
               MOVE 50.00 TO MFCT-COPY-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-COPY-FEE-AMT > 10000.00
               MOVE 10000.00 TO MFCT-COPY-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4440-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFCT-POSTAGE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MFCT-POSTAGE-AMT - (WS-WORK-AMT * 2)
           COMPUTE MFCM-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4450-CHECK-LAST-STMT.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO MFCM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4460-LIMIT-POSTAGE.
           IF MFCM-POSTAGE-AMT < 10.00
               MOVE 10.00 TO MFCM-POSTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-POSTAGE-AMT > 5000.00
               MOVE 5000.00 TO MFCM-POSTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH FEE ON THE POSTAGE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4470-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN MFCM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO MFCT-RESEARCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MFCT-RESEARCH-AMT ROUNDED = MFCM-POSTAGE-AMT
                       * MFCM-DISCOUNT / 200
               WHEN MFCM-BRANCH-CD = 'AT'
                   MOVE 15.00 TO MFCT-RESEARCH-AMT
               WHEN MFCM-BRANCH-CD = 'KR'
                   MOVE 12.00 TO MFCT-RESEARCH-AMT
               WHEN OTHER
                   COMPUTE MFCT-RESEARCH-AMT ROUNDED = MFCM-POSTAGE-AMT
                       * MFCM-DISCOUNT / 100
           END-EVALUATE
           IF MFCT-RESEARCH-AMT > 0 AND MFCT-RESEARCH-AMT < 5.00
               MOVE 5.00 TO MFCT-RESEARCH-AMT
           END-IF
           IF MFCT-RESEARCH-AMT > 100.00
               MOVE 100.00 TO MFCT-RESEARCH-AMT
           END-IF
           ADD MFCT-RESEARCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4480-COUNT-CYCLE.
           IF MFCM-CYCLE-CNT < 9999999
               ADD 1 TO MFCM-CYCLE-CNT
           END-IF
           IF MFCM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO MFCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4490-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCM-RESEARCH-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MFCM-RESEARCH-AMT - (WS-WORK-AMT * 3)
           COMPUTE MFCM-POSTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4500-FLAG-COPY-FEE.
           IF MFCM-COPY-FEE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MFCM-COPY-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4510-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO MFCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4520-ACCRUE-RESEARCH.
           IF MFCT-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-RESEARCH-AMT * MFCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO MFCT-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4530-FLAG-STORAGE.
           IF MFCM-STORAGE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MFCM-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4540-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-BAND-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT NOT < WS-COPY-FEE-BAND-LOW(WS-TX)
                   MOVE WS-COPY-FEE-BAND-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4550-ACCRUE-POSTAGE.
           IF MFCM-POSTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-POSTAGE-AMT * MFCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E936 ADDR REQUIRED
      * E863 WORK AMT NEGATIVE
      * E932 WORK AMT OVER 10000
      * E136 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4560-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-ADDR = SPACES
                   MOVE 'E936' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E863' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E932' TO WS-REJECT-CODE
               WHEN MFCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E136' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4570-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF MFCM-NOTICE-DATE > 0 AND MFCM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4580-DESCRIBE-TRAN.
           EVALUATE MFCT-TRAN-CD
               WHEN 'IX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE BAND TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4590-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-BAND-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MFCM-STORAGE-AMT NOT < WS-COPY-FEE-BAND-LOW(WS-TX)
                   MOVE WS-COPY-FEE-BAND-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4600-ACCRUE-RESEARCH.
           IF MFCT-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-RESEARCH-AMT * MFCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MFCM-COPY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE YTD FEE ON THE RESEARCH AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4610-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO MFCM-STORAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE MFCM-STORAGE-YTD ROUNDED = MFCT-RESEARCH-AMT
                       * MFCM-FEE-PCT / 200
               WHEN MFCM-BRANCH-CD = 'AT'
                   MOVE 7.00 TO MFCM-STORAGE-YTD
               WHEN MFCM-BRANCH-CD = 'KR'
                   MOVE 15.00 TO MFCM-STORAGE-YTD
               WHEN OTHER
                   COMPUTE MFCM-STORAGE-YTD ROUNDED = MFCT-RESEARCH-AMT
                       * MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCM-STORAGE-YTD > 0 AND MFCM-STORAGE-YTD < 2.00
               MOVE 2.00 TO MFCM-STORAGE-YTD
           END-IF
           IF MFCM-STORAGE-YTD > 50.00
               MOVE 50.00 TO MFCM-STORAGE-YTD
           END-IF
           ADD MFCM-STORAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4620-DESCRIBE-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ND'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4630-DESCRIBE-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'HH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ND'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * G -> K WHEN EXPIRY DATE IS PAST
      * K -> W WHEN EXPIRY DATE IS PAST
      * W -> Y WHEN EXPIRY DATE IS PAST
      * Y -> G WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4640-SET-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE FICHE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4650-DESCRIBE-FICHE-TYPE.
           EVALUATE MFCM-FICHE-TYPE-CD
               WHEN 'AW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * G -> K WHEN LAST MAINT DATE IS PAST
      * K -> W WHEN LAST MAINT DATE IS PAST
      * W -> Y WHEN LAST MAINT DATE IS PAST
      * Y -> G WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4660-SET-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'K' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'W' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'Y' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF MFCM-LAST-MAINT-DATE > 0 AND MFCM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'G' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE COPY FEE FEE ON THE POSTAGE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4670-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN MFCM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO MFCM-COPY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-STOP-FLAG = 'Y'
                   COMPUTE MFCM-COPY-FEE-AMT ROUNDED = MFCM-POSTAGE-YTD
                       * MFCM-MARGIN / 200
               WHEN MFCM-FICHE-TYPE-CD = 'AW'
                   MOVE 15.00 TO MFCM-COPY-FEE-AMT
               WHEN MFCM-FICHE-TYPE-CD = 'JM'
                   MOVE 3.00 TO MFCM-COPY-FEE-AMT
               WHEN OTHER
                   COMPUTE MFCM-COPY-FEE-AMT ROUNDED = MFCM-POSTAGE-YTD
                       * MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCM-COPY-FEE-AMT > 0 AND MFCM-COPY-FEE-AMT < 5.00
               MOVE 5.00 TO MFCM-COPY-FEE-AMT
           END-IF
           IF MFCM-COPY-FEE-AMT > 25.00
               MOVE 25.00 TO MFCM-COPY-FEE-AMT
           END-IF
           ADD MFCM-COPY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4680-CHECK-NOTICE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4690-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN MFCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO MFCM-POSTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-EXEMPT-FLAG = 'Y'
                   COMPUTE MFCM-POSTAGE-YTD ROUNDED = WS-WORK-AMT *
                       MFCM-DISCOUNT / 200
               WHEN MFCM-FICHE-TYPE-CD = 'AW'
                   MOVE 20.00 TO MFCM-POSTAGE-YTD
               WHEN MFCM-FICHE-TYPE-CD = 'JM'
                   MOVE 12.00 TO MFCM-POSTAGE-YTD
               WHEN OTHER
                   COMPUTE MFCM-POSTAGE-YTD ROUNDED = WS-WORK-AMT *
                       MFCM-DISCOUNT / 100
           END-EVALUATE
           IF MFCM-POSTAGE-YTD > 0 AND MFCM-POSTAGE-YTD < 10.00
               MOVE 10.00 TO MFCM-POSTAGE-YTD
           END-IF
           IF MFCM-POSTAGE-YTD > 100.00
               MOVE 100.00 TO MFCM-POSTAGE-YTD
           END-IF
           ADD MFCM-POSTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4700-FLAG-STORAGE.
           IF MFCT-STORAGE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFCT-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4710-LIMIT-STORAGE.
           IF MFCM-STORAGE-YTD < 25.00
               MOVE 25.00 TO MFCM-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-STORAGE-YTD > 50000.00
               MOVE 50000.00 TO MFCM-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4720-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF MFCT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE MFCT-POST-DATE TO DTU-DATE-1
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
      * POSTAGE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4730-FLAG-POSTAGE.
           IF MFCM-POSTAGE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFCM-POSTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESEARCH YTD.
      *----------------------------------------------------------------*
       4740-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-RESEARCH-YTD ROUNDED =
                   MFCM-RESEARCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCM-RESEARCH-YTD
           END-IF
           ADD MFCM-RESEARCH-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RESEARCH AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4750-FLAG-RESEARCH.
           IF MFCT-RESEARCH-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCT-RESEARCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4760-CHECK-OPEN.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO MFCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E602 MEMO REQUIRED
      * E276 COPY FEE AMT NEGATIVE
      * E253 COPY FEE AMT OVER 50000
      * E352 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4770-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-MEMO = SPACES
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN MFCT-COPY-FEE-AMT < 0
                   MOVE 'E276' TO WS-REJECT-CODE
               WHEN MFCT-COPY-FEE-AMT > 50000.00
                   MOVE 'E253' TO WS-REJECT-CODE
               WHEN MFCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E352' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4780-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MFCM-RESEARCH-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = WS-WORK-AMT-2 *
                       MFCM-FEE-PCT / 200
               WHEN MFCM-TYPE-CD = 'G'
                   MOVE 3.00 TO MFCM-RESEARCH-YTD
               WHEN MFCM-TYPE-CD = 'K'
                   MOVE 5.00 TO MFCM-RESEARCH-YTD
               WHEN OTHER
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = WS-WORK-AMT-2 *
                       MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCM-RESEARCH-YTD > 0 AND MFCM-RESEARCH-YTD < 5.00
               MOVE 5.00 TO MFCM-RESEARCH-YTD
           END-IF
           IF MFCM-RESEARCH-YTD > 75.00
               MOVE 75.00 TO MFCM-RESEARCH-YTD
           END-IF
           ADD MFCM-RESEARCH-YTD TO WS-TOTAL-FEES.

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
                   MOVE MFRISK-REVIEW-FLAG
                     TO MFCM-REVIEW-FLAG
                   MOVE MFRISK-AUTO-RENEW-FLAG
                     TO MFCM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-MFRISK' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

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
                   MOVE MFDTE-EXEMPT-FLAG
                     TO MFCM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-MFDTE' TO ABND-PARAGRAPH
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
           DISPLAY 'MFCPOST READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'MFCPOST' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
