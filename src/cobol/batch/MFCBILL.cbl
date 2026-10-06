      *================================================================*
      * PROGRAM  : MFCBILL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MICROFICHE STATEMENTS - CYCLE BILLING. READS THE MFC*
      *            TRANSACTIONS, UPDATES THE MFC MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. STATEMENT MICROFICHE INDEXING,  *
      *            RETENTION AND RESEARCH FEES                         *
      * INPUT    : MFCMST (KSDS), MFCTRN, BANKCTL                      *
      * OUTPUT   : MFCOUT, MFCBIRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, MFDTE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-01-15 SKN  ORIGINAL                                       *
      * 1998-03-06 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-12-13 SKN  COPY-FEE FEE SCHEDULE UPDATE PRB9065           *
      * 2000-07-17 MKL  AUDIT FINDING - POSTAGE CONTROL TOTALS INC8517 *
      * 2002-04-14 GAF  STORAGE EXCEPTIONS TO OPS QUEUE PRB3114        *
      * 2003-07-08 LMB  POSTAGE FEE SCHEDULE UPDATE REQ7863            *
      * 2003-02-14 PQR  RESEARCH ROUNDING - HALF UP REQ9270            *
      * 2004-11-03 BHS  POSTAGE NOTICE TEXT PER LEGAL REQ3582          *
      * 2004-01-08 CRP  AUDIT FINDING - STORAGE CONTROL TOTALS CHG6101 *
      * 2005-05-02 PQR  AUDIT FINDING - POSTAGE CONTROL TOTALS REQ6383 *
      * 2008-03-08 EWS  COPY-FEE EXCEPTIONS TO OPS QUEUE INC6012       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFCBILL.
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
           SELECT REPORT-FILE ASSIGN TO "MFCBIRPT"
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
                                           VALUE 11.000000.
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
      * WS-STORAGE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000091000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000502500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000409000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000506500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007000000000291000'.
       01  WS-STORAGE-TIER-TBL REDEFINES WS-STORAGE-TIER-VALUES.
           05  WS-STORAGE-TIER-ROW OCCURS 5 TIMES.
               10  WS-STORAGE-TIER-LOW     PIC 9(09)V99.
               10  WS-STORAGE-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STORAGE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000016000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000300000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000421500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000498500'.
       01  WS-STORAGE-SCHED-TBL REDEFINES WS-STORAGE-SCHED-VALUES.
           05  WS-STORAGE-SCHED-ROW OCCURS 4 TIMES.
               10  WS-STORAGE-SCHED-LOW    PIC 9(09)V99.
               10  WS-STORAGE-SCHED-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  MFCBILL  MICROFICHE STATEMENTS'.
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
               PERFORM 3000-PRORATE-RESEARCH
               PERFORM 3010-SPLIT-COPY-FEE
               PERFORM 3020-AGE-LAST-PAY
               PERFORM 3030-SET-TYPE
               PERFORM 3040-DESCRIBE-RETENTION
               PERFORM 3050-SET-CHANNEL
               PERFORM 3060-SPLIT-COPY-FEE
               PERFORM 3070-SELECT-FEE-PCT
               PERFORM 3080-FLAG-POSTAGE
               PERFORM 3090-SET-STATUS
               PERFORM 3100-FLAG-STORAGE
               PERFORM 3110-SPLIT-RESEARCH
               PERFORM 3120-SELECT-FEE-PCT
               PERFORM 3130-LIMIT-ITEM
               PERFORM 3140-ASSESS-NOTICE-SENT
               PERFORM 3150-PRORATE-COPY-FEE
               PERFORM 3160-EDIT-CONTACT
               PERFORM 3170-SET-STATUS
               PERFORM 3180-ACCRUE-COPY-FEE
               PERFORM 3190-DESCRIBE-CHANNEL
               PERFORM 3200-COUNT-ITEM
               PERFORM 3210-PRORATE-COPY-FEE
               PERFORM 3220-EDIT-REJECT
               PERFORM 3230-AGE-OPEN
               PERFORM 3240-ASSESS-REVIEW
               PERFORM 3250-EDIT-MEMO
               PERFORM 3260-LIMIT-STORAGE
               PERFORM 3270-SET-CHANNEL
               PERFORM 3280-DESCRIBE-RETENTION
               PERFORM 3290-SELECT-DISCOUNT
               PERFORM 3300-LIMIT-RESEARCH
               PERFORM 3310-SET-STATUS
               PERFORM 3320-EDIT-REJECT
               PERFORM 3330-SET-CHANNEL
               PERFORM 3340-LIMIT-COPY-FEE
               PERFORM 3350-DESCRIBE-RETENTION
               PERFORM 3360-ACCUM-TYPE
               PERFORM 3370-ACCRUE-POSTAGE
               PERFORM 3380-AGE-CLOSE
               PERFORM 3390-PRORATE-RESEARCH
               PERFORM 3400-SELECT-FEE-PCT
               PERFORM 3410-SELECT-DISCOUNT
               PERFORM 3420-FLAG-STORAGE
               PERFORM 3430-PRORATE-COPY-FEE
               PERFORM 3440-AGE-LAST-MAINT
               PERFORM 3450-ACCRUE-DISCOUNT
               PERFORM 3460-ASSESS-NOTICE-SENT
               PERFORM 3470-SET-CHANNEL
               PERFORM 3480-PRORATE-STORAGE
               PERFORM 3490-FLAG-ITEM
               PERFORM 3500-COUNT-PAYMENT
               PERFORM 3510-SET-CHANNEL
               PERFORM 3520-SELECT-DISCOUNT
               PERFORM 3530-PRORATE-STORAGE
               PERFORM 3540-LIMIT-RESEARCH
               PERFORM 3550-LIMIT-ITEM
               PERFORM 3560-LIMIT-COPY-FEE
               PERFORM 3570-CHECK-EFF
               PERFORM 3580-SELECT-DISCOUNT
               PERFORM 3590-CHECK-NOTICE
               PERFORM 3600-CHECK-LAST-STMT
               PERFORM 3610-AGE-EXPIRY
               PERFORM 3620-DESCRIBE-STATUS
               PERFORM 3630-LIMIT-POSTAGE
               PERFORM 3640-ACCRUE-POSTAGE
               PERFORM 3650-LIMIT-COPY-FEE
               PERFORM 3660-ACCUM-TYPE
               PERFORM 3670-DESCRIBE-TYPE
               PERFORM 3680-LIMIT-RESEARCH
               PERFORM 3690-AGE-POST
               PERFORM 3700-ACCRUE-FEE-PCT
               PERFORM 3710-ACCUM-FICHE-TYPE
               PERFORM 3720-SET-STATUS
               PERFORM 3730-COUNT-CYCLE
               PERFORM 3740-PRORATE-RESEARCH
               PERFORM 3750-CHECK-EXPIRY
               PERFORM 3760-ACCUM-CHANNEL
               PERFORM 3770-SPLIT-POSTAGE
               PERFORM 3780-CHECK-POST
               PERFORM 3790-DESCRIBE-RETENTION
               PERFORM 3800-COUNT-CYCLE
               PERFORM 3810-CHECK-EXPIRY
               PERFORM 3820-ACCRUE-STORAGE
               PERFORM 3830-CHECK-MATURITY
               PERFORM 3840-ASSESS-EXEMPT
               PERFORM 3850-SELECT-FEE-PCT
               PERFORM 3860-PRORATE-RESEARCH
               PERFORM 3870-CHECK-NOTICE
               PERFORM 3880-LIMIT-COPY-FEE
               PERFORM 3890-FLAG-RESEARCH
               PERFORM 3900-SET-FICHE-TYPE
               PERFORM 3910-COUNT-PAYMENT
               PERFORM 3920-CHECK-NEXT-DUE
               PERFORM 3930-LIMIT-COPY-FEE
               PERFORM 3940-AGE-EXPIRY
               PERFORM 3950-EDIT-DESC
               PERFORM 3960-LIMIT-STORAGE
               PERFORM 3970-SELECT-DISCOUNT
               PERFORM 3980-AGE-LAST-MAINT
               PERFORM 3990-SET-FICHE-TYPE
               PERFORM 4000-CHECK-POST
               PERFORM 4010-SELECT-FEE-PCT
               PERFORM 4020-SPLIT-STORAGE
               PERFORM 4030-ACCRUE-RESEARCH
               PERFORM 4040-PRORATE-STORAGE
               PERFORM 4050-AGE-NOTICE
               PERFORM 4060-CHECK-MATURITY
               PERFORM 4070-SPLIT-STORAGE
               PERFORM 4080-SET-CHANNEL
               PERFORM 4090-ASSESS-REVERSAL
               PERFORM 4100-COUNT-PAYMENT
               PERFORM 4110-SET-BRANCH
               PERFORM 4120-EDIT-ADDR
               PERFORM 4130-LIMIT-STORAGE
               PERFORM 4140-AGE-CLOSE
               PERFORM 4150-LIMIT-POSTAGE
               PERFORM 4160-PRORATE-POSTAGE
               PERFORM 4170-CHECK-POST
               PERFORM 4180-PRORATE-POSTAGE
               PERFORM 4190-SET-RETENTION
               PERFORM 4200-FLAG-STORAGE
               PERFORM 4210-ASSESS-AUTO-RENEW
               PERFORM 4220-ACCUM-FICHE-TYPE
               PERFORM 4230-CHECK-LAST-STMT
               PERFORM 4240-COUNT-NOTICE
               PERFORM 4250-SPLIT-COPY-FEE
               PERFORM 4260-LIMIT-RESEARCH
               PERFORM 4270-FLAG-COPY-FEE
               PERFORM 4280-LIMIT-COPY-FEE
               PERFORM 4290-ACCUM-TRAN
               PERFORM 4300-ACCRUE-STORAGE
               PERFORM 4310-SELECT-DISCOUNT
               PERFORM 4320-LIMIT-RESEARCH
               PERFORM 4330-SET-TYPE
               PERFORM 4340-DESCRIBE-CHANNEL
               PERFORM 4350-EDIT-REF-NO
               PERFORM 4360-SELECT-MARGIN
               PERFORM 4370-ASSESS-REVIEW
               PERFORM 4380-SELECT-MARGIN
               PERFORM 4390-COUNT-ITEM
               PERFORM 4400-LIMIT-COPY-FEE
               PERFORM 4410-PRORATE-POSTAGE
               PERFORM 4420-SPLIT-STORAGE
               PERFORM 4430-COUNT-CYCLE
               PERFORM 4440-ACCUM-TRAN
               PERFORM 4450-EDIT-REJECT
               PERFORM 4460-SELECT-MARGIN
               PERFORM 4470-SPLIT-RESEARCH
               PERFORM 4480-ACCRUE-COPY-FEE
               PERFORM 4490-PRORATE-RESEARCH
               PERFORM 4500-CHECK-MATURITY
               PERFORM 4510-SELECT-DISCOUNT
               PERFORM 4520-CHECK-NEXT-DUE
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
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3000-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-RESEARCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT COPY FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3010-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = MFCT-COPY-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MFCT-COPY-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE MFCT-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-LAST-PAY.
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
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * G -> K WHEN LAST PAY DATE IS PAST
      * K -> W WHEN LAST PAY DATE IS PAST
      * W -> Y WHEN LAST PAY DATE IS PAST
      * Y -> G WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN EFF DATE IS PAST
      * GL -> HH WHEN EFF DATE IS PAST
      * HH -> LM WHEN EFF DATE IS PAST
      * LM -> MD WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COPY FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3060-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = MFCM-COPY-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MFCM-COPY-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE MFCM-RESEARCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE POSTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MFCM-POSTAGE-YTD NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3080-FLAG-POSTAGE.
           IF MFCT-POSTAGE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCT-POSTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AST -> JXN WHEN LAST PAY DATE IS PAST
      * JXN -> KJJ WHEN LAST PAY DATE IS PAST
      * KJJ -> TZL WHEN LAST PAY DATE IS PAST
      * TZL -> XZM WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JXN' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JXN'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJJ' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KJJ'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TZL' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TZL'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZM' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3100-FLAG-STORAGE.
           IF MFCT-STORAGE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MFCT-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCM-RESEARCH-YTD / 12
           COMPUTE WS-WORK-AMT-2 = MFCM-RESEARCH-YTD - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MFCM-COPY-FEE-YTD NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3130-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COPY FEE FEE ON THE POSTAGE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3140-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN MFCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO MFCM-COPY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-STOP-FLAG = 'Y'
                   COMPUTE MFCM-COPY-FEE-AMT ROUNDED = MFCM-POSTAGE-YTD
                       * MFCM-FEE-PCT / 200
               WHEN MFCM-TYPE-CD = 'G'
                   MOVE 10.00 TO MFCM-COPY-FEE-AMT
               WHEN MFCM-TYPE-CD = 'K'
                   MOVE 10.00 TO MFCM-COPY-FEE-AMT
               WHEN OTHER
                   COMPUTE MFCM-COPY-FEE-AMT ROUNDED = MFCM-POSTAGE-YTD
                       * MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCM-COPY-FEE-AMT > 0 AND MFCM-COPY-FEE-AMT < 2.00
               MOVE 2.00 TO MFCM-COPY-FEE-AMT
           END-IF
           IF MFCM-COPY-FEE-AMT > 75.00
               MOVE 75.00 TO MFCM-COPY-FEE-AMT
           END-IF
           ADD MFCM-COPY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COPY FEE AMT.
      *----------------------------------------------------------------*
       3150-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-COPY-FEE-AMT ROUNDED =
                   MFCM-RESEARCH-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-COPY-FEE-AMT
           END-IF
           ADD MFCT-COPY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E872 CONTACT REQUIRED
      * E217 POSTAGE AMT NEGATIVE
      * E463 POSTAGE AMT OVER 25000
      * E536 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3160-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CONTACT = SPACES
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN MFCT-POSTAGE-AMT < 0
                   MOVE 'E217' TO WS-REJECT-CODE
               WHEN MFCT-POSTAGE-AMT > 25000.00
                   MOVE 'E463' TO WS-REJECT-CODE
               WHEN MFCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E536' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AST -> JXN WHEN NOTICE DATE IS PAST
      * JXN -> KJJ WHEN NOTICE DATE IS PAST
      * KJJ -> TZL WHEN NOTICE DATE IS PAST
      * TZL -> XZM WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3170-SET-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   IF MFCM-NOTICE-DATE > 0 AND MFCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JXN' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JXN'
                   IF MFCM-NOTICE-DATE > 0 AND MFCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJJ' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KJJ'
                   IF MFCM-NOTICE-DATE > 0 AND MFCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TZL' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TZL'
                   IF MFCM-NOTICE-DATE > 0 AND MFCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZM' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COPY FEE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3180-ACCRUE-COPY-FEE.
           IF MFCT-COPY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-COPY-FEE-AMT * MFCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO MFCM-COPY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ND'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3200-COUNT-ITEM.
           IF MFCM-ITEM-CNT < 9999999
               ADD 1 TO MFCM-ITEM-CNT
           END-IF
           IF MFCM-ITEM-CNT NOT < 12
               MOVE 'Y' TO MFCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3210-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MFCM-COPY-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E939 REJECT CD REQUIRED
      * E519 WORK AMT NEGATIVE
      * E637 WORK AMT OVER 5000
      * E773 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3220-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-REJECT-CD = SPACES
                   MOVE 'E939' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E637' TO WS-REJECT-CODE
               WHEN MFCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3230-AGE-OPEN.
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
                   ADD MFCM-COPY-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MFCM-COPY-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFCM-COPY-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCM-COPY-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-COPY-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE COPY FEE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3240-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN MFCM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO MFCT-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE MFCT-STORAGE-AMT ROUNDED = MFCT-COPY-FEE-AMT
                       * MFCM-FEE-PCT / 200
               WHEN MFCM-RETENTION-CD = 'KT'
                   MOVE 7.00 TO MFCT-STORAGE-AMT
               WHEN MFCM-RETENTION-CD = 'SJ'
                   MOVE 15.00 TO MFCT-STORAGE-AMT
               WHEN OTHER
                   COMPUTE MFCT-STORAGE-AMT ROUNDED = MFCT-COPY-FEE-AMT
                       * MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCT-STORAGE-AMT > 0 AND MFCT-STORAGE-AMT < 2.00
               MOVE 2.00 TO MFCT-STORAGE-AMT
           END-IF
           IF MFCT-STORAGE-AMT > 250.00
               MOVE 250.00 TO MFCT-STORAGE-AMT
           END-IF
           ADD MFCT-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E346 MEMO REQUIRED
      * E476 POSTAGE AMT NEGATIVE
      * E874 POSTAGE AMT OVER 1000000
      * E454 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-MEMO = SPACES
                   MOVE 'E346' TO WS-REJECT-CODE
               WHEN MFCT-POSTAGE-AMT < 0
                   MOVE 'E476' TO WS-REJECT-CODE
               WHEN MFCT-POSTAGE-AMT > 1000000.00
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN MFCM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E454' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-STORAGE.
           IF MFCM-STORAGE-AMT < 25.00
               MOVE 25.00 TO MFCM-STORAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-STORAGE-AMT > 10000.00
               MOVE 10000.00 TO MFCM-STORAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN NOTICE DATE IS PAST
      * GL -> HH WHEN NOTICE DATE IS PAST
      * HH -> LM WHEN NOTICE DATE IS PAST
      * LM -> MD WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3270-SET-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   IF MFCM-NOTICE-DATE > 0 AND MFCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFCM-NOTICE-DATE > 0 AND MFCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFCM-NOTICE-DATE > 0 AND MFCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFCM-NOTICE-DATE > 0 AND MFCM-NOTICE-DATE <
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
       3280-DESCRIBE-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MFCM-STORAGE-AMT NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3300-LIMIT-RESEARCH.
           IF MFCT-RESEARCH-AMT < 10.00
               MOVE 10.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-RESEARCH-AMT > 5000.00
               MOVE 5000.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AST -> JXN WHEN OPEN DATE IS PAST
      * JXN -> KJJ WHEN OPEN DATE IS PAST
      * KJJ -> TZL WHEN OPEN DATE IS PAST
      * TZL -> XZM WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3310-SET-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JXN' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JXN'
                   IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJJ' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KJJ'
                   IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TZL' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TZL'
                   IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZM' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E739 REJECT CD REQUIRED
      * E322 POSTAGE AMT NEGATIVE
      * E017 POSTAGE AMT OVER 50000
      * E362 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-REJECT-CD = SPACES
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN MFCT-POSTAGE-AMT < 0
                   MOVE 'E322' TO WS-REJECT-CODE
               WHEN MFCT-POSTAGE-AMT > 50000.00
                   MOVE 'E017' TO WS-REJECT-CODE
               WHEN MFCM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN LAST STMT DATE IS PAST
      * GL -> HH WHEN LAST STMT DATE IS PAST
      * HH -> LM WHEN LAST STMT DATE IS PAST
      * LM -> MD WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3330-SET-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COPY FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3340-LIMIT-COPY-FEE.
           IF MFCM-COPY-FEE-AMT < 50.00
               MOVE 50.00 TO MFCM-COPY-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-COPY-FEE-AMT > 50000.00
               MOVE 50000.00 TO MFCM-COPY-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3350-DESCRIBE-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3360-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-POSTAGE.
           IF MFCM-POSTAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-POSTAGE-YTD * MFCM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO MFCM-COPY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3380-AGE-CLOSE.
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
               WHEN WS-DAYS NOT > 15
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3390-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MFCT-RESEARCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE COPY FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MFCM-COPY-FEE-AMT NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE RESEARCH YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3410-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MFCM-RESEARCH-YTD NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3420-FLAG-STORAGE.
           IF MFCM-STORAGE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MFCM-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COPY FEE AMT.
      *----------------------------------------------------------------*
       3430-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-COPY-FEE-AMT ROUNDED =
                   MFCM-RESEARCH-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCM-COPY-FEE-AMT
           END-IF
           ADD MFCM-COPY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-LAST-MAINT.
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
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3450-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * MFCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MFCM-COPY-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3460-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN MFCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO MFCM-STORAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE MFCM-STORAGE-YTD ROUNDED = WS-WORK-AMT-2 *
                       MFCM-MARGIN / 200
               WHEN MFCM-RETENTION-CD = 'KT'
                   MOVE 3.00 TO MFCM-STORAGE-YTD
               WHEN MFCM-RETENTION-CD = 'SJ'
                   MOVE 12.00 TO MFCM-STORAGE-YTD
               WHEN OTHER
                   COMPUTE MFCM-STORAGE-YTD ROUNDED = WS-WORK-AMT-2 *
                       MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCM-STORAGE-YTD > 0 AND MFCM-STORAGE-YTD < 1.00
               MOVE 1.00 TO MFCM-STORAGE-YTD
           END-IF
           IF MFCM-STORAGE-YTD > 150.00
               MOVE 150.00 TO MFCM-STORAGE-YTD
           END-IF
           ADD MFCM-STORAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN NEXT DUE DATE IS PAST
      * GL -> HH WHEN NEXT DUE DATE IS PAST
      * HH -> LM WHEN NEXT DUE DATE IS PAST
      * LM -> MD WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3470-SET-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE YTD.
      *----------------------------------------------------------------*
       3480-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-STORAGE-YTD ROUNDED =
                   MFCM-COPY-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCM-STORAGE-YTD
           END-IF
           ADD MFCM-STORAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3490-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3500-COUNT-PAYMENT.
           IF MFCM-PAYMENT-CNT < 9999999
               ADD 1 TO MFCM-PAYMENT-CNT
           END-IF
           IF MFCM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN LAST STMT DATE IS PAST
      * GL -> HH WHEN LAST STMT DATE IS PAST
      * HH -> LM WHEN LAST STMT DATE IS PAST
      * LM -> MD WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3510-SET-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3520-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MFCM-COPY-FEE-YTD NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3530-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-STORAGE-AMT ROUNDED =
                   MFCM-COPY-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-STORAGE-AMT
           END-IF
           ADD MFCT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RESEARCH AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3540-LIMIT-RESEARCH.
           IF MFCM-RESEARCH-AMT < 50.00
               MOVE 50.00 TO MFCM-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-RESEARCH-AMT > 2500.00
               MOVE 2500.00 TO MFCM-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3550-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3560-LIMIT-COPY-FEE.
           IF MFCM-COPY-FEE-YTD < 25.00
               MOVE 25.00 TO MFCM-COPY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-COPY-FEE-YTD > 2500.00
               MOVE 2500.00 TO MFCM-COPY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3570-CHECK-EFF.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE STORAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MFCM-STORAGE-YTD NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-NOTICE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO MFCM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3600-CHECK-LAST-STMT.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO MFCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3610-AGE-EXPIRY.
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
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3620-DESCRIBE-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JXN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KJJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TZL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XZM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * POSTAGE AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3630-LIMIT-POSTAGE.
           IF MFCM-POSTAGE-AMT < 100.00
               MOVE 100.00 TO MFCM-POSTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-POSTAGE-AMT > 1000.00
               MOVE 1000.00 TO MFCM-POSTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3640-ACCRUE-POSTAGE.
           IF MFCM-POSTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-POSTAGE-AMT * MFCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MFCT-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3650-LIMIT-COPY-FEE.
           IF MFCM-COPY-FEE-AMT < 10.00
               MOVE 10.00 TO MFCM-COPY-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-COPY-FEE-AMT > 500.00
               MOVE 500.00 TO MFCM-COPY-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE POSTAGE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3660-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-TYPE-CD
                   ADD MFCT-POSTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCT-POSTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'K'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RESEARCH YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3680-LIMIT-RESEARCH.
           IF MFCM-RESEARCH-YTD < 25.00
               MOVE 25.00 TO MFCM-RESEARCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-RESEARCH-YTD > 50000.00
               MOVE 50000.00 TO MFCM-RESEARCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-POST.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3700-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * MFCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MFCM-COPY-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESEARCH AMT BY FICHE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3710-ACCUM-FICHE-TYPE.
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
      * STATUS TRANSITIONS FOR STATUS CD:
      * AST -> JXN WHEN EFF DATE IS PAST
      * JXN -> KJJ WHEN EFF DATE IS PAST
      * KJJ -> TZL WHEN EFF DATE IS PAST
      * TZL -> XZM WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JXN' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JXN'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KJJ' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KJJ'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TZL' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TZL'
                   IF MFCT-EFF-DATE > 0 AND MFCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XZM' TO MFCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-CYCLE.
           IF MFCM-CYCLE-CNT < 9999999
               ADD 1 TO MFCM-CYCLE-CNT
           END-IF
           IF MFCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESEARCH YTD.
      *----------------------------------------------------------------*
       3740-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-RESEARCH-YTD ROUNDED =
                   MFCM-STORAGE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCM-RESEARCH-YTD
           END-IF
           ADD MFCM-RESEARCH-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3750-CHECK-EXPIRY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO MFCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COPY FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3760-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCT-CHANNEL-CD
                   ADD MFCT-COPY-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCT-COPY-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3770-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFCM-POSTAGE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = MFCM-POSTAGE-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3780-CHECK-POST.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO MFCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3790-DESCRIBE-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3800-COUNT-CYCLE.
           IF MFCM-CYCLE-CNT < 9999999
               ADD 1 TO MFCM-CYCLE-CNT
           END-IF
           IF MFCM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO MFCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3810-CHECK-EXPIRY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO MFCM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3820-ACCRUE-STORAGE.
           IF MFCT-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-STORAGE-AMT * MFCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MFCM-POSTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3830-CHECK-MATURITY.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO MFCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COPY FEE FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3840-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MFCM-COPY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE MFCM-COPY-FEE-AMT ROUNDED = WS-WORK-AMT-2 *
                       MFCM-FEE-PCT / 200
               WHEN MFCT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO MFCM-COPY-FEE-AMT
               WHEN MFCT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO MFCM-COPY-FEE-AMT
               WHEN OTHER
                   COMPUTE MFCM-COPY-FEE-AMT ROUNDED = WS-WORK-AMT-2 *
                       MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCM-COPY-FEE-AMT > 0 AND MFCM-COPY-FEE-AMT < 2.00
               MOVE 2.00 TO MFCM-COPY-FEE-AMT
           END-IF
           IF MFCM-COPY-FEE-AMT > 250.00
               MOVE 250.00 TO MFCM-COPY-FEE-AMT
           END-IF
           ADD MFCM-COPY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3850-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESEARCH YTD.
      *----------------------------------------------------------------*
       3860-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-RESEARCH-YTD ROUNDED =
                   MFCT-STORAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCM-RESEARCH-YTD
           END-IF
           ADD MFCM-RESEARCH-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3870-CHECK-NOTICE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO MFCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3880-LIMIT-COPY-FEE.
           IF MFCM-COPY-FEE-YTD < 100.00
               MOVE 100.00 TO MFCM-COPY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-COPY-FEE-YTD > 10000.00
               MOVE 10000.00 TO MFCM-COPY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3890-FLAG-RESEARCH.
           IF MFCM-RESEARCH-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MFCM-RESEARCH-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR FICHE TYPE CD:
      * AW -> JM WHEN LAST PAY DATE IS PAST
      * JM -> KV WHEN LAST PAY DATE IS PAST
      * KV -> MD WHEN LAST PAY DATE IS PAST
      * MD -> NR WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3900-SET-FICHE-TYPE.
           EVALUATE MFCM-FICHE-TYPE-CD
               WHEN 'AW'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MD'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NR' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3910-COUNT-PAYMENT.
           IF MFCM-PAYMENT-CNT < 9999999
               ADD 1 TO MFCM-PAYMENT-CNT
           END-IF
           IF MFCM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO MFCM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3920-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO MFCM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3930-LIMIT-COPY-FEE.
           IF MFCM-COPY-FEE-AMT < 25.00
               MOVE 25.00 TO MFCM-COPY-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-COPY-FEE-AMT > 10000.00
               MOVE 10000.00 TO MFCM-COPY-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3940-AGE-EXPIRY.
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
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E450 DESC REQUIRED
      * E829 STORAGE AMT NEGATIVE
      * E977 STORAGE AMT OVER 10000
      * E100 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3950-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-DESC = SPACES
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN MFCT-STORAGE-AMT < 0
                   MOVE 'E829' TO WS-REJECT-CODE
               WHEN MFCT-STORAGE-AMT > 10000.00
                   MOVE 'E977' TO WS-REJECT-CODE
               WHEN MFCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E100' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3960-LIMIT-STORAGE.
           IF MFCM-STORAGE-YTD < 100.00
               MOVE 100.00 TO MFCM-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-STORAGE-YTD > 5000.00
               MOVE 5000.00 TO MFCM-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MFCM-RESEARCH-AMT NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3980-AGE-LAST-MAINT.
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
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR FICHE TYPE CD:
      * AW -> JM WHEN EXPIRY DATE IS PAST
      * JM -> KV WHEN EXPIRY DATE IS PAST
      * KV -> MD WHEN EXPIRY DATE IS PAST
      * MD -> NR WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3990-SET-FICHE-TYPE.
           EVALUATE MFCM-FICHE-TYPE-CD
               WHEN 'AW'
                   IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JM' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JM'
                   IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KV' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KV'
                   IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MD'
                   IF MFCM-EXPIRY-DATE > 0 AND MFCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NR' TO MFCM-FICHE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4000-CHECK-POST.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE STORAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4010-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MFCM-STORAGE-YTD NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4020-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = MFCM-STORAGE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MFCM-STORAGE-AMT - (WS-WORK-AMT * 4)
           COMPUTE MFCM-RESEARCH-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4030-ACCRUE-RESEARCH.
           IF MFCT-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-RESEARCH-AMT * MFCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MFCM-STORAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STORAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE YTD.
      *----------------------------------------------------------------*
       4040-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-STORAGE-YTD ROUNDED =
                   MFCT-STORAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCM-STORAGE-YTD
           END-IF
           ADD MFCM-STORAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-NOTICE.
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
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4060-CHECK-MATURITY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO MFCM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4070-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = MFCM-STORAGE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MFCM-STORAGE-AMT - (WS-WORK-AMT * 12)
           COMPUTE MFCM-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN LAST STMT DATE IS PAST
      * GL -> HH WHEN LAST STMT DATE IS PAST
      * HH -> LM WHEN LAST STMT DATE IS PAST
      * LM -> MD WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4080-SET-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GL' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GL'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HH' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HH'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LM' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LM'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MD' TO MFCM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH YTD FEE ON THE STORAGE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4090-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO MFCM-RESEARCH-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = MFCM-STORAGE-AMT
                       * MFCM-FEE-PCT / 200
               WHEN MFCM-RETENTION-CD = 'KT'
                   MOVE 7.00 TO MFCM-RESEARCH-YTD
               WHEN MFCM-RETENTION-CD = 'SJ'
                   MOVE 10.00 TO MFCM-RESEARCH-YTD
               WHEN OTHER
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = MFCM-STORAGE-AMT
                       * MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCM-RESEARCH-YTD > 0 AND MFCM-RESEARCH-YTD < 10.00
               MOVE 10.00 TO MFCM-RESEARCH-YTD
           END-IF
           IF MFCM-RESEARCH-YTD > 75.00
               MOVE 75.00 TO MFCM-RESEARCH-YTD
           END-IF
           ADD MFCM-RESEARCH-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-PAYMENT.
           IF MFCM-PAYMENT-CNT < 9999999
               ADD 1 TO MFCM-PAYMENT-CNT
           END-IF
           IF MFCM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO MFCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * AT -> KR WHEN LAST STMT DATE IS PAST
      * KR -> LK WHEN LAST STMT DATE IS PAST
      * LK -> US WHEN LAST STMT DATE IS PAST
      * US -> AT WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4110-SET-BRANCH.
           EVALUATE MFCM-BRANCH-CD
               WHEN 'AT'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LK' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LK'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AT' TO MFCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E007 ADDR REQUIRED
      * E590 COPY FEE YTD NEGATIVE
      * E226 COPY FEE YTD OVER 1000000
      * E578 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4120-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-ADDR = SPACES
                   MOVE 'E007' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-YTD < 0
                   MOVE 'E590' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-YTD > 1000000.00
                   MOVE 'E226' TO WS-REJECT-CODE
               WHEN MFCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E578' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4130-LIMIT-STORAGE.
           IF MFCM-STORAGE-YTD < 25.00
               MOVE 25.00 TO MFCM-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-STORAGE-YTD > 500.00
               MOVE 500.00 TO MFCM-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-CLOSE.
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
               WHEN WS-DAYS NOT > 90
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * POSTAGE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4150-LIMIT-POSTAGE.
           IF MFCM-POSTAGE-AMT < 50.00
               MOVE 50.00 TO MFCM-POSTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-POSTAGE-AMT > 10000.00
               MOVE 10000.00 TO MFCM-POSTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       4160-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-POSTAGE-AMT ROUNDED =
                   MFCT-COPY-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4170-CHECK-POST.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO MFCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO POSTAGE YTD.
      *----------------------------------------------------------------*
       4180-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-POSTAGE-YTD ROUNDED =
                   MFCT-RESEARCH-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCM-POSTAGE-YTD
           END-IF
           ADD MFCM-POSTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR RETENTION CD:
      * KT -> SJ WHEN NEXT DUE DATE IS PAST
      * SJ -> WV WHEN NEXT DUE DATE IS PAST
      * WV -> KT WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4190-SET-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SJ' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SJ'
                   IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF MFCM-NEXT-DUE-DATE > 0 AND MFCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STORAGE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4200-FLAG-STORAGE.
           IF MFCM-STORAGE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MFCM-STORAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE STORAGE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4210-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = MFCM-STORAGE-AMT *
                       MFCM-MARGIN / 200
               WHEN MFCT-TRAN-CD = 'IX'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN MFCT-TRAN-CD = 'RS'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = MFCM-STORAGE-AMT *
                       MFCM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE POSTAGE AMT BY FICHE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4220-ACCUM-FICHE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-FICHE-TYPE-CD
                   ADD MFCM-POSTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-FICHE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-POSTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4230-CHECK-LAST-STMT.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4240-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO MFCM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COPY FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4250-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = MFCM-COPY-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MFCM-COPY-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4260-LIMIT-RESEARCH.
           IF MFCM-RESEARCH-AMT < 0.00
               MOVE 0.00 TO MFCM-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-RESEARCH-AMT > 10000.00
               MOVE 10000.00 TO MFCM-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4270-FLAG-COPY-FEE.
           IF MFCM-COPY-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFCM-COPY-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4280-LIMIT-COPY-FEE.
           IF MFCM-COPY-FEE-YTD < 25.00
               MOVE 25.00 TO MFCM-COPY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-COPY-FEE-YTD > 1000.00
               MOVE 1000.00 TO MFCM-COPY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE POSTAGE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4290-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCT-TRAN-CD
                   ADD MFCM-POSTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-POSTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4300-ACCRUE-STORAGE.
           IF MFCT-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-STORAGE-AMT * MFCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MFCM-COPY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE POSTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4310-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MFCM-POSTAGE-YTD NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4320-LIMIT-RESEARCH.
           IF MFCM-RESEARCH-AMT < 0.00
               MOVE 0.00 TO MFCM-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-RESEARCH-AMT > 500.00
               MOVE 500.00 TO MFCM-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * G -> K WHEN LAST PAY DATE IS PAST
      * K -> W WHEN LAST PAY DATE IS PAST
      * W -> Y WHEN LAST PAY DATE IS PAST
      * Y -> G WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4330-SET-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF MFCM-LAST-PAY-DATE > 0 AND MFCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4340-DESCRIBE-CHANNEL.
           EVALUATE MFCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E950 REF NO REQUIRED
      * E164 STORAGE AMT NEGATIVE
      * E364 STORAGE AMT OVER 5000
      * E323 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4350-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-REF-NO = SPACES
                   MOVE 'E950' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-AMT < 0
                   MOVE 'E164' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-AMT > 5000.00
                   MOVE 'E364' TO WS-REJECT-CODE
               WHEN MFCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E323' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE POSTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4360-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MFCM-POSTAGE-AMT NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE COPY FEE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4370-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN MFCM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO MFCT-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE MFCT-STORAGE-AMT ROUNDED = MFCT-COPY-FEE-AMT
                       * MFCM-MARGIN / 200
               WHEN MFCM-RETENTION-CD = 'KT'
                   MOVE 7.00 TO MFCT-STORAGE-AMT
               WHEN MFCM-RETENTION-CD = 'SJ'
                   MOVE 3.00 TO MFCT-STORAGE-AMT
               WHEN OTHER
                   COMPUTE MFCT-STORAGE-AMT ROUNDED = MFCT-COPY-FEE-AMT
                       * MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCT-STORAGE-AMT > 0 AND MFCT-STORAGE-AMT < 1.00
               MOVE 1.00 TO MFCT-STORAGE-AMT
           END-IF
           IF MFCT-STORAGE-AMT > 250.00
               MOVE 250.00 TO MFCT-STORAGE-AMT
           END-IF
           ADD MFCT-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4380-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF MFCM-RESEARCH-AMT NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4390-COUNT-ITEM.
           IF MFCM-ITEM-CNT < 9999999
               ADD 1 TO MFCM-ITEM-CNT
           END-IF
           IF MFCM-ITEM-CNT NOT < 12
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4400-LIMIT-COPY-FEE.
           IF MFCT-COPY-FEE-AMT < 10.00
               MOVE 10.00 TO MFCT-COPY-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-COPY-FEE-AMT > 50000.00
               MOVE 50000.00 TO MFCT-COPY-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4410-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-POSTAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT STORAGE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4420-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = MFCM-STORAGE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = MFCM-STORAGE-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4430-COUNT-CYCLE.
           IF MFCM-CYCLE-CNT < 9999999
               ADD 1 TO MFCM-CYCLE-CNT
           END-IF
           IF MFCM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO MFCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4440-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCT-TRAN-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E132 REJECT CD REQUIRED
      * E610 STORAGE YTD NEGATIVE
      * E318 STORAGE YTD OVER 5000
      * E925 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4450-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-REJECT-CD = SPACES
                   MOVE 'E132' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-YTD < 0
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-YTD > 5000.00
                   MOVE 'E318' TO WS-REJECT-CODE
               WHEN MFCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E925' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4460-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MFCT-RESEARCH-AMT NOT < WS-STORAGE-SCHED-LOW(WS-TX)
                   MOVE WS-STORAGE-SCHED-RATE(WS-TX) TO MFCM-MARGIN
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
       4470-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCM-RESEARCH-AMT / 4
           COMPUTE WS-WORK-AMT-2 = MFCM-RESEARCH-AMT - (WS-WORK-AMT * 4)
           COMPUTE MFCT-RESEARCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COPY FEE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4480-ACCRUE-COPY-FEE.
           IF MFCM-COPY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-COPY-FEE-AMT * MFCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO MFCT-RESEARCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESEARCH YTD.
      *----------------------------------------------------------------*
       4490-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-RESEARCH-YTD ROUNDED =
                   MFCT-COPY-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCM-RESEARCH-YTD
           END-IF
           ADD MFCM-RESEARCH-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4500-CHECK-MATURITY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO MFCM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE SCHED TIER FOR THE POSTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4510-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-SCHED-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
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
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4520-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
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
                   MOVE MFDTE-AUTO-RENEW-FLAG
                     TO MFCM-AUTO-RENEW-FLAG
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
           DISPLAY 'MFCBILL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'MFCBILL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
