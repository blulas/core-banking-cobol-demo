      *================================================================*
      * PROGRAM  : MFCCONV                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MICROFICHE STATEMENTS - FILE CONVERSION. READS THE  *
      *            MFC TRANSACTIONS, UPDATES THE MFC MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. STATEMENT MICROFICHE   *
      *            INDEXING, RETENTION AND RESEARCH FEES               *
      * INPUT    : MFCMST (KSDS), MFCTRN, BANKCTL                      *
      * OUTPUT   : MFCOUT, MFCCORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, MFSTAT, MFDTE, MFLIM    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-03-26 BHS  ORIGINAL                                       *
      * 1996-07-26 JLM  STORAGE RERUN PROTECTION REQ5010               *
      * 1997-04-12 JLM  AUDIT FINDING - POSTAGE CONTROL TOTALS PRB4537 *
      * 1998-05-14 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-09-21 JLM  NEW STORAGE REPORT COLUMNS PRB0481             *
      * 2005-04-24 GAF  COPY-FEE ROUNDING - HALF UP REQ9409            *
      * 2006-04-25 CRP  COPY-FEE CUTOFF MOVED CHG9999                  *
      * 2006-01-18 GAF  POSTAGE CUTOFF MOVED CHG9006                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFCCONV.
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
           SELECT REPORT-FILE ASSIGN TO "MFCCORPT"
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
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 30.
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
                                           VALUE '00000000000000655000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000120500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000310500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000748000'.
       01  WS-STORAGE-TIER-TBL REDEFINES WS-STORAGE-TIER-VALUES.
           05  WS-STORAGE-TIER-ROW OCCURS 4 TIMES.
               10  WS-STORAGE-TIER-LOW     PIC 9(09)V99.
               10  WS-STORAGE-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COPY-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COPY-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000024000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000789500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000491500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005200000000381000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007700000000375000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008200000000277000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008250000000869500'.
       01  WS-COPY-FEE-TIER-TBL REDEFINES WS-COPY-FEE-TIER-VALUES.
           05  WS-COPY-FEE-TIER-ROW OCCURS 7 TIMES.
               10  WS-COPY-FEE-TIER-LOW    PIC 9(09)V99.
               10  WS-COPY-FEE-TIER-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  MFCCONV  MICROFICHE STATEMENTS'.
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
       COPY MFSTATP.
       COPY MFDTEP.
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
               PERFORM 3000-LIMIT-RESEARCH
               PERFORM 3010-CHECK-POST
               PERFORM 3020-CHECK-CLOSE
               PERFORM 3030-AGE-CLOSE
               PERFORM 3040-COUNT-NOTICE
               PERFORM 3050-LIMIT-POSTAGE
               PERFORM 3060-PRORATE-STORAGE
               PERFORM 3070-ASSESS-EXEMPT
               PERFORM 3080-ACCRUE-STORAGE
               PERFORM 3090-AGE-NOTICE
               PERFORM 3100-COUNT-NOTICE
               PERFORM 3110-AGE-LAST-MAINT
               PERFORM 3120-SET-RETENTION
               PERFORM 3130-AGE-CLOSE
               PERFORM 3140-SELECT-MARGIN
               PERFORM 3150-ACCUM-STATUS
               PERFORM 3160-ACCRUE-POSTAGE
               PERFORM 3170-ACCRUE-STORAGE
               PERFORM 3180-COUNT-ITEM
               PERFORM 3190-DESCRIBE-CHANNEL
               PERFORM 3200-FLAG-COPY-FEE
               PERFORM 3210-SET-CHANNEL
               PERFORM 3220-COUNT-PAYMENT
               PERFORM 3230-COUNT-PAYMENT
               PERFORM 3240-AGE-LAST-STMT
               PERFORM 3250-COUNT-CYCLE
               PERFORM 3260-COUNT-SEQ
               PERFORM 3270-LIMIT-RESEARCH
               PERFORM 3280-ASSESS-REVERSAL
               PERFORM 3290-ASSESS-REVERSAL
               PERFORM 3300-SELECT-DISCOUNT
               PERFORM 3310-LIMIT-RESEARCH
               PERFORM 3320-ASSESS-EXEMPT
               PERFORM 3330-DESCRIBE-TYPE
               PERFORM 3340-PRORATE-COPY-FEE
               PERFORM 3350-SELECT-DISCOUNT
               PERFORM 3360-ACCRUE-COPY-FEE
               PERFORM 3370-FLAG-RESEARCH
               PERFORM 3380-LIMIT-POSTAGE
               PERFORM 3390-SPLIT-POSTAGE
               PERFORM 3400-ACCUM-BRANCH
               PERFORM 3410-CHECK-CLOSE
               PERFORM 3420-EDIT-REF-NO
               PERFORM 3430-EDIT-DESC
               PERFORM 3440-CHECK-POST
               PERFORM 3450-SELECT-DISCOUNT
               PERFORM 3460-LIMIT-POSTAGE
               PERFORM 3470-COUNT-PAYMENT
               PERFORM 3480-ACCUM-CHANNEL
               PERFORM 3490-ACCRUE-POSTAGE
               PERFORM 3500-SPLIT-COPY-FEE
               PERFORM 3510-DESCRIBE-TRAN
               PERFORM 3520-ACCRUE-COPY-FEE
               PERFORM 3530-LIMIT-COPY-FEE
               PERFORM 3540-CHECK-NEXT-DUE
               PERFORM 3550-AGE-MATURITY
               PERFORM 3560-COUNT-SEQ
               PERFORM 3570-PRORATE-COPY-FEE
               PERFORM 3580-CHECK-EFF
               PERFORM 3590-COUNT-CYCLE
               PERFORM 3600-AGE-LAST-PAY
               PERFORM 3610-CHECK-NEXT-DUE
               PERFORM 3620-PRORATE-RESEARCH
               PERFORM 3630-COUNT-CYCLE
               PERFORM 3640-ASSESS-REVIEW
               PERFORM 3650-FLAG-POSTAGE
               PERFORM 3660-SPLIT-RESEARCH
               PERFORM 3670-ASSESS-AUTO-RENEW
               PERFORM 3680-SET-RETENTION
               PERFORM 3690-AGE-NOTICE
               PERFORM 3700-DESCRIBE-BRANCH
               PERFORM 3710-CHECK-EXPIRY
               PERFORM 3720-EDIT-CUST-ID
               PERFORM 3730-AGE-NEXT-DUE
               PERFORM 3740-COUNT-NOTICE
               PERFORM 3750-PRORATE-POSTAGE
               PERFORM 3760-EDIT-CONTACT
               PERFORM 3770-DESCRIBE-FICHE-TYPE
               PERFORM 3780-ACCRUE-RESEARCH
               PERFORM 3790-ACCRUE-POSTAGE
               PERFORM 3800-LIMIT-STORAGE
               PERFORM 3810-SET-STATUS
               PERFORM 3820-AGE-OPEN
               PERFORM 3830-ASSESS-EXEMPT
               PERFORM 3840-CHECK-CLOSE
               PERFORM 3850-ACCUM-TRAN
               PERFORM 3860-SELECT-MARGIN
               PERFORM 3870-DESCRIBE-STATUS
               PERFORM 3880-FLAG-STORAGE
               PERFORM 3890-SELECT-FEE-PCT
               PERFORM 3900-AGE-LAST-STMT
               PERFORM 3910-SPLIT-POSTAGE
               PERFORM 3920-LIMIT-STORAGE
               PERFORM 3930-COUNT-ITEM
               PERFORM 3940-PRORATE-COPY-FEE
               PERFORM 3950-EDIT-CUST-ID
               PERFORM 3960-CHECK-LAST-MAINT
               PERFORM 3970-SPLIT-STORAGE
               PERFORM 3980-ACCRUE-FEE-PCT
               PERFORM 3990-SPLIT-RESEARCH
               PERFORM 4000-EDIT-NAME
               PERFORM 4010-EDIT-CONTACT
               PERFORM 4020-FLAG-COPY-FEE
               PERFORM 4030-LIMIT-STORAGE
               PERFORM 4040-ASSESS-AUTO-RENEW
               PERFORM 4050-SET-TYPE
               PERFORM 4060-AGE-OPEN
               PERFORM 4070-DESCRIBE-STATUS
               PERFORM 4080-ACCRUE-POSTAGE
               PERFORM 4090-LIMIT-POSTAGE
               PERFORM 4100-LIMIT-POSTAGE
               PERFORM 4110-EDIT-MEMO
               PERFORM 4120-PRORATE-COPY-FEE
               PERFORM 4130-LIMIT-ITEM
               PERFORM 4140-COUNT-ITEM
               PERFORM 4150-DESCRIBE-STATUS
               PERFORM 4160-EDIT-REF-NO
               PERFORM 4170-PRORATE-POSTAGE
               PERFORM 4180-DESCRIBE-TRAN
               PERFORM 4190-ACCRUE-RESEARCH
               PERFORM 4200-EDIT-CONTACT
               PERFORM 4210-SET-TYPE
               PERFORM 4220-CHECK-LAST-PAY
               PERFORM 4230-AGE-LAST-STMT
               PERFORM 4240-DESCRIBE-STATUS
               PERFORM 4250-ACCUM-CHANNEL
               PERFORM 4260-PRORATE-STORAGE
               PERFORM 4270-EDIT-ADDR
               PERFORM 4280-SELECT-MARGIN
               PERFORM 4290-ACCUM-CHANNEL
               PERFORM 4300-SELECT-MARGIN
               PERFORM 4310-DESCRIBE-STATUS
               PERFORM 4320-SET-TYPE
               PERFORM 4330-EDIT-NAME
               PERFORM 4340-FLAG-ITEM
               PERFORM 7000-CALL-MFSTAT
               PERFORM 7020-CALL-MFDTE
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
      * RESEARCH AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3000-LIMIT-RESEARCH.
           IF MFCT-RESEARCH-AMT < 100.00
               MOVE 100.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-RESEARCH-AMT > 1000.00
               MOVE 1000.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3010-CHECK-POST.
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
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3020-CHECK-CLOSE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO MFCM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-CLOSE.
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
                   ADD MFCM-RESEARCH-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFCM-RESEARCH-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MFCM-RESEARCH-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD MFCM-RESEARCH-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-RESEARCH-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO MFCM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3050-LIMIT-POSTAGE.
           IF MFCT-POSTAGE-AMT < 25.00
               MOVE 25.00 TO MFCT-POSTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-POSTAGE-AMT > 500.00
               MOVE 500.00 TO MFCT-POSTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3060-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-STORAGE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-STORAGE-AMT
           END-IF
           ADD MFCT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE FEE ON THE RESEARCH YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3070-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MFCT-POSTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE MFCT-POSTAGE-AMT ROUNDED = MFCM-RESEARCH-YTD
                       * MFCM-FEE-PCT / 200
               WHEN MFCT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO MFCT-POSTAGE-AMT
               WHEN MFCT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO MFCT-POSTAGE-AMT
               WHEN OTHER
                   COMPUTE MFCT-POSTAGE-AMT ROUNDED = MFCM-RESEARCH-YTD
                       * MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCT-POSTAGE-AMT > 0 AND MFCT-POSTAGE-AMT < 10.00
               MOVE 10.00 TO MFCT-POSTAGE-AMT
           END-IF
           IF MFCT-POSTAGE-AMT > 100.00
               MOVE 100.00 TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-STORAGE.
           IF MFCM-STORAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-STORAGE-YTD * MFCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MFCT-POSTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3090-AGE-NOTICE.
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
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO MFCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3110-AGE-LAST-MAINT.
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
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR RETENTION CD:
      * KT -> SJ WHEN OPEN DATE IS PAST
      * SJ -> WV WHEN OPEN DATE IS PAST
      * WV -> KT WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-RETENTION.
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
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-CLOSE.
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
               WHEN WS-DAYS NOT > 90
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-STORAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3140-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MFCT-RESEARCH-AMT NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3150-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-STATUS-CD
                   ADD MFCT-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCT-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-POSTAGE.
           IF MFCT-POSTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-POSTAGE-AMT * MFCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MFCT-RESEARCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3170-ACCRUE-STORAGE.
           IF MFCM-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-STORAGE-AMT * MFCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3180-COUNT-ITEM.
           IF MFCM-ITEM-CNT < 9999999
               ADD 1 TO MFCM-ITEM-CNT
           END-IF
           IF MFCM-ITEM-CNT NOT < 10
               MOVE 'Y' TO MFCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ND'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COPY FEE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3200-FLAG-COPY-FEE.
           IF MFCM-COPY-FEE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCM-COPY-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3210-SET-CHANNEL.
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
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3220-COUNT-PAYMENT.
           IF MFCM-PAYMENT-CNT < 9999999
               ADD 1 TO MFCM-PAYMENT-CNT
           END-IF
           IF MFCM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3230-COUNT-PAYMENT.
           IF MFCM-PAYMENT-CNT < 9999999
               ADD 1 TO MFCM-PAYMENT-CNT
           END-IF
           IF MFCM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO MFCM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3240-AGE-LAST-STMT.
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
               WHEN WS-DAYS NOT > 90
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3250-COUNT-CYCLE.
           IF MFCM-CYCLE-CNT < 9999999
               ADD 1 TO MFCM-CYCLE-CNT
           END-IF
           IF MFCM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO MFCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3260-COUNT-SEQ.
           IF MFCT-SEQ < 9999999
               ADD 1 TO MFCT-SEQ
           END-IF
           IF MFCT-SEQ NOT < 12
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3270-LIMIT-RESEARCH.
           IF MFCM-RESEARCH-AMT < 100.00
               MOVE 100.00 TO MFCM-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-RESEARCH-AMT > 2500.00
               MOVE 2500.00 TO MFCM-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH FEE ON THE RESEARCH YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3280-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO MFCM-RESEARCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE MFCM-RESEARCH-AMT ROUNDED = MFCM-RESEARCH-YTD
                       * MFCM-FEE-PCT / 200
               WHEN MFCT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO MFCM-RESEARCH-AMT
               WHEN MFCT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO MFCM-RESEARCH-AMT
               WHEN OTHER
                   COMPUTE MFCM-RESEARCH-AMT ROUNDED = MFCM-RESEARCH-YTD
                       * MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCM-RESEARCH-AMT > 0 AND MFCM-RESEARCH-AMT < 10.00
               MOVE 10.00 TO MFCM-RESEARCH-AMT
           END-IF
           IF MFCM-RESEARCH-AMT > 250.00
               MOVE 250.00 TO MFCM-RESEARCH-AMT
           END-IF
           ADD MFCM-RESEARCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE YTD FEE ON THE STORAGE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3290-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO MFCM-POSTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-STOP-FLAG = 'Y'
                   COMPUTE MFCM-POSTAGE-YTD ROUNDED = MFCT-STORAGE-AMT *
                       MFCM-MARGIN / 200
               WHEN MFCM-STATUS-CD = 'AST'
                   MOVE 15.00 TO MFCM-POSTAGE-YTD
               WHEN MFCM-STATUS-CD = 'JXN'
                   MOVE 20.00 TO MFCM-POSTAGE-YTD
               WHEN OTHER
                   COMPUTE MFCM-POSTAGE-YTD ROUNDED = MFCT-STORAGE-AMT *
                       MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCM-POSTAGE-YTD > 0 AND MFCM-POSTAGE-YTD < 1.00
               MOVE 1.00 TO MFCM-POSTAGE-YTD
           END-IF
           IF MFCM-POSTAGE-YTD > 50.00
               MOVE 50.00 TO MFCM-POSTAGE-YTD
           END-IF
           ADD MFCM-POSTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE TIER TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3300-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-TIER-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
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
      * RESEARCH AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3310-LIMIT-RESEARCH.
           IF MFCT-RESEARCH-AMT < 0.00
               MOVE 0.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-RESEARCH-AMT > 1000.00
               MOVE 1000.00 TO MFCT-RESEARCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE FEE ON THE STORAGE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3320-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MFCM-POSTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   COMPUTE MFCM-POSTAGE-AMT ROUNDED = MFCT-STORAGE-AMT *
                       MFCM-FEE-PCT / 200
               WHEN MFCM-BRANCH-CD = 'AT'
                   MOVE 10.00 TO MFCM-POSTAGE-AMT
               WHEN MFCM-BRANCH-CD = 'KR'
                   MOVE 10.00 TO MFCM-POSTAGE-AMT
               WHEN OTHER
                   COMPUTE MFCM-POSTAGE-AMT ROUNDED = MFCT-STORAGE-AMT *
                       MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCM-POSTAGE-AMT > 0 AND MFCM-POSTAGE-AMT < 2.00
               MOVE 2.00 TO MFCM-POSTAGE-AMT
           END-IF
           IF MFCM-POSTAGE-AMT > 100.00
               MOVE 100.00 TO MFCM-POSTAGE-AMT
           END-IF
           ADD MFCM-POSTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3330-DESCRIBE-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'K'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COPY FEE AMT.
      *----------------------------------------------------------------*
       3340-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-COPY-FEE-AMT ROUNDED =
                   MFCM-POSTAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCT-COPY-FEE-AMT
           END-IF
           ADD MFCT-COPY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE POSTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MFCM-POSTAGE-AMT NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COPY FEE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3360-ACCRUE-COPY-FEE.
           IF MFCM-COPY-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-COPY-FEE-YTD * MFCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MFCM-STORAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3370-FLAG-RESEARCH.
           IF MFCT-RESEARCH-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCT-RESEARCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3380-LIMIT-POSTAGE.
           IF MFCM-POSTAGE-YTD < 10.00
               MOVE 10.00 TO MFCM-POSTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-POSTAGE-YTD > 5000.00
               MOVE 5000.00 TO MFCM-POSTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3390-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFCM-POSTAGE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MFCM-POSTAGE-AMT - (WS-WORK-AMT * 12)
           COMPUTE MFCM-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STORAGE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3400-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-BRANCH-CD
                   ADD MFCT-STORAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCT-STORAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3410-CHECK-CLOSE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO MFCM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E165 REF NO REQUIRED
      * E241 COPY FEE YTD NEGATIVE
      * E209 COPY FEE YTD OVER 10000
      * E241 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-REF-NO = SPACES
                   MOVE 'E165' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-YTD < 0
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-YTD > 10000.00
                   MOVE 'E209' TO WS-REJECT-CODE
               WHEN MFCM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E774 DESC REQUIRED
      * E893 STORAGE AMT NEGATIVE
      * E238 STORAGE AMT OVER 99999
      * E776 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3430-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-DESC = SPACES
                   MOVE 'E774' TO WS-REJECT-CODE
               WHEN MFCT-STORAGE-AMT < 0
                   MOVE 'E893' TO WS-REJECT-CODE
               WHEN MFCT-STORAGE-AMT > 99999.00
                   MOVE 'E238' TO WS-REJECT-CODE
               WHEN MFCM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E776' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-POST.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO MFCM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE TIER TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-TIER-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF MFCM-RESEARCH-AMT NOT < WS-COPY-FEE-TIER-LOW(WS-TX)
                   MOVE WS-COPY-FEE-TIER-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3460-LIMIT-POSTAGE.
           IF MFCM-POSTAGE-YTD < 50.00
               MOVE 50.00 TO MFCM-POSTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-POSTAGE-YTD > 500.00
               MOVE 500.00 TO MFCM-POSTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-PAYMENT.
           IF MFCM-PAYMENT-CNT < 9999999
               ADD 1 TO MFCM-PAYMENT-CNT
           END-IF
           IF MFCM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COPY FEE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3480-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-CHANNEL-CD
                   ADD MFCM-COPY-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-COPY-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-POSTAGE.
           IF MFCT-POSTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-POSTAGE-AMT * MFCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MFCT-COPY-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3500-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE MFCM-COPY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3510-DESCRIBE-TRAN.
           EVALUATE MFCT-TRAN-CD
               WHEN 'IX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PG'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COPY FEE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3520-ACCRUE-COPY-FEE.
           IF MFCM-COPY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-COPY-FEE-AMT * MFCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3530-LIMIT-COPY-FEE.
           IF MFCM-COPY-FEE-YTD < 10.00
               MOVE 10.00 TO MFCM-COPY-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-COPY-FEE-YTD > 50000.00
               MOVE 50000.00 TO MFCM-COPY-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3540-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-MATURITY.
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
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3560-COUNT-SEQ.
           IF MFCT-SEQ < 9999999
               ADD 1 TO MFCT-SEQ
           END-IF
           IF MFCT-SEQ NOT < 6
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COPY FEE AMT.
      *----------------------------------------------------------------*
       3570-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-COPY-FEE-AMT ROUNDED =
                   MFCM-RESEARCH-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCT-COPY-FEE-AMT
           END-IF
           ADD MFCT-COPY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3580-CHECK-EFF.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-CYCLE.
           IF MFCM-CYCLE-CNT < 9999999
               ADD 1 TO MFCM-CYCLE-CNT
           END-IF
           IF MFCM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-LAST-PAY.
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
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-COPY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3610-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO MFCM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3620-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MFCM-RESEARCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-CYCLE.
           IF MFCM-CYCLE-CNT < 9999999
               ADD 1 TO MFCM-CYCLE-CNT
           END-IF
           IF MFCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO MFCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE RESEARCH AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3640-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN MFCM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO MFCM-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE MFCM-STORAGE-AMT ROUNDED = MFCT-RESEARCH-AMT
                       * MFCM-DISCOUNT / 200
               WHEN MFCT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO MFCM-STORAGE-AMT
               WHEN MFCT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO MFCM-STORAGE-AMT
               WHEN OTHER
                   COMPUTE MFCM-STORAGE-AMT ROUNDED = MFCT-RESEARCH-AMT
                       * MFCM-DISCOUNT / 100
           END-EVALUATE
           IF MFCM-STORAGE-AMT > 0 AND MFCM-STORAGE-AMT < 5.00
               MOVE 5.00 TO MFCM-STORAGE-AMT
           END-IF
           IF MFCM-STORAGE-AMT > 75.00
               MOVE 75.00 TO MFCM-STORAGE-AMT
           END-IF
           ADD MFCM-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * POSTAGE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3650-FLAG-POSTAGE.
           IF MFCT-POSTAGE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MFCT-POSTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCT-RESEARCH-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MFCT-RESEARCH-AMT - (WS-WORK-AMT * 3)
           COMPUTE MFCT-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COPY FEE FEE ON THE RESEARCH YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3670-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MFCM-COPY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-EXEMPT-FLAG = 'Y'
                   COMPUTE MFCM-COPY-FEE-AMT ROUNDED = MFCM-RESEARCH-YTD
                       * MFCM-FEE-PCT / 200
               WHEN MFCM-BRANCH-CD = 'AT'
                   MOVE 3.00 TO MFCM-COPY-FEE-AMT
               WHEN MFCM-BRANCH-CD = 'KR'
                   MOVE 12.00 TO MFCM-COPY-FEE-AMT
               WHEN OTHER
                   COMPUTE MFCM-COPY-FEE-AMT ROUNDED = MFCM-RESEARCH-YTD
                       * MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCM-COPY-FEE-AMT > 0 AND MFCM-COPY-FEE-AMT < 1.00
               MOVE 1.00 TO MFCM-COPY-FEE-AMT
           END-IF
           IF MFCM-COPY-FEE-AMT > 150.00
               MOVE 150.00 TO MFCM-COPY-FEE-AMT
           END-IF
           ADD MFCM-COPY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR RETENTION CD:
      * KT -> SJ WHEN MATURITY DATE IS PAST
      * SJ -> WV WHEN MATURITY DATE IS PAST
      * WV -> KT WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3680-SET-RETENTION.
           EVALUATE MFCM-RETENTION-CD
               WHEN 'KT'
                   IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SJ' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SJ'
                   IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KT' TO MFCM-RETENTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-NOTICE.
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
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3700-DESCRIBE-BRANCH.
           EVALUATE MFCM-BRANCH-CD
               WHEN 'AT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'US'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3710-CHECK-EXPIRY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E176 CUST ID REQUIRED
      * E179 STORAGE YTD NEGATIVE
      * E911 STORAGE YTD OVER 25000
      * E119 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CUST-ID = SPACES
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-YTD < 0
                   MOVE 'E179' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-YTD > 25000.00
                   MOVE 'E911' TO WS-REJECT-CODE
               WHEN MFCM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E119' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3730-AGE-NEXT-DUE.
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
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO MFCM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       3750-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-POSTAGE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCM-POSTAGE-AMT
           END-IF
           ADD MFCM-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E234 CONTACT REQUIRED
      * E739 COPY FEE AMT NEGATIVE
      * E382 COPY FEE AMT OVER 99999
      * E454 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3760-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CONTACT = SPACES
                   MOVE 'E234' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-AMT < 0
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-AMT > 99999.00
                   MOVE 'E382' TO WS-REJECT-CODE
               WHEN MFCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E454' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE FICHE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-FICHE-TYPE.
           EVALUATE MFCM-FICHE-TYPE-CD
               WHEN 'AW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3780-ACCRUE-RESEARCH.
           IF MFCM-RESEARCH-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-RESEARCH-YTD * MFCM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3790-ACCRUE-POSTAGE.
           IF MFCM-POSTAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-POSTAGE-YTD * MFCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MFCM-COPY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3800-LIMIT-STORAGE.
           IF MFCM-STORAGE-YTD < 10.00
               MOVE 10.00 TO MFCM-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-STORAGE-YTD > 2500.00
               MOVE 2500.00 TO MFCM-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * AST -> JXN WHEN NEXT DUE DATE IS PAST
      * JXN -> KJJ WHEN NEXT DUE DATE IS PAST
      * KJJ -> TZL WHEN NEXT DUE DATE IS PAST
      * TZL -> XZM WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-STATUS.
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
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3820-AGE-OPEN.
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
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-POSTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE FEE ON THE RESEARCH AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3830-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MFCM-POSTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE MFCM-POSTAGE-AMT ROUNDED = MFCT-RESEARCH-AMT
                       * MFCM-MARGIN / 200
               WHEN MFCM-STATUS-CD = 'AST'
                   MOVE 12.00 TO MFCM-POSTAGE-AMT
               WHEN MFCM-STATUS-CD = 'JXN'
                   MOVE 3.00 TO MFCM-POSTAGE-AMT
               WHEN OTHER
                   COMPUTE MFCM-POSTAGE-AMT ROUNDED = MFCT-RESEARCH-AMT
                       * MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCM-POSTAGE-AMT > 0 AND MFCM-POSTAGE-AMT < 10.00
               MOVE 10.00 TO MFCM-POSTAGE-AMT
           END-IF
           IF MFCM-POSTAGE-AMT > 25.00
               MOVE 25.00 TO MFCM-POSTAGE-AMT
           END-IF
           ADD MFCM-POSTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3840-CHECK-CLOSE.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE POSTAGE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3850-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCT-TRAN-CD
                   ADD MFCT-POSTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCT-POSTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3860-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JXN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KJJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TZL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XZM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STORAGE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3880-FLAG-STORAGE.
           IF MFCM-STORAGE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCM-STORAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE TIER TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3890-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MFCM-RESEARCH-AMT NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3900-AGE-LAST-STMT.
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
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3910-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFCM-POSTAGE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = MFCM-POSTAGE-YTD - (WS-WORK-AMT * 2)
           COMPUTE MFCM-RESEARCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3920-LIMIT-STORAGE.
           IF MFCM-STORAGE-YTD < 100.00
               MOVE 100.00 TO MFCM-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-STORAGE-YTD > 2500.00
               MOVE 2500.00 TO MFCM-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3930-COUNT-ITEM.
           IF MFCM-ITEM-CNT < 9999999
               ADD 1 TO MFCM-ITEM-CNT
           END-IF
           IF MFCM-ITEM-CNT NOT < 3
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COPY FEE YTD.
      *----------------------------------------------------------------*
       3940-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-COPY-FEE-YTD ROUNDED =
                   MFCT-COPY-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCM-COPY-FEE-YTD
           END-IF
           ADD MFCM-COPY-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E542 CUST ID REQUIRED
      * E919 RESEARCH AMT NEGATIVE
      * E187 RESEARCH AMT OVER 50000
      * E159 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3950-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CUST-ID = SPACES
                   MOVE 'E542' TO WS-REJECT-CODE
               WHEN MFCT-RESEARCH-AMT < 0
                   MOVE 'E919' TO WS-REJECT-CODE
               WHEN MFCT-RESEARCH-AMT > 50000.00
                   MOVE 'E187' TO WS-REJECT-CODE
               WHEN MFCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E159' TO WS-REJECT-CODE
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
       3960-CHECK-LAST-MAINT.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO MFCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3970-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = MFCM-STORAGE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MFCM-STORAGE-AMT - (WS-WORK-AMT * 12)
           COMPUTE MFCM-POSTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3980-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * MFCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO MFCT-POSTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCT-RESEARCH-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MFCT-RESEARCH-AMT - (WS-WORK-AMT *
               12)
           COMPUTE MFCT-COPY-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E189 NAME REQUIRED
      * E185 WORK AMT NEGATIVE
      * E676 WORK AMT OVER 250000
      * E155 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4000-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-NAME = SPACES
                   MOVE 'E189' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E185' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E676' TO WS-REJECT-CODE
               WHEN MFCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E155' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E773 CONTACT REQUIRED
      * E446 RESEARCH YTD NEGATIVE
      * E698 RESEARCH YTD OVER 250000
      * E781 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CONTACT = SPACES
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-YTD < 0
                   MOVE 'E446' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-YTD > 250000.00
                   MOVE 'E698' TO WS-REJECT-CODE
               WHEN MFCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E781' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4020-FLAG-COPY-FEE.
           IF MFCT-COPY-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCT-COPY-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STORAGE YTD IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4030-LIMIT-STORAGE.
           IF MFCM-STORAGE-YTD < 10.00
               MOVE 10.00 TO MFCM-STORAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-STORAGE-YTD > 500.00
               MOVE 500.00 TO MFCM-STORAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COPY FEE FEE ON THE RESEARCH AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4040-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MFCT-COPY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE MFCT-COPY-FEE-AMT ROUNDED = MFCT-RESEARCH-AMT
                       * MFCM-MARGIN / 200
               WHEN MFCM-STATUS-CD = 'AST'
                   MOVE 15.00 TO MFCT-COPY-FEE-AMT
               WHEN MFCM-STATUS-CD = 'JXN'
                   MOVE 7.00 TO MFCT-COPY-FEE-AMT
               WHEN OTHER
                   COMPUTE MFCT-COPY-FEE-AMT ROUNDED = MFCT-RESEARCH-AMT
                       * MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCT-COPY-FEE-AMT > 0 AND MFCT-COPY-FEE-AMT < 5.00
               MOVE 5.00 TO MFCT-COPY-FEE-AMT
           END-IF
           IF MFCT-COPY-FEE-AMT > 35.00
               MOVE 35.00 TO MFCT-COPY-FEE-AMT
           END-IF
           ADD MFCT-COPY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * G -> K WHEN OPEN DATE IS PAST
      * K -> W WHEN OPEN DATE IS PAST
      * W -> Y WHEN OPEN DATE IS PAST
      * Y -> G WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4050-SET-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF MFCM-OPEN-DATE > 0 AND MFCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-OPEN.
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
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JXN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KJJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TZL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XZM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4080-ACCRUE-POSTAGE.
           IF MFCT-POSTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-POSTAGE-AMT * MFCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MFCM-POSTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4090-LIMIT-POSTAGE.
           IF MFCM-POSTAGE-YTD < 50.00
               MOVE 50.00 TO MFCM-POSTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-POSTAGE-YTD > 50000.00
               MOVE 50000.00 TO MFCM-POSTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4100-LIMIT-POSTAGE.
           IF MFCM-POSTAGE-AMT < 50.00
               MOVE 50.00 TO MFCM-POSTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-POSTAGE-AMT > 500.00
               MOVE 500.00 TO MFCM-POSTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E203 MEMO REQUIRED
      * E035 POSTAGE AMT NEGATIVE
      * E064 POSTAGE AMT OVER 1000000
      * E658 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4110-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-MEMO = SPACES
                   MOVE 'E203' TO WS-REJECT-CODE
               WHEN MFCM-POSTAGE-AMT < 0
                   MOVE 'E035' TO WS-REJECT-CODE
               WHEN MFCM-POSTAGE-AMT > 1000000.00
                   MOVE 'E064' TO WS-REJECT-CODE
               WHEN MFCM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E658' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4120-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-COPY-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4130-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4140-COUNT-ITEM.
           IF MFCM-ITEM-CNT < 9999999
               ADD 1 TO MFCM-ITEM-CNT
           END-IF
           IF MFCM-ITEM-CNT NOT < 6
               MOVE 'Y' TO MFCM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4150-DESCRIBE-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JXN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KJJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TZL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XZM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E736 REF NO REQUIRED
      * E208 COPY FEE AMT NEGATIVE
      * E460 COPY FEE AMT OVER 25000
      * E852 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4160-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-REF-NO = SPACES
                   MOVE 'E736' TO WS-REJECT-CODE
               WHEN MFCT-COPY-FEE-AMT < 0
                   MOVE 'E208' TO WS-REJECT-CODE
               WHEN MFCT-COPY-FEE-AMT > 25000.00
                   MOVE 'E460' TO WS-REJECT-CODE
               WHEN MFCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO POSTAGE AMT.
      *----------------------------------------------------------------*
       4170-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-POSTAGE-AMT ROUNDED =
                   MFCT-RESEARCH-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4180-DESCRIBE-TRAN.
           EVALUATE MFCT-TRAN-CD
               WHEN 'IX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'CP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4190-ACCRUE-RESEARCH.
           IF MFCT-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-RESEARCH-AMT * MFCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MFCT-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E921 CONTACT REQUIRED
      * E458 POSTAGE AMT NEGATIVE
      * E843 POSTAGE AMT OVER 1000000
      * E036 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CONTACT = SPACES
                   MOVE 'E921' TO WS-REJECT-CODE
               WHEN MFCM-POSTAGE-AMT < 0
                   MOVE 'E458' TO WS-REJECT-CODE
               WHEN MFCM-POSTAGE-AMT > 1000000.00
                   MOVE 'E843' TO WS-REJECT-CODE
               WHEN MFCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E036' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * G -> K WHEN NOTICE DATE IS PAST
      * K -> W WHEN NOTICE DATE IS PAST
      * W -> Y WHEN NOTICE DATE IS PAST
      * Y -> G WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-TYPE.
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
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4220-CHECK-LAST-PAY.
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
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4230-AGE-LAST-STMT.
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
                   ADD MFCM-POSTAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD MFCM-POSTAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD MFCM-POSTAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCM-POSTAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-POSTAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4240-DESCRIBE-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JXN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KJJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TZL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XZM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE POSTAGE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4250-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-CHANNEL-CD
                   ADD MFCM-POSTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCM-POSTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       4260-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-STORAGE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCT-STORAGE-AMT
           END-IF
           ADD MFCT-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E447 ADDR REQUIRED
      * E932 RESEARCH YTD NEGATIVE
      * E276 RESEARCH YTD OVER 1000000
      * E398 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4270-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-ADDR = SPACES
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-YTD < 0
                   MOVE 'E932' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-YTD > 1000000.00
                   MOVE 'E276' TO WS-REJECT-CODE
               WHEN MFCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E398' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE TIER TIER FOR THE STORAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4280-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-TIER-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF MFCM-STORAGE-YTD NOT < WS-COPY-FEE-TIER-LOW(WS-TX)
                   MOVE WS-COPY-FEE-TIER-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4290-ACCUM-CHANNEL.
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
      * SELECT THE STORAGE TIER TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4300-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-TIER-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF MFCT-RESEARCH-AMT NOT < WS-STORAGE-TIER-LOW(WS-TX)
                   MOVE WS-STORAGE-TIER-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4310-DESCRIBE-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JXN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KJJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TZL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XZM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * G -> K WHEN LAST STMT DATE IS PAST
      * K -> W WHEN LAST STMT DATE IS PAST
      * W -> Y WHEN LAST STMT DATE IS PAST
      * Y -> G WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4320-SET-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF MFCM-LAST-STMT-DATE > 0 AND MFCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E315 NAME REQUIRED
      * E155 WORK AMT NEGATIVE
      * E664 WORK AMT OVER 5000
      * E929 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4330-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-NAME = SPACES
                   MOVE 'E315' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E155' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 5000.00
                   MOVE 'E664' TO WS-REJECT-CODE
               WHEN MFCM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E929' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4340-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE MFC FIELDS TO MFSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-MFSTAT.
           INITIALIZE MFSTAT-PARMS
           MOVE MFCM-COPY-FEE-YTD
             TO MFSTAT-COPY-FEE-YTD
           MOVE MFCM-STORAGE-YTD
             TO MFSTAT-STORAGE-YTD
           MOVE MFCM-RESEARCH-YTD
             TO MFSTAT-RESEARCH-YTD
           MOVE MFCM-STORAGE-AMT
             TO MFSTAT-STORAGE-AMT
           MOVE MFCM-POSTAGE-AMT
             TO MFSTAT-POSTAGE-AMT
           MOVE MFCM-POSTAGE-YTD
             TO MFSTAT-POSTAGE-YTD
           MOVE MFCM-CLOSE-DATE
             TO MFSTAT-CLOSE-DATE
           MOVE MFCM-LAST-PAY-DATE
             TO MFSTAT-LAST-PAY-DATE
           MOVE MFCM-OPEN-DATE
             TO MFSTAT-OPEN-DATE
           MOVE MFCM-NEXT-DUE-DATE
             TO MFSTAT-NEXT-DUE-DATE
           MOVE MFCM-RETENTION-CD
             TO MFSTAT-RETENTION-CD
           MOVE MFCM-CHANNEL-CD
             TO MFSTAT-CHANNEL-CD
           MOVE MFCM-STATUS-CD
             TO MFSTAT-STATUS-CD
           MOVE MFCM-BRANCH-CD
             TO MFSTAT-BRANCH-CD
           MOVE MFCM-AUTO-RENEW-FLAG
             TO MFSTAT-AUTO-RENEW-FLAG
           MOVE MFCM-NOTICE-SENT-FLAG
             TO MFSTAT-NOTICE-SENT-FLAG
           MOVE MFCM-EXEMPT-FLAG
             TO MFSTAT-EXEMPT-FLAG
           MOVE MFCM-CYCLE-CNT
             TO MFSTAT-CYCLE-CNT
           MOVE MFCM-ITEM-CNT
             TO MFSTAT-ITEM-CNT
           MOVE MFCM-MARGIN
             TO MFSTAT-MARGIN
           MOVE MFCM-DISCOUNT
             TO MFSTAT-DISCOUNT
           MOVE MFCM-CONTACT
             TO MFSTAT-CONTACT
           MOVE MFCM-REF-NO
             TO MFSTAT-REF-NO
           CALL 'MFSTAT' USING MFSTAT-PARMS
           EVALUATE MFSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE MFSTAT-STORAGE-AMT
                     TO MFCM-STORAGE-AMT
                   MOVE MFSTAT-POSTAGE-AMT
                     TO MFCM-POSTAGE-AMT
                   MOVE MFSTAT-AUTO-RENEW-FLAG
                     TO MFCM-AUTO-RENEW-FLAG
                   MOVE MFSTAT-NOTICE-SENT-FLAG
                     TO MFCM-NOTICE-SENT-FLAG
                   MOVE MFSTAT-EXEMPT-FLAG
                     TO MFCM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-MFSTAT' TO ABND-PARAGRAPH
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
                   MOVE MFDTE-RESEARCH-AMT
                     TO MFCM-RESEARCH-AMT
                   MOVE MFDTE-STORAGE-YTD
                     TO MFCM-STORAGE-YTD
                   MOVE MFDTE-POSTAGE-YTD
                     TO MFCM-POSTAGE-YTD
                   MOVE MFDTE-POSTAGE-AMT
                     TO MFCM-POSTAGE-AMT
                   MOVE MFDTE-EXEMPT-FLAG
                     TO MFCM-EXEMPT-FLAG
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
                   MOVE MFLIM-REVIEW-FLAG
                     TO MFCM-REVIEW-FLAG
                   MOVE MFLIM-STOP-FLAG
                     TO MFCM-STOP-FLAG
                   MOVE MFLIM-NOTICE-SENT-FLAG
                     TO MFCM-NOTICE-SENT-FLAG
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
           DISPLAY 'MFCCONV READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'MFCCONV' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
