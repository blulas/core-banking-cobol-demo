      *================================================================*
      * PROGRAM  : MFCRPT                                              *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MICROFICHE STATEMENTS - MANAGEMENT REPORTING. READS *
      *            THE MFC TRANSACTIONS, UPDATES THE MFC MASTER AND    *
      *            PRODUCES THE CONTROL REPORT. STATEMENT MICROFICHE   *
      *            INDEXING, RETENTION AND RESEARCH FEES               *
      * INPUT    : MFCMST (KSDS), MFCTRN, BANKCTL                      *
      * OUTPUT   : MFCOUT, MFCRPRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, MFELG                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-10-10 BHS  ORIGINAL                                       *
      * 1997-11-19 RJT  POSTAGE CUTOFF MOVED PRB7090                   *
      * 1999-01-19 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-05-21 GAF  AUDIT FINDING - POSTAGE CONTROL TOTALS CHG0845 *
      * 2002-05-08 JLM  POSTAGE ROUNDING - HALF UP REQ6825             *
      * 2003-07-22 DPS  COPY-FEE CALCULATION CORRECTED REQ8387         *
      * 2004-04-06 EWS  AUDIT FINDING - STORAGE CONTROL TOTALS REQ4647 *
      * 2005-10-22 BHS  POSTAGE NOTICE TEXT PER LEGAL INC5752          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MFCRPT.
       AUTHOR. R J TRAVERS.

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
           SELECT REPORT-FILE ASSIGN TO "MFCRPRPT"
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
                                           VALUE 23.000000.
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
      * WS-RESEARCH-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RESEARCH-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000180500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000343000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000347500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005150000000275000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015150000000175500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016150000000441500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026150000000132500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026650000000898500'.
       01  WS-RESEARCH-TIER-TBL REDEFINES WS-RESEARCH-TIER-VALUES.
           05  WS-RESEARCH-TIER-ROW OCCURS 8 TIMES.
               10  WS-RESEARCH-TIER-LOW    PIC 9(09)V99.
               10  WS-RESEARCH-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-STORAGE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-STORAGE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000636500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000566000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000319000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003750000000077000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008750000000153500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013750000000273500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016250000000818000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016750000000694500'.
       01  WS-STORAGE-GRID-TBL REDEFINES WS-STORAGE-GRID-VALUES.
           05  WS-STORAGE-GRID-ROW OCCURS 8 TIMES.
               10  WS-STORAGE-GRID-LOW     PIC 9(09)V99.
               10  WS-STORAGE-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RESEARCH-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RESEARCH-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000758500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000431000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000379000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022500000000825000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025000000000389500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027500000000659500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027550000000037000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027800000000280500'.
       01  WS-RESEARCH-SCHED-TBL REDEFINES WS-RESEARCH-SCHED-VALUES.
           05  WS-RESEARCH-SCHED-ROW OCCURS 8 TIMES.
               10  WS-RESEARCH-SCHED-LOW   PIC 9(09)V99.
               10  WS-RESEARCH-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COPY-FEE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COPY-FEE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000580500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000863500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000491000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012550000000384500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017550000000886500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020050000000177500'.
       01  WS-COPY-FEE-BAND-TBL REDEFINES WS-COPY-FEE-BAND-VALUES.
           05  WS-COPY-FEE-BAND-ROW OCCURS 6 TIMES.
               10  WS-COPY-FEE-BAND-LOW    PIC 9(09)V99.
               10  WS-COPY-FEE-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  MFCRPT  MICROFICHE STATEMENTS'.
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
               PERFORM 3000-PRORATE-COPY-FEE
               PERFORM 3010-EDIT-REF-NO
               PERFORM 3020-FLAG-COPY-FEE
               PERFORM 3030-ASSESS-REVERSAL
               PERFORM 3040-PRORATE-ITEM
               PERFORM 3050-SPLIT-STORAGE
               PERFORM 3060-ACCRUE-RESEARCH
               PERFORM 3070-ASSESS-HOLD
               PERFORM 3080-COUNT-CYCLE
               PERFORM 3090-PRORATE-RESEARCH
               PERFORM 3100-LIMIT-POSTAGE
               PERFORM 3110-CHECK-EXPIRY
               PERFORM 3120-ACCUM-FICHE-TYPE
               PERFORM 3130-AGE-CLOSE
               PERFORM 3140-EDIT-DESC
               PERFORM 3150-COUNT-NOTICE
               PERFORM 3160-SELECT-FEE-PCT
               PERFORM 3170-PRORATE-POSTAGE
               PERFORM 3180-DESCRIBE-RETENTION
               PERFORM 3190-ACCUM-FICHE-TYPE
               PERFORM 3200-EDIT-CUST-ID
               PERFORM 3210-SPLIT-RESEARCH
               PERFORM 3220-AGE-LAST-MAINT
               PERFORM 3230-EDIT-ADDR
               PERFORM 3240-PRORATE-RESEARCH
               PERFORM 3250-FLAG-RESEARCH
               PERFORM 3260-DESCRIBE-STATUS
               PERFORM 3270-CHECK-NEXT-DUE
               PERFORM 3280-COUNT-ITEM
               PERFORM 3290-PRORATE-POSTAGE
               PERFORM 3300-FLAG-STORAGE
               PERFORM 3310-CHECK-NEXT-DUE
               PERFORM 3320-LIMIT-ITEM
               PERFORM 3330-COUNT-SEQ
               PERFORM 3340-ASSESS-EXEMPT
               PERFORM 3350-EDIT-MEMO
               PERFORM 3360-SELECT-DISCOUNT
               PERFORM 3370-AGE-LAST-MAINT
               PERFORM 3380-EDIT-CONTACT
               PERFORM 3390-SELECT-MARGIN
               PERFORM 3400-SPLIT-POSTAGE
               PERFORM 3410-SELECT-MARGIN
               PERFORM 3420-SPLIT-COPY-FEE
               PERFORM 3430-FLAG-ITEM
               PERFORM 3440-COUNT-CYCLE
               PERFORM 3450-COUNT-NOTICE
               PERFORM 3460-FLAG-POSTAGE
               PERFORM 3470-ASSESS-STOP
               PERFORM 3480-LIMIT-POSTAGE
               PERFORM 3490-ACCRUE-RESEARCH
               PERFORM 3500-COUNT-NOTICE
               PERFORM 3510-ASSESS-EXEMPT
               PERFORM 3520-AGE-EXPIRY
               PERFORM 3530-SET-TRAN
               PERFORM 3540-PRORATE-RESEARCH
               PERFORM 3550-SELECT-FEE-PCT
               PERFORM 3560-AGE-EXPIRY
               PERFORM 3570-SET-CHANNEL
               PERFORM 3580-DESCRIBE-CHANNEL
               PERFORM 3590-CHECK-NOTICE
               PERFORM 3600-EDIT-REF-NO
               PERFORM 3610-SPLIT-POSTAGE
               PERFORM 3620-COUNT-ITEM
               PERFORM 3630-EDIT-OPERATOR
               PERFORM 3640-CHECK-EXPIRY
               PERFORM 3650-CHECK-NOTICE
               PERFORM 3660-ACCRUE-STORAGE
               PERFORM 3670-CHECK-CLOSE
               PERFORM 3680-CHECK-LAST-MAINT
               PERFORM 3690-ACCRUE-POSTAGE
               PERFORM 3700-PRORATE-RESEARCH
               PERFORM 3710-AGE-NOTICE
               PERFORM 3720-SELECT-FEE-PCT
               PERFORM 3730-COUNT-CYCLE
               PERFORM 3740-ASSESS-NOTICE-SENT
               PERFORM 3750-FLAG-RESEARCH
               PERFORM 3760-DESCRIBE-CHANNEL
               PERFORM 3770-ACCRUE-COPY-FEE
               PERFORM 3780-DESCRIBE-FICHE-TYPE
               PERFORM 3790-DESCRIBE-STATUS
               PERFORM 3800-SET-BRANCH
               PERFORM 3810-SPLIT-POSTAGE
               PERFORM 3820-CHECK-LAST-PAY
               PERFORM 3830-PRORATE-COPY-FEE
               PERFORM 3840-CHECK-EFF
               PERFORM 3850-ACCRUE-RESEARCH
               PERFORM 3860-ASSESS-STOP
               PERFORM 3870-SET-FICHE-TYPE
               PERFORM 3880-ACCUM-BRANCH
               PERFORM 3890-SELECT-MARGIN
               PERFORM 3900-COUNT-ITEM
               PERFORM 3910-PRORATE-STORAGE
               PERFORM 3920-EDIT-MEMO
               PERFORM 3930-FLAG-COPY-FEE
               PERFORM 3940-FLAG-RESEARCH
               PERFORM 3950-SET-TYPE
               PERFORM 3960-ACCRUE-POSTAGE
               PERFORM 3970-LIMIT-RESEARCH
               PERFORM 3980-SPLIT-POSTAGE
               PERFORM 3990-FLAG-POSTAGE
               PERFORM 4000-LIMIT-COPY-FEE
               PERFORM 4010-ASSESS-REVIEW
               PERFORM 4020-ASSESS-AUTO-RENEW
               PERFORM 4030-PRORATE-COPY-FEE
               PERFORM 4040-ACCUM-CHANNEL
               PERFORM 4050-SPLIT-RESEARCH
               PERFORM 4060-PRORATE-STORAGE
               PERFORM 4070-ASSESS-STOP
               PERFORM 4080-AGE-LAST-MAINT
               PERFORM 4090-PRORATE-RESEARCH
               PERFORM 4100-CHECK-LAST-PAY
               PERFORM 4110-EDIT-NAME
               PERFORM 4120-EDIT-MEMO
               PERFORM 4130-ASSESS-STOP
               PERFORM 4140-FLAG-ITEM
               PERFORM 4150-LIMIT-ITEM
               PERFORM 4160-EDIT-OPERATOR
               PERFORM 4170-EDIT-MEMO
               PERFORM 4180-AGE-NOTICE
               PERFORM 4190-SET-CHANNEL
               PERFORM 4200-SELECT-DISCOUNT
               PERFORM 4210-ACCUM-BRANCH
               PERFORM 4220-ACCUM-CHANNEL
               PERFORM 4230-SELECT-DISCOUNT
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
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COPY FEE AMT.
      *----------------------------------------------------------------*
       3000-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-COPY-FEE-AMT ROUNDED =
                   MFCT-RESEARCH-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCM-COPY-FEE-AMT
           END-IF
           ADD MFCM-COPY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E006 REF NO REQUIRED
      * E792 WORK AMT 2 NEGATIVE
      * E521 WORK AMT 2 OVER 50000
      * E407 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3010-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-REF-NO = SPACES
                   MOVE 'E006' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E792' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E521' TO WS-REJECT-CODE
               WHEN MFCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E407' TO WS-REJECT-CODE
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
       3020-FLAG-COPY-FEE.
           IF MFCT-COPY-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCT-COPY-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE RESEARCH YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3030-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MFCM-RESEARCH-YTD *
                       MFCM-FEE-PCT / 200
               WHEN MFCM-FICHE-TYPE-CD = 'AW'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN MFCM-FICHE-TYPE-CD = 'JM'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = MFCM-RESEARCH-YTD *
                       MFCM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3040-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT STORAGE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-STORAGE.
           COMPUTE WS-WORK-AMT = MFCM-STORAGE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MFCM-STORAGE-AMT - (WS-WORK-AMT * 2)
           COMPUTE MFCM-STORAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-RESEARCH.
           IF MFCM-RESEARCH-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-RESEARCH-AMT * MFCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MFCM-COPY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COPY FEE FEE ON THE POSTAGE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3070-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN MFCM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO MFCT-COPY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-REVIEW-FLAG = 'Y'
                   COMPUTE MFCT-COPY-FEE-AMT ROUNDED = MFCM-POSTAGE-AMT
                       * MFCM-DISCOUNT / 200
               WHEN MFCM-RETENTION-CD = 'KT'
                   MOVE 20.00 TO MFCT-COPY-FEE-AMT
               WHEN MFCM-RETENTION-CD = 'SJ'
                   MOVE 7.00 TO MFCT-COPY-FEE-AMT
               WHEN OTHER
                   COMPUTE MFCT-COPY-FEE-AMT ROUNDED = MFCM-POSTAGE-AMT
                       * MFCM-DISCOUNT / 100
           END-EVALUATE
           IF MFCT-COPY-FEE-AMT > 0 AND MFCT-COPY-FEE-AMT < 1.00
               MOVE 1.00 TO MFCT-COPY-FEE-AMT
           END-IF
           IF MFCT-COPY-FEE-AMT > 50.00
               MOVE 50.00 TO MFCT-COPY-FEE-AMT
           END-IF
           ADD MFCT-COPY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3080-COUNT-CYCLE.
           IF MFCM-CYCLE-CNT < 9999999
               ADD 1 TO MFCM-CYCLE-CNT
           END-IF
           IF MFCM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO MFCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESEARCH AMT.
      *----------------------------------------------------------------*
       3090-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-RESEARCH-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-RESEARCH-AMT
           END-IF
           ADD MFCT-RESEARCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * POSTAGE AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3100-LIMIT-POSTAGE.
           IF MFCT-POSTAGE-AMT < 100.00
               MOVE 100.00 TO MFCT-POSTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-POSTAGE-AMT > 10000.00
               MOVE 10000.00 TO MFCT-POSTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3110-CHECK-EXPIRY.
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
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY FICHE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-FICHE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-FICHE-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-FICHE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
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
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-POSTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E283 DESC REQUIRED
      * E298 RESEARCH AMT NEGATIVE
      * E711 RESEARCH AMT OVER 5000
      * E813 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-DESC = SPACES
                   MOVE 'E283' TO WS-REJECT-CODE
               WHEN MFCT-RESEARCH-AMT < 0
                   MOVE 'E298' TO WS-REJECT-CODE
               WHEN MFCT-RESEARCH-AMT > 5000.00
                   MOVE 'E711' TO WS-REJECT-CODE
               WHEN MFCM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E813' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3150-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH TIER TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3160-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-TIER-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MFCM-COPY-FEE-YTD NOT < WS-RESEARCH-TIER-LOW(WS-TX)
                   MOVE WS-RESEARCH-TIER-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO POSTAGE YTD.
      *----------------------------------------------------------------*
       3170-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-POSTAGE-YTD ROUNDED =
                   MFCM-RESEARCH-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCM-POSTAGE-YTD
           END-IF
           ADD MFCM-POSTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE RETENTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-RETENTION.
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
      * ACCUMULATE COPY FEE AMT BY FICHE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3190-ACCUM-FICHE-TYPE.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E543 CUST ID REQUIRED
      * E536 COPY FEE AMT NEGATIVE
      * E707 COPY FEE AMT OVER 250000
      * E472 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3200-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CUST-ID = SPACES
                   MOVE 'E543' TO WS-REJECT-CODE
               WHEN MFCT-COPY-FEE-AMT < 0
                   MOVE 'E536' TO WS-REJECT-CODE
               WHEN MFCT-COPY-FEE-AMT > 250000.00
                   MOVE 'E707' TO WS-REJECT-CODE
               WHEN MFCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E472' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCM-RESEARCH-AMT / 12
           COMPUTE WS-WORK-AMT-2 = MFCM-RESEARCH-AMT - (WS-WORK-AMT *
               12)
           COMPUTE MFCM-STORAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-LAST-MAINT.
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
               WHEN WS-DAYS NOT > 15
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-STORAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E884 ADDR REQUIRED
      * E153 RESEARCH YTD NEGATIVE
      * E621 RESEARCH YTD OVER 5000
      * E295 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3230-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-ADDR = SPACES
                   MOVE 'E884' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-YTD < 0
                   MOVE 'E153' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-YTD > 5000.00
                   MOVE 'E621' TO WS-REJECT-CODE
               WHEN MFCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E295' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESEARCH YTD.
      *----------------------------------------------------------------*
       3240-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-RESEARCH-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCM-RESEARCH-YTD
           END-IF
           ADD MFCM-RESEARCH-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RESEARCH AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3250-FLAG-RESEARCH.
           IF MFCT-RESEARCH-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCT-RESEARCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3260-DESCRIBE-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JXN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KJJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TZL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XZM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3270-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO MFCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-ITEM.
           IF MFCM-ITEM-CNT < 9999999
               ADD 1 TO MFCM-ITEM-CNT
           END-IF
           IF MFCM-ITEM-CNT NOT < 10
               MOVE 'Y' TO MFCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3290-PRORATE-POSTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MFCT-POSTAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STORAGE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3300-FLAG-STORAGE.
           IF MFCT-STORAGE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE MFCT-STORAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO MFCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3320-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3330-COUNT-SEQ.
           IF MFCT-SEQ < 9999999
               ADD 1 TO MFCT-SEQ
           END-IF
           IF MFCT-SEQ NOT < 10
               MOVE 'Y' TO MFCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3340-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MFCM-RESEARCH-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = WS-WORK-AMT-2 *
                       MFCM-DISCOUNT / 200
               WHEN MFCM-TYPE-CD = 'G'
                   MOVE 10.00 TO MFCM-RESEARCH-YTD
               WHEN MFCM-TYPE-CD = 'K'
                   MOVE 10.00 TO MFCM-RESEARCH-YTD
               WHEN OTHER
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = WS-WORK-AMT-2 *
                       MFCM-DISCOUNT / 100
           END-EVALUATE
           IF MFCM-RESEARCH-YTD > 0 AND MFCM-RESEARCH-YTD < 2.00
               MOVE 2.00 TO MFCM-RESEARCH-YTD
           END-IF
           IF MFCM-RESEARCH-YTD > 250.00
               MOVE 250.00 TO MFCM-RESEARCH-YTD
           END-IF
           ADD MFCM-RESEARCH-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E555 MEMO REQUIRED
      * E408 STORAGE YTD NEGATIVE
      * E940 STORAGE YTD OVER 99999
      * E217 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-MEMO = SPACES
                   MOVE 'E555' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-YTD < 0
                   MOVE 'E408' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-YTD > 99999.00
                   MOVE 'E940' TO WS-REJECT-CODE
               WHEN MFCM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E217' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE GRID TIER FOR THE STORAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3360-SELECT-DISCOUNT.
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
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-LAST-MAINT.
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
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E277 CONTACT REQUIRED
      * E093 STORAGE AMT NEGATIVE
      * E579 STORAGE AMT OVER 5000
      * E190 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-CONTACT = SPACES
                   MOVE 'E277' TO WS-REJECT-CODE
               WHEN MFCT-STORAGE-AMT < 0
                   MOVE 'E093' TO WS-REJECT-CODE
               WHEN MFCT-STORAGE-AMT > 5000.00
                   MOVE 'E579' TO WS-REJECT-CODE
               WHEN MFCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E190' TO WS-REJECT-CODE
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
       3390-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-SCHED-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MFCM-STORAGE-AMT NOT < WS-RESEARCH-SCHED-LOW(WS-TX)
                   MOVE WS-RESEARCH-SCHED-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFCM-POSTAGE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MFCM-POSTAGE-AMT - (WS-WORK-AMT * 2)
           COMPUTE MFCT-COPY-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH SCHED TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3410-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-SCHED-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MFCT-RESEARCH-AMT NOT < WS-RESEARCH-SCHED-LOW(WS-TX)
                   MOVE WS-RESEARCH-SCHED-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COPY FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-COPY-FEE.
           COMPUTE WS-WORK-AMT = MFCM-COPY-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MFCM-COPY-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE MFCM-COPY-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3430-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3440-COUNT-CYCLE.
           IF MFCM-CYCLE-CNT < 9999999
               ADD 1 TO MFCM-CYCLE-CNT
           END-IF
           IF MFCM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO MFCM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3450-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO MFCM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3460-FLAG-POSTAGE.
           IF MFCM-POSTAGE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFCM-POSTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE FEE ON THE RESEARCH AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3470-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MFCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MFCT-POSTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE MFCT-POSTAGE-AMT ROUNDED = MFCM-RESEARCH-AMT
                       * MFCM-FEE-PCT / 200
               WHEN MFCM-BRANCH-CD = 'AT'
                   MOVE 15.00 TO MFCT-POSTAGE-AMT
               WHEN MFCM-BRANCH-CD = 'KR'
                   MOVE 3.00 TO MFCT-POSTAGE-AMT
               WHEN OTHER
                   COMPUTE MFCT-POSTAGE-AMT ROUNDED = MFCM-RESEARCH-AMT
                       * MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCT-POSTAGE-AMT > 0 AND MFCT-POSTAGE-AMT < 5.00
               MOVE 5.00 TO MFCT-POSTAGE-AMT
           END-IF
           IF MFCT-POSTAGE-AMT > 35.00
               MOVE 35.00 TO MFCT-POSTAGE-AMT
           END-IF
           ADD MFCT-POSTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * POSTAGE AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3480-LIMIT-POSTAGE.
           IF MFCT-POSTAGE-AMT < 10.00
               MOVE 10.00 TO MFCT-POSTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCT-POSTAGE-AMT > 2500.00
               MOVE 2500.00 TO MFCT-POSTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-RESEARCH.
           IF MFCM-RESEARCH-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-RESEARCH-YTD * MFCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MFCT-RESEARCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3500-COUNT-NOTICE.
           IF MFCM-NOTICE-CNT < 9999999
               ADD 1 TO MFCM-NOTICE-CNT
           END-IF
           IF MFCM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO MFCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESEARCH YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3510-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN MFCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO MFCM-RESEARCH-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-HOLD-FLAG = 'Y'
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = WS-WORK-AMT *
                       MFCM-FEE-PCT / 200
               WHEN MFCM-RETENTION-CD = 'KT'
                   MOVE 3.00 TO MFCM-RESEARCH-YTD
               WHEN MFCM-RETENTION-CD = 'SJ'
                   MOVE 15.00 TO MFCM-RESEARCH-YTD
               WHEN OTHER
                   COMPUTE MFCM-RESEARCH-YTD ROUNDED = WS-WORK-AMT *
                       MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCM-RESEARCH-YTD > 0 AND MFCM-RESEARCH-YTD < 1.00
               MOVE 1.00 TO MFCM-RESEARCH-YTD
           END-IF
           IF MFCM-RESEARCH-YTD > 250.00
               MOVE 250.00 TO MFCM-RESEARCH-YTD
           END-IF
           ADD MFCM-RESEARCH-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-EXPIRY.
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
                   ADD MFCM-RESEARCH-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFCM-RESEARCH-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD MFCM-RESEARCH-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD MFCM-RESEARCH-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-RESEARCH-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IX -> RS WHEN MATURITY DATE IS PAST
      * RS -> CP WHEN MATURITY DATE IS PAST
      * CP -> PG WHEN MATURITY DATE IS PAST
      * PG -> IX WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3530-SET-TRAN.
           EVALUATE MFCT-TRAN-CD
               WHEN 'IX'
                   IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RS' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RS'
                   IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PG' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PG'
                   IF MFCM-MATURITY-DATE > 0 AND MFCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'IX' TO MFCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3540-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MFCT-RESEARCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE BAND TIER FOR THE POSTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-BAND-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF MFCT-POSTAGE-AMT NOT < WS-COPY-FEE-BAND-LOW(WS-TX)
                   MOVE WS-COPY-FEE-BAND-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3560-AGE-EXPIRY.
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
                   ADD MFCM-COPY-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD MFCM-COPY-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD MFCM-COPY-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCM-COPY-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCM-COPY-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * FV -> GL WHEN LAST STMT DATE IS PAST
      * GL -> HH WHEN LAST STMT DATE IS PAST
      * HH -> LM WHEN LAST STMT DATE IS PAST
      * LM -> MD WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-CHANNEL.
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
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3580-DESCRIBE-CHANNEL.
           EVALUATE MFCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
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
           IF WS-DAYS > 45
               MOVE 'Y' TO MFCM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E601 REF NO REQUIRED
      * E016 STORAGE AMT NEGATIVE
      * E017 STORAGE AMT OVER 250000
      * E222 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3600-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-REF-NO = SPACES
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN MFCT-STORAGE-AMT < 0
                   MOVE 'E016' TO WS-REJECT-CODE
               WHEN MFCT-STORAGE-AMT > 250000.00
                   MOVE 'E017' TO WS-REJECT-CODE
               WHEN MFCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E222' TO WS-REJECT-CODE
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
       3610-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE MFCM-POSTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-ITEM.
           IF MFCM-ITEM-CNT < 9999999
               ADD 1 TO MFCM-ITEM-CNT
           END-IF
           IF MFCM-ITEM-CNT NOT < 5
               MOVE 'Y' TO MFCM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E730 OPERATOR REQUIRED
      * E062 RESEARCH AMT NEGATIVE
      * E515 RESEARCH AMT OVER 1000000
      * E343 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-OPERATOR = SPACES
                   MOVE 'E730' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-AMT < 0
                   MOVE 'E062' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-AMT > 1000000.00
                   MOVE 'E515' TO WS-REJECT-CODE
               WHEN MFCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E343' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3640-CHECK-EXPIRY.
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
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-NOTICE.
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
               MOVE 'Y' TO MFCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STORAGE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3660-ACCRUE-STORAGE.
           IF MFCT-STORAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-STORAGE-AMT * MFCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO MFCM-COPY-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3670-CHECK-CLOSE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3680-CHECK-LAST-MAINT.
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
               MOVE 'Y' TO MFCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3690-ACCRUE-POSTAGE.
           IF MFCM-POSTAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-POSTAGE-YTD * MFCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO MFCM-STORAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESEARCH AMT.
      *----------------------------------------------------------------*
       3700-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-RESEARCH-AMT ROUNDED =
                   MFCM-RESEARCH-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-RESEARCH-AMT
           END-IF
           ADD MFCT-RESEARCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-NOTICE.
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
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-RESEARCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RESEARCH TIER TIER FOR THE COPY FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3720-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-TIER-RATE(1) TO MFCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MFCM-COPY-FEE-YTD NOT < WS-RESEARCH-TIER-LOW(WS-TX)
                   MOVE WS-RESEARCH-TIER-RATE(WS-TX) TO MFCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-CYCLE.
           IF MFCM-CYCLE-CNT < 9999999
               ADD 1 TO MFCM-CYCLE-CNT
           END-IF
           IF MFCM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO MFCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STORAGE FEE ON THE POSTAGE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3740-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN MFCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO MFCT-STORAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-STOP-FLAG = 'Y'
                   COMPUTE MFCT-STORAGE-AMT ROUNDED = MFCM-POSTAGE-AMT *
                       MFCM-FEE-PCT / 200
               WHEN MFCM-STATUS-CD = 'AST'
                   MOVE 10.00 TO MFCT-STORAGE-AMT
               WHEN MFCM-STATUS-CD = 'JXN'
                   MOVE 20.00 TO MFCT-STORAGE-AMT
               WHEN OTHER
                   COMPUTE MFCT-STORAGE-AMT ROUNDED = MFCM-POSTAGE-AMT *
                       MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCT-STORAGE-AMT > 0 AND MFCT-STORAGE-AMT < 2.00
               MOVE 2.00 TO MFCT-STORAGE-AMT
           END-IF
           IF MFCT-STORAGE-AMT > 35.00
               MOVE 35.00 TO MFCT-STORAGE-AMT
           END-IF
           ADD MFCT-STORAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RESEARCH AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3750-FLAG-RESEARCH.
           IF MFCM-RESEARCH-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFCM-RESEARCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3760-DESCRIBE-CHANNEL.
           EVALUATE MFCM-CHANNEL-CD
               WHEN 'FV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ND'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COPY FEE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3770-ACCRUE-COPY-FEE.
           IF MFCT-COPY-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-COPY-FEE-AMT * MFCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO MFCT-RESEARCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE FICHE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3780-DESCRIBE-FICHE-TYPE.
           EVALUATE MFCM-FICHE-TYPE-CD
               WHEN 'AW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3790-DESCRIBE-STATUS.
           EVALUATE MFCM-STATUS-CD
               WHEN 'AST'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JXN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KJJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TZL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XZM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * AT -> KR WHEN LAST PAY DATE IS PAST
      * KR -> LK WHEN LAST PAY DATE IS PAST
      * LK -> US WHEN LAST PAY DATE IS PAST
      * US -> AT WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3800-SET-BRANCH.
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
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3810-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE MFCM-POSTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3820-CHECK-LAST-PAY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO MFCM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE POSTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COPY FEE AMT.
      *----------------------------------------------------------------*
       3830-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCT-COPY-FEE-AMT ROUNDED =
                   MFCM-POSTAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCT-COPY-FEE-AMT
           END-IF
           ADD MFCT-COPY-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3840-CHECK-EFF.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO MFCM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESEARCH YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3850-ACCRUE-RESEARCH.
           IF MFCM-RESEARCH-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCM-RESEARCH-YTD * MFCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO MFCM-POSTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COPY FEE FEE ON THE RESEARCH AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3860-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MFCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MFCM-COPY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   COMPUTE MFCM-COPY-FEE-AMT ROUNDED = MFCM-RESEARCH-AMT
                       * MFCM-DISCOUNT / 200
               WHEN MFCT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO MFCM-COPY-FEE-AMT
               WHEN MFCT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO MFCM-COPY-FEE-AMT
               WHEN OTHER
                   COMPUTE MFCM-COPY-FEE-AMT ROUNDED = MFCM-RESEARCH-AMT
                       * MFCM-DISCOUNT / 100
           END-EVALUATE
           IF MFCM-COPY-FEE-AMT > 0 AND MFCM-COPY-FEE-AMT < 1.00
               MOVE 1.00 TO MFCM-COPY-FEE-AMT
           END-IF
           IF MFCM-COPY-FEE-AMT > 75.00
               MOVE 75.00 TO MFCM-COPY-FEE-AMT
           END-IF
           ADD MFCM-COPY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR FICHE TYPE CD:
      * AW -> JM WHEN EFF DATE IS PAST
      * JM -> KV WHEN EFF DATE IS PAST
      * KV -> MD WHEN EFF DATE IS PAST
      * MD -> NR WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-FICHE-TYPE.
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
      * ACCUMULATE WORK AMT 2 BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3880-ACCUM-BRANCH.
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
      * SELECT THE RESEARCH TIER TIER FOR THE RESEARCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3890-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESEARCH-TIER-RATE(1) TO MFCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF MFCM-RESEARCH-AMT NOT < WS-RESEARCH-TIER-LOW(WS-TX)
                   MOVE WS-RESEARCH-TIER-RATE(WS-TX) TO MFCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3900-COUNT-ITEM.
           IF MFCM-ITEM-CNT < 9999999
               ADD 1 TO MFCM-ITEM-CNT
           END-IF
           IF MFCM-ITEM-CNT NOT < 12
               MOVE 'Y' TO MFCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       3910-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-STORAGE-AMT ROUNDED =
                   MFCM-RESEARCH-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCM-STORAGE-AMT
           END-IF
           ADD MFCM-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E330 MEMO REQUIRED
      * E093 WORK AMT NEGATIVE
      * E609 WORK AMT OVER 99999
      * E106 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-MEMO = SPACES
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E093' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E609' TO WS-REJECT-CODE
               WHEN MFCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3930-FLAG-COPY-FEE.
           IF MFCM-COPY-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFCM-COPY-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3940-FLAG-RESEARCH.
           IF MFCM-RESEARCH-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE MFCM-RESEARCH-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * G -> K WHEN CLOSE DATE IS PAST
      * K -> W WHEN CLOSE DATE IS PAST
      * W -> Y WHEN CLOSE DATE IS PAST
      * Y -> G WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3950-SET-TYPE.
           EVALUATE MFCM-TYPE-CD
               WHEN 'G'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'W'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO MFCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON POSTAGE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3960-ACCRUE-POSTAGE.
           IF MFCT-POSTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   MFCT-POSTAGE-AMT * MFCM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO MFCM-STORAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESEARCH YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3970-LIMIT-RESEARCH.
           IF MFCM-RESEARCH-YTD < 0.00
               MOVE 0.00 TO MFCM-RESEARCH-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-RESEARCH-YTD > 500.00
               MOVE 500.00 TO MFCM-RESEARCH-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT POSTAGE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3980-SPLIT-POSTAGE.
           COMPUTE WS-WORK-AMT = MFCM-POSTAGE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = MFCM-POSTAGE-AMT - (WS-WORK-AMT * 3)
           COMPUTE MFCM-POSTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * POSTAGE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3990-FLAG-POSTAGE.
           IF MFCT-POSTAGE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE MFCT-POSTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * COPY FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4000-LIMIT-COPY-FEE.
           IF MFCM-COPY-FEE-AMT < 10.00
               MOVE 10.00 TO MFCM-COPY-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF MFCM-COPY-FEE-AMT > 50000.00
               MOVE 50000.00 TO MFCM-COPY-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COPY FEE FEE ON THE POSTAGE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4010-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN MFCM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO MFCM-COPY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   COMPUTE MFCM-COPY-FEE-AMT ROUNDED = MFCM-POSTAGE-AMT
                       * MFCM-MARGIN / 200
               WHEN MFCM-FICHE-TYPE-CD = 'AW'
                   MOVE 20.00 TO MFCM-COPY-FEE-AMT
               WHEN MFCM-FICHE-TYPE-CD = 'JM'
                   MOVE 12.00 TO MFCM-COPY-FEE-AMT
               WHEN OTHER
                   COMPUTE MFCM-COPY-FEE-AMT ROUNDED = MFCM-POSTAGE-AMT
                       * MFCM-MARGIN / 100
           END-EVALUATE
           IF MFCM-COPY-FEE-AMT > 0 AND MFCM-COPY-FEE-AMT < 1.00
               MOVE 1.00 TO MFCM-COPY-FEE-AMT
           END-IF
           IF MFCM-COPY-FEE-AMT > 50.00
               MOVE 50.00 TO MFCM-COPY-FEE-AMT
           END-IF
           ADD MFCM-COPY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE POSTAGE YTD FEE ON THE RESEARCH AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4020-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN MFCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO MFCM-POSTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   COMPUTE MFCM-POSTAGE-YTD ROUNDED = MFCM-RESEARCH-AMT
                       * MFCM-FEE-PCT / 200
               WHEN MFCM-CHANNEL-CD = 'FV'
                   MOVE 3.00 TO MFCM-POSTAGE-YTD
               WHEN MFCM-CHANNEL-CD = 'GL'
                   MOVE 5.00 TO MFCM-POSTAGE-YTD
               WHEN OTHER
                   COMPUTE MFCM-POSTAGE-YTD ROUNDED = MFCM-RESEARCH-AMT
                       * MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCM-POSTAGE-YTD > 0 AND MFCM-POSTAGE-YTD < 10.00
               MOVE 10.00 TO MFCM-POSTAGE-YTD
           END-IF
           IF MFCM-POSTAGE-YTD > 50.00
               MOVE 50.00 TO MFCM-POSTAGE-YTD
           END-IF
           ADD MFCM-POSTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE COPY FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4030-PRORATE-COPY-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   MFCM-COPY-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE RESEARCH AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4040-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCT-CHANNEL-CD
                   ADD MFCT-RESEARCH-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCT-RESEARCH-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESEARCH AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4050-SPLIT-RESEARCH.
           COMPUTE WS-WORK-AMT = MFCM-RESEARCH-AMT / 2
           COMPUTE WS-WORK-AMT-2 = MFCM-RESEARCH-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STORAGE AMT.
      *----------------------------------------------------------------*
       4060-PRORATE-STORAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-STORAGE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO MFCM-STORAGE-AMT
           END-IF
           ADD MFCM-STORAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE COPY FEE FEE ON THE COPY FEE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4070-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MFCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MFCT-COPY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE MFCT-COPY-FEE-AMT ROUNDED = MFCM-COPY-FEE-AMT
                       * MFCM-FEE-PCT / 200
               WHEN MFCM-RETENTION-CD = 'KT'
                   MOVE 12.00 TO MFCT-COPY-FEE-AMT
               WHEN MFCM-RETENTION-CD = 'SJ'
                   MOVE 15.00 TO MFCT-COPY-FEE-AMT
               WHEN OTHER
                   COMPUTE MFCT-COPY-FEE-AMT ROUNDED = MFCM-COPY-FEE-AMT
                       * MFCM-FEE-PCT / 100
           END-EVALUATE
           IF MFCT-COPY-FEE-AMT > 0 AND MFCT-COPY-FEE-AMT < 1.00
               MOVE 1.00 TO MFCT-COPY-FEE-AMT
           END-IF
           IF MFCT-COPY-FEE-AMT > 50.00
               MOVE 50.00 TO MFCT-COPY-FEE-AMT
           END-IF
           ADD MFCT-COPY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4080-AGE-LAST-MAINT.
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
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD MFCT-COPY-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RESEARCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESEARCH AMT.
      *----------------------------------------------------------------*
       4090-PRORATE-RESEARCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE MFCM-RESEARCH-AMT ROUNDED =
                   MFCT-RESEARCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO MFCM-RESEARCH-AMT
           END-IF
           ADD MFCM-RESEARCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4100-CHECK-LAST-PAY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO MFCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E149 NAME REQUIRED
      * E012 COPY FEE AMT NEGATIVE
      * E719 COPY FEE AMT OVER 10000
      * E106 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4110-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCM-NAME = SPACES
                   MOVE 'E149' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-AMT < 0
                   MOVE 'E012' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-AMT > 10000.00
                   MOVE 'E719' TO WS-REJECT-CODE
               WHEN MFCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E106' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E463 MEMO REQUIRED
      * E549 RESEARCH YTD NEGATIVE
      * E878 RESEARCH YTD OVER 250000
      * E557 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4120-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-MEMO = SPACES
                   MOVE 'E463' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-YTD < 0
                   MOVE 'E549' TO WS-REJECT-CODE
               WHEN MFCM-RESEARCH-YTD > 250000.00
                   MOVE 'E878' TO WS-REJECT-CODE
               WHEN MFCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E557' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COPY FEE FEE ON THE RESEARCH YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4130-ASSESS-STOP.
           EVALUATE TRUE
               WHEN MFCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO MFCM-COPY-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN MFCT-REVERSAL-FLAG = 'Y'
                   COMPUTE MFCM-COPY-FEE-AMT ROUNDED = MFCM-RESEARCH-YTD
                       * MFCM-DISCOUNT / 200
               WHEN MFCM-FICHE-TYPE-CD = 'AW'
                   MOVE 5.00 TO MFCM-COPY-FEE-AMT
               WHEN MFCM-FICHE-TYPE-CD = 'JM'
                   MOVE 15.00 TO MFCM-COPY-FEE-AMT
               WHEN OTHER
                   COMPUTE MFCM-COPY-FEE-AMT ROUNDED = MFCM-RESEARCH-YTD
                       * MFCM-DISCOUNT / 100
           END-EVALUATE
           IF MFCM-COPY-FEE-AMT > 0 AND MFCM-COPY-FEE-AMT < 10.00
               MOVE 10.00 TO MFCM-COPY-FEE-AMT
           END-IF
           IF MFCM-COPY-FEE-AMT > 150.00
               MOVE 150.00 TO MFCM-COPY-FEE-AMT
           END-IF
           ADD MFCM-COPY-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4140-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4150-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E314 OPERATOR REQUIRED
      * E878 COPY FEE AMT NEGATIVE
      * E645 COPY FEE AMT OVER 99999
      * E750 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4160-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-OPERATOR = SPACES
                   MOVE 'E314' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-AMT < 0
                   MOVE 'E878' TO WS-REJECT-CODE
               WHEN MFCM-COPY-FEE-AMT > 99999.00
                   MOVE 'E645' TO WS-REJECT-CODE
               WHEN MFCM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E750' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E652 MEMO REQUIRED
      * E675 STORAGE YTD NEGATIVE
      * E277 STORAGE YTD OVER 1000000
      * E502 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4170-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN MFCT-MEMO = SPACES
                   MOVE 'E652' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-YTD < 0
                   MOVE 'E675' TO WS-REJECT-CODE
               WHEN MFCM-STORAGE-YTD > 1000000.00
                   MOVE 'E277' TO WS-REJECT-CODE
               WHEN MFCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E502' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4180-AGE-NOTICE.
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
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN CLOSE DATE IS PAST
      * BAT -> MAL WHEN CLOSE DATE IS PAST
      * MAL -> TEL WHEN CLOSE DATE IS PAST
      * TEL -> ONL WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4190-SET-CHANNEL.
           EVALUATE MFCT-CHANNEL-CD
               WHEN 'BRN'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF MFCM-CLOSE-DATE > 0 AND MFCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO MFCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COPY FEE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4200-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COPY-FEE-BAND-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-COPY-FEE-BAND-LOW(WS-TX)
                   MOVE WS-COPY-FEE-BAND-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE POSTAGE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4210-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCM-BRANCH-CD
                   ADD MFCT-POSTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE MFCT-POSTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4220-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = MFCT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE MFCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE STORAGE GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-STORAGE-GRID-RATE(1) TO MFCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-STORAGE-GRID-LOW(WS-TX)
                   MOVE WS-STORAGE-GRID-RATE(WS-TX) TO MFCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF MFCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO MFCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

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
                   MOVE MFELG-POSTAGE-AMT
                     TO MFCM-POSTAGE-AMT
                   MOVE MFELG-COPY-FEE-AMT
                     TO MFCM-COPY-FEE-AMT
                   MOVE MFELG-STORAGE-AMT
                     TO MFCM-STORAGE-AMT
                   MOVE MFELG-COPY-FEE-YTD
                     TO MFCM-COPY-FEE-YTD
                   MOVE MFELG-EXEMPT-FLAG
                     TO MFCM-EXEMPT-FLAG
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
           DISPLAY 'MFCRPT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'MFCRPT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
