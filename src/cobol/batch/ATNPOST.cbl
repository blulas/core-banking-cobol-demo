      *================================================================*
      * PROGRAM  : ATNPOST                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : ATM NETWORK SETTLEMENT - DAILY POSTING. READS THE   *
      *            ATN TRANSACTIONS, UPDATES THE ATN MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. PROPRIETARY SHARED ATM *
      *            NETWORK SETTLEMENT AND INTERCHANGE                  *
      * INPUT    : ATNMST (KSDS), ATNTRN, BANKCTL                      *
      * OUTPUT   : ATNOUT, ATNPORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ATVAL                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-08-02 EWS  ORIGINAL                                       *
      * 1996-04-01 PQR  ADJUST EXCEPTIONS TO OPS QUEUE CHG8091         *
      * 2002-10-11 GAF  INTERCHG CALCULATION CORRECTED INC8870         *
      * 2003-07-19 GAF  INTERCHG NOTICE TEXT PER LEGAL INC3671         *
      * 2004-02-17 CRP  DISPENSED CUTOFF MOVED INC8617                 *
      * 2006-06-07 GAF  SURCHG NOTICE TEXT PER LEGAL REQ9775           *
      * 2010-05-16 JLM  NEW ADJUST REPORT COLUMNS PRB9459              *
      * 2012-05-23 BHS  CASSETTE ROUNDING - HALF UP CHG0219            *
      * 2013-10-13 RJT  AUDIT FINDING - DISPENSED CONTROL TOTALS CHG644*
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATNPOST.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "ATNMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ATNM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "ATNTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "ATNOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "ATNPORPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY ATNMST.
       FD  TRAN-FILE.
       COPY ATNTRN.
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
      * WS-INTERCHG-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTERCHG-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000661000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000169000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000140500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000336500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011750000000174500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011850000000558000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012350000000890500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012450000000632000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017450000000056500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017550000000506500'.
       01  WS-INTERCHG-TIER-TBL REDEFINES WS-INTERCHG-TIER-VALUES.
           05  WS-INTERCHG-TIER-ROW OCCURS 10 TIMES.
               10  WS-INTERCHG-TIER-LOW    PIC 9(09)V99.
               10  WS-INTERCHG-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SURCHG-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SURCHG-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000724000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000112000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000707000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020500000000196500'.
       01  WS-SURCHG-BAND-TBL REDEFINES WS-SURCHG-BAND-VALUES.
           05  WS-SURCHG-BAND-ROW OCCURS 4 TIMES.
               10  WS-SURCHG-BAND-LOW      PIC 9(09)V99.
               10  WS-SURCHG-BAND-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SURCHG-BANDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SURCHG-BANDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000778000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000192000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000099000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000847000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011750000000232000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000257500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012850000000576500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017850000000093000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027850000000070000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028100000000594000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029100000000362000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031600000000125000'.
       01  WS-SURCHG-BANDX-TBL REDEFINES WS-SURCHG-BANDX-VALUES.
           05  WS-SURCHG-BANDX-ROW OCCURS 12 TIMES.
               10  WS-SURCHG-BANDX-LOW     PIC 9(09)V99.
               10  WS-SURCHG-BANDX-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-ADJUST-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-ADJUST-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000128000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000763000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000128000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000151500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002800000000775000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000878500'.
       01  WS-ADJUST-GRID-TBL REDEFINES WS-ADJUST-GRID-VALUES.
           05  WS-ADJUST-GRID-ROW OCCURS 6 TIMES.
               10  WS-ADJUST-GRID-LOW      PIC 9(09)V99.
               10  WS-ADJUST-GRID-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ATNPOST  ATM NETWORK SETTLEMENT'.
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
       COPY ATVALP.

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
               MOVE 'ATNMST' TO ABND-FILE-NAME
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
           MOVE ATNT-KEY TO ATNM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-PRORATE-SURCHG
               PERFORM 3010-ACCRUE-SETTLE
               PERFORM 3020-ACCRUE-ADJUST
               PERFORM 3030-ACCUM-REGION
               PERFORM 3040-SET-TRAN
               PERFORM 3050-ACCUM-CHANNEL
               PERFORM 3060-ACCRUE-CASSETTE
               PERFORM 3070-CHECK-EFF
               PERFORM 3080-ACCRUE-SETTLE
               PERFORM 3090-LIMIT-DISPENSED
               PERFORM 3100-AGE-EXPIRY
               PERFORM 3110-FLAG-INTERCHG
               PERFORM 3120-DESCRIBE-STATUS
               PERFORM 3130-ACCRUE-INTERCHG
               PERFORM 3140-SELECT-DISCOUNT
               PERFORM 3150-ACCUM-REASON
               PERFORM 3160-SPLIT-ADJUST
               PERFORM 3170-ACCUM-BRANCH
               PERFORM 3180-PRORATE-SETTLE
               PERFORM 3190-EDIT-OPERATOR
               PERFORM 3200-ASSESS-NOTICE-SENT
               PERFORM 3210-CHECK-POST
               PERFORM 3220-AGE-EFF
               PERFORM 3230-SET-TRAN
               PERFORM 3240-ACCUM-REGION
               PERFORM 3250-LIMIT-ADJUST
               PERFORM 3260-DESCRIBE-NETWORK
               PERFORM 3270-PRORATE-SETTLE
               PERFORM 3280-PRORATE-CASSETTE
               PERFORM 3290-SET-NETWORK
               PERFORM 3300-COUNT-ITEM
               PERFORM 3310-ACCRUE-SETTLE
               PERFORM 3320-CHECK-POST
               PERFORM 3330-EDIT-NAME
               PERFORM 3340-FLAG-SETTLE
               PERFORM 3350-ASSESS-NOTICE-SENT
               PERFORM 3360-ASSESS-REVIEW
               PERFORM 3370-FLAG-CASSETTE
               PERFORM 3380-ACCRUE-RATE
               PERFORM 3390-SELECT-FEE-PCT
               PERFORM 3400-FLAG-INTERCHG
               PERFORM 3410-ACCUM-TERMINAL
               PERFORM 3420-ASSESS-EXEMPT
               PERFORM 3430-LIMIT-SETTLE
               PERFORM 3440-CHECK-NEXT-DUE
               PERFORM 3450-CHECK-NEXT-DUE
               PERFORM 3460-COUNT-RETRY
               PERFORM 3470-LIMIT-INTERCHG
               PERFORM 3480-ASSESS-REVERSAL
               PERFORM 3490-ACCUM-TERMINAL
               PERFORM 3500-AGE-NEXT-REVIEW
               PERFORM 3510-ACCUM-NETWORK
               PERFORM 3520-SELECT-RATE
               PERFORM 3530-SELECT-RATE
               PERFORM 3540-DESCRIBE-REGION
               PERFORM 3550-ACCRUE-CASSETTE
               PERFORM 3560-CHECK-CLOSE
               PERFORM 3570-FLAG-DISPENSED
               PERFORM 3580-SET-TRAN
               PERFORM 3590-ACCUM-REGION
               PERFORM 3600-SELECT-DISCOUNT
               PERFORM 3610-ACCRUE-ADJUST
               PERFORM 3620-SET-REGION
               PERFORM 3630-DESCRIBE-STATUS
               PERFORM 3640-EDIT-NAME
               PERFORM 3650-ACCRUE-SURCHG
               PERFORM 3660-EDIT-REJECT
               PERFORM 3670-LIMIT-ADJUST
               PERFORM 3680-LIMIT-CASSETTE
               PERFORM 3690-SPLIT-ADJUST
               PERFORM 3700-CHECK-NEXT-DUE
               PERFORM 3710-CHECK-EFF
               PERFORM 3720-SPLIT-SETTLE
               PERFORM 3730-DESCRIBE-STATUS
               PERFORM 3740-COUNT-ITEM
               PERFORM 3750-SELECT-RATE
               PERFORM 3760-AGE-CLOSE
               PERFORM 3770-ACCRUE-SURCHG
               PERFORM 3780-ASSESS-HOLD
               PERFORM 3790-FLAG-ADJUST
               PERFORM 3800-FLAG-ITEM
               PERFORM 3810-EDIT-OPERATOR
               PERFORM 3820-SET-TRAN
               PERFORM 3830-PRORATE-DISPENSED
               PERFORM 3840-DESCRIBE-NETWORK
               PERFORM 3850-SPLIT-SETTLE
               PERFORM 3860-ACCRUE-DISPENSED
               PERFORM 3870-SET-REGION
               PERFORM 3880-DESCRIBE-CHANNEL
               PERFORM 3890-PRORATE-CASSETTE
               PERFORM 3900-ACCRUE-DISPENSED
               PERFORM 3910-ASSESS-NOTICE-SENT
               PERFORM 3920-COUNT-NOTICE
               PERFORM 3930-EDIT-REF-NO
               PERFORM 3940-SET-REASON
               PERFORM 3950-EDIT-DESC
               PERFORM 3960-PRORATE-ADJUST
               PERFORM 3970-AGE-EXPIRY
               PERFORM 3980-AGE-NOTICE
               PERFORM 3990-SET-REASON
               PERFORM 4000-PRORATE-ADJUST
               PERFORM 4010-ASSESS-SUSPEND
               PERFORM 4020-SET-REASON
               PERFORM 4030-PRORATE-ADJUST
               PERFORM 4040-ACCRUE-DISPENSED
               PERFORM 4050-ACCRUE-DISCOUNT
               PERFORM 4060-COUNT-ITEM
               PERFORM 4070-EDIT-CUST-ID
               PERFORM 4080-AGE-LAST-PAY
               PERFORM 4090-AGE-NOTICE
               PERFORM 4100-ASSESS-NOTICE-SENT
               PERFORM 4110-PRORATE-ADJUST
               PERFORM 4120-COUNT-ITEM
               PERFORM 4130-DESCRIBE-REASON
               PERFORM 4140-DESCRIBE-TRAN
               PERFORM 4150-SPLIT-CASSETTE
               PERFORM 4160-COUNT-PAYMENT
               PERFORM 4170-PRORATE-SURCHG
               PERFORM 4180-FLAG-SETTLE
               PERFORM 4190-PRORATE-DISPENSED
               PERFORM 4200-ASSESS-STOP
               PERFORM 4210-FLAG-SURCHG
               PERFORM 4220-ACCUM-CHANNEL
               PERFORM 4230-ASSESS-SUSPEND
               PERFORM 4240-AGE-OPEN
               PERFORM 4250-COUNT-ITEM
               PERFORM 4260-PRORATE-DISPENSED
               PERFORM 4270-LIMIT-SURCHG
               PERFORM 4280-FLAG-DISPENSED
               PERFORM 4290-SPLIT-ADJUST
               PERFORM 4300-AGE-MATURITY
               PERFORM 4310-SELECT-DISCOUNT
               PERFORM 4320-ASSESS-REVIEW
               PERFORM 4330-LIMIT-SURCHG
               PERFORM 4340-DESCRIBE-TERMINAL
               PERFORM 4350-AGE-NOTICE
               PERFORM 4360-CHECK-EFF
               PERFORM 4370-SELECT-FEE-PCT
               PERFORM 4380-SELECT-FEE-PCT
               PERFORM 4390-PRORATE-DISPENSED
               PERFORM 4400-SELECT-RATE
               PERFORM 4410-EDIT-CUST-ID
               PERFORM 4420-SELECT-RATE
               PERFORM 4430-ACCRUE-DISPENSED
               PERFORM 4440-ACCRUE-INTERCHG
               PERFORM 4450-SELECT-DISCOUNT
               PERFORM 4460-CHECK-EXPIRY
               PERFORM 4470-PRORATE-ADJUST
               PERFORM 4480-COUNT-NOTICE
               PERFORM 4490-COUNT-SEQ
               PERFORM 4500-PRORATE-DISPENSED
               PERFORM 4510-CHECK-LAST-PAY
               PERFORM 4520-ASSESS-NOTICE-SENT
               PERFORM 4530-COUNT-RETRY
               PERFORM 4540-ASSESS-SUSPEND
               PERFORM 4550-SELECT-DISCOUNT
               PERFORM 4560-ACCRUE-CASSETTE
               PERFORM 4570-SELECT-DISCOUNT
               PERFORM 4580-PRORATE-SURCHG
               PERFORM 4590-SELECT-DISCOUNT
               PERFORM 4600-ASSESS-EXEMPT
               PERFORM 4610-EDIT-CONTACT
               PERFORM 7030-CALL-ATVAL
               REWRITE ATN-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'ATNMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM ATN-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * PRORATE DISPENSED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SURCHG AMT.
      *----------------------------------------------------------------*
       3000-PRORATE-SURCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-SURCHG-AMT ROUNDED =
                   ATNT-DISPENSED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-SURCHG-AMT
           END-IF
           ADD ATNM-SURCHG-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-SETTLE.
           IF ATNM-SETTLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-SETTLE-YTD * ATNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATNM-CASSETTE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADJUST AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3020-ACCRUE-ADJUST.
           IF ATNM-ADJUST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-ADJUST-AMT * ATNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ATNM-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3030-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> BI WHEN EFF DATE IS PAST
      * BI -> TR WHEN EFF DATE IS PAST
      * TR -> AD WHEN EFF DATE IS PAST
      * AD -> RV WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-TRAN.
           EVALUATE ATNT-TRAN-CD
               WHEN 'WD'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BI' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BI'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TR' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TR'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE ADJUST AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNT-CHANNEL-CD
                   ADD ATNM-ADJUST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-ADJUST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASSETTE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-CASSETTE.
           IF ATNT-CASSETTE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-CASSETTE-AMT * ATNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF ATNT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-SETTLE.
           IF ATNT-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-SETTLE-AMT * ATNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATNM-INTERCHG-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3090-LIMIT-DISPENSED.
           IF ATNM-DISPENSED-YTD < 0.00
               MOVE 0.00 TO ATNM-DISPENSED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-DISPENSED-YTD > 2500.00
               MOVE 2500.00 TO ATNM-DISPENSED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTERCHG YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3110-FLAG-INTERCHG.
           IF ATNM-INTERCHG-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-INTERCHG-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3120-DESCRIBE-STATUS.
           EVALUATE ATNM-STATUS-CD
               WHEN 'DF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTERCHG AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3130-ACCRUE-INTERCHG.
           IF ATNM-INTERCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-INTERCHG-AMT * ATNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ATNM-CASSETTE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG BANDX TIER FOR THE ADJUST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3140-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-BANDX-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ATNM-ADJUST-YTD NOT < WS-SURCHG-BANDX-LOW(WS-TX)
                   MOVE WS-SURCHG-BANDX-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3150-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-REASON-CD
                   ADD ATNM-CASSETTE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-CASSETTE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADJUST YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3160-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATNM-ADJUST-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ATNM-ADJUST-YTD - (WS-WORK-AMT * 4)
           COMPUTE ATNM-SETTLE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-BRANCH-CD
                   ADD ATNT-CASSETTE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-CASSETTE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTERCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       3180-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-SETTLE-AMT ROUNDED =
                   ATNM-INTERCHG-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-SETTLE-AMT
           END-IF
           ADD ATNM-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E123 OPERATOR REQUIRED
      * E010 DISPENSED AMT NEGATIVE
      * E443 DISPENSED AMT OVER 99999
      * E512 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3190-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-OPERATOR = SPACES
                   MOVE 'E123' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-AMT < 0
                   MOVE 'E010' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-AMT > 99999.00
                   MOVE 'E443' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E512' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADJUST YTD FEE ON THE DISPENSED AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3200-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-ADJUST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-HOLD-FLAG = 'Y'
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = ATNT-DISPENSED-AMT
                       * ATNM-FEE-PCT / 200
               WHEN ATNT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO ATNM-ADJUST-YTD
               WHEN ATNT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO ATNM-ADJUST-YTD
               WHEN OTHER
                   COMPUTE ATNM-ADJUST-YTD ROUNDED = ATNT-DISPENSED-AMT
                       * ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-ADJUST-YTD > 0 AND ATNM-ADJUST-YTD < 5.00
               MOVE 5.00 TO ATNM-ADJUST-YTD
           END-IF
           IF ATNM-ADJUST-YTD > 25.00
               MOVE 25.00 TO ATNM-ADJUST-YTD
           END-IF
           ADD ATNM-ADJUST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF ATNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SURCHG-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> BI WHEN NEXT DUE DATE IS PAST
      * BI -> TR WHEN NEXT DUE DATE IS PAST
      * TR -> AD WHEN NEXT DUE DATE IS PAST
      * AD -> RV WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-TRAN.
           EVALUATE ATNT-TRAN-CD
               WHEN 'WD'
                   IF ATNM-NEXT-DUE-DATE > 0 AND ATNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BI' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BI'
                   IF ATNM-NEXT-DUE-DATE > 0 AND ATNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TR' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TR'
                   IF ATNM-NEXT-DUE-DATE > 0 AND ATNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF ATNM-NEXT-DUE-DATE > 0 AND ATNM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DISPENSED AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3240-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-REGION-CD
                   ADD ATNT-DISPENSED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-DISPENSED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ADJUST YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-ADJUST.
           IF ATNM-ADJUST-YTD < 50.00
               MOVE 50.00 TO ATNM-ADJUST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-ADJUST-YTD > 5000.00
               MOVE 5000.00 TO ATNM-ADJUST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE NETWORK CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3260-DESCRIBE-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'C'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SURCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       3270-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-SETTLE-AMT ROUNDED =
                   ATNM-SURCHG-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-SETTLE-AMT
           END-IF
           ADD ATNM-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE SURCHG YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASSETTE YTD.
      *----------------------------------------------------------------*
       3280-PRORATE-CASSETTE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-CASSETTE-YTD ROUNDED =
                   ATNM-SURCHG-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-CASSETTE-YTD
           END-IF
           ADD ATNM-CASSETTE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR NETWORK CD:
      * A -> C WHEN NEXT REVIEW DATE IS PAST
      * C -> H WHEN NEXT REVIEW DATE IS PAST
      * H -> R WHEN NEXT REVIEW DATE IS PAST
      * R -> S WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3290-SET-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'C' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'C'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'H' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ATNM-NEXT-REVIEW-DATE > 0 AND
                       ATNM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'S' TO ATNM-NETWORK-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 12
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3310-ACCRUE-SETTLE.
           IF ATNM-SETTLE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-SETTLE-YTD * ATNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ATNM-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3320-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF ATNT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E104 NAME REQUIRED
      * E310 SURCHG AMT NEGATIVE
      * E687 SURCHG AMT OVER 250000
      * E471 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3330-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-NAME = SPACES
                   MOVE 'E104' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-AMT < 0
                   MOVE 'E310' TO WS-REJECT-CODE
               WHEN ATNM-SURCHG-AMT > 250000.00
                   MOVE 'E687' TO WS-REJECT-CODE
               WHEN ATNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E471' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3340-FLAG-SETTLE.
           IF ATNT-SETTLE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNT-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3350-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ATNT-SURCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-STOP-FLAG = 'Y'
                   COMPUTE ATNT-SURCHG-AMT ROUNDED = WS-WORK-AMT *
                       ATNM-DISCOUNT / 200
               WHEN ATNM-NETWORK-CD = 'A'
                   MOVE 10.00 TO ATNT-SURCHG-AMT
               WHEN ATNM-NETWORK-CD = 'C'
                   MOVE 5.00 TO ATNT-SURCHG-AMT
               WHEN OTHER
                   COMPUTE ATNT-SURCHG-AMT ROUNDED = WS-WORK-AMT *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNT-SURCHG-AMT > 0 AND ATNT-SURCHG-AMT < 10.00
               MOVE 10.00 TO ATNT-SURCHG-AMT
           END-IF
           IF ATNT-SURCHG-AMT > 150.00
               MOVE 150.00 TO ATNT-SURCHG-AMT
           END-IF
           ADD ATNT-SURCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE ADJUST FEE ON THE CASSETTE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3360-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ATNM-ADJUST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   COMPUTE ATNM-ADJUST-AMT ROUNDED = ATNM-CASSETTE-YTD *
                       ATNM-DISCOUNT / 200
               WHEN ATNT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO ATNM-ADJUST-AMT
               WHEN ATNT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO ATNM-ADJUST-AMT
               WHEN OTHER
                   COMPUTE ATNM-ADJUST-AMT ROUNDED = ATNM-CASSETTE-YTD *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-ADJUST-AMT > 0 AND ATNM-ADJUST-AMT < 2.00
               MOVE 2.00 TO ATNM-ADJUST-AMT
           END-IF
           IF ATNM-ADJUST-AMT > 50.00
               MOVE 50.00 TO ATNM-ADJUST-AMT
           END-IF
           ADD ATNM-ADJUST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASSETTE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3370-FLAG-CASSETTE.
           IF ATNM-CASSETTE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNM-CASSETTE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3380-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ATNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATNM-SURCHG-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADJUST GRID TIER FOR THE SURCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3390-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ADJUST-GRID-RATE(1) TO ATNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ATNM-SURCHG-YTD NOT < WS-ADJUST-GRID-LOW(WS-TX)
                   MOVE WS-ADJUST-GRID-RATE(WS-TX) TO ATNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTERCHG AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3400-FLAG-INTERCHG.
           IF ATNM-INTERCHG-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-INTERCHG-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3410-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-TERMINAL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASSETTE YTD FEE ON THE SETTLE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3420-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-CASSETTE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-STOP-FLAG = 'Y'
                   COMPUTE ATNM-CASSETTE-YTD ROUNDED = ATNM-SETTLE-YTD *
                       ATNM-RATE / 200
               WHEN ATNM-TERMINAL-CD = 'BJ'
                   MOVE 10.00 TO ATNM-CASSETTE-YTD
               WHEN ATNM-TERMINAL-CD = 'JJ'
                   MOVE 3.00 TO ATNM-CASSETTE-YTD
               WHEN OTHER
                   COMPUTE ATNM-CASSETTE-YTD ROUNDED = ATNM-SETTLE-YTD *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-CASSETTE-YTD > 0 AND ATNM-CASSETTE-YTD < 10.00
               MOVE 10.00 TO ATNM-CASSETTE-YTD
           END-IF
           IF ATNM-CASSETTE-YTD > 50.00
               MOVE 50.00 TO ATNM-CASSETTE-YTD
           END-IF
           ADD ATNM-CASSETTE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SETTLE YTD IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3430-LIMIT-SETTLE.
           IF ATNM-SETTLE-YTD < 50.00
               MOVE 50.00 TO ATNM-SETTLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SETTLE-YTD > 5000.00
               MOVE 5000.00 TO ATNM-SETTLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ATNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3460-COUNT-RETRY.
           IF ATNM-RETRY-CNT < 9999999
               ADD 1 TO ATNM-RETRY-CNT
           END-IF
           IF ATNM-RETRY-CNT NOT < 3
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTERCHG AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3470-LIMIT-INTERCHG.
           IF ATNM-INTERCHG-AMT < 10.00
               MOVE 10.00 TO ATNM-INTERCHG-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-INTERCHG-AMT > 10000.00
               MOVE 10000.00 TO ATNM-INTERCHG-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASSETTE FEE ON THE WORK AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3480-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ATNT-CASSETTE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-STOP-FLAG = 'Y'
                   COMPUTE ATNT-CASSETTE-AMT ROUNDED = WS-WORK-AMT *
                       ATNM-DISCOUNT / 200
               WHEN ATNT-TRAN-CD = 'WD'
                   MOVE 5.00 TO ATNT-CASSETTE-AMT
               WHEN ATNT-TRAN-CD = 'BI'
                   MOVE 15.00 TO ATNT-CASSETTE-AMT
               WHEN OTHER
                   COMPUTE ATNT-CASSETTE-AMT ROUNDED = WS-WORK-AMT *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNT-CASSETTE-AMT > 0 AND ATNT-CASSETTE-AMT < 2.00
               MOVE 2.00 TO ATNT-CASSETTE-AMT
           END-IF
           IF ATNT-CASSETTE-AMT > 75.00
               MOVE 75.00 TO ATNT-CASSETTE-AMT
           END-IF
           ADD ATNT-CASSETTE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG AMT BY TERMINAL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3490-ACCUM-TERMINAL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-TERMINAL-CD
                   ADD ATNT-SURCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-TERMINAL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNT-SURCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3500-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NEXT-REVIEW-DATE > 0 AND ATNM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE AMT BY NETWORK CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-NETWORK.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-NETWORK-CD
                   ADD ATNM-SETTLE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-NETWORK-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-SETTLE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADJUST GRID TIER FOR THE DISPENSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3520-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ADJUST-GRID-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ATNM-DISPENSED-AMT NOT < WS-ADJUST-GRID-LOW(WS-TX)
                   MOVE WS-ADJUST-GRID-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADJUST GRID TIER FOR THE SETTLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3530-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-ADJUST-GRID-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ATNM-SETTLE-YTD NOT < WS-ADJUST-GRID-LOW(WS-TX)
                   MOVE WS-ADJUST-GRID-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-REGION.
           EVALUATE ATNM-REGION-CD
               WHEN 'F'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'G'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'P'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'V'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASSETTE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3550-ACCRUE-CASSETTE.
           IF ATNT-CASSETTE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNT-CASSETTE-AMT * ATNM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3560-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ATNM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3570-FLAG-DISPENSED.
           IF ATNM-DISPENSED-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-DISPENSED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> BI WHEN EFF DATE IS PAST
      * BI -> TR WHEN EFF DATE IS PAST
      * TR -> AD WHEN EFF DATE IS PAST
      * AD -> RV WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3580-SET-TRAN.
           EVALUATE ATNT-TRAN-CD
               WHEN 'WD'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BI' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BI'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TR' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TR'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF ATNT-EFF-DATE > 0 AND ATNT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SURCHG AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3590-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNM-REGION-CD
                   ADD ATNM-SURCHG-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-SURCHG-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADJUST GRID TIER FOR THE DISPENSED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3600-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ADJUST-GRID-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ATNT-DISPENSED-AMT NOT < WS-ADJUST-GRID-LOW(WS-TX)
                   MOVE WS-ADJUST-GRID-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ADJUST AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3610-ACCRUE-ADJUST.
           IF ATNM-ADJUST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-ADJUST-AMT * ATNM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ATNM-SURCHG-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * F -> G WHEN LAST PAY DATE IS PAST
      * G -> H WHEN LAST PAY DATE IS PAST
      * H -> P WHEN LAST PAY DATE IS PAST
      * P -> V WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-REGION.
           EVALUATE ATNM-REGION-CD
               WHEN 'F'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-STATUS.
           EVALUATE ATNM-STATUS-CD
               WHEN 'DF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E912 NAME REQUIRED
      * E109 WORK AMT NEGATIVE
      * E909 WORK AMT OVER 10000
      * E780 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-NAME = SPACES
                   MOVE 'E912' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E909' TO WS-REJECT-CODE
               WHEN ATNT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E780' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURCHG YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-SURCHG.
           IF ATNM-SURCHG-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-SURCHG-YTD * ATNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ATNM-ADJUST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E164 REJECT CD REQUIRED
      * E468 CASSETTE AMT NEGATIVE
      * E424 CASSETTE AMT OVER 25000
      * E972 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-REJECT-CD = SPACES
                   MOVE 'E164' TO WS-REJECT-CODE
               WHEN ATNM-CASSETTE-AMT < 0
                   MOVE 'E468' TO WS-REJECT-CODE
               WHEN ATNM-CASSETTE-AMT > 25000.00
                   MOVE 'E424' TO WS-REJECT-CODE
               WHEN ATNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E972' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ADJUST AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3670-LIMIT-ADJUST.
           IF ATNM-ADJUST-AMT < 25.00
               MOVE 25.00 TO ATNM-ADJUST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-ADJUST-AMT > 50000.00
               MOVE 50000.00 TO ATNM-ADJUST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASSETTE YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3680-LIMIT-CASSETTE.
           IF ATNM-CASSETTE-YTD < 25.00
               MOVE 25.00 TO ATNM-CASSETTE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-CASSETTE-YTD > 10000.00
               MOVE 10000.00 TO ATNM-CASSETTE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADJUST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3690-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATNM-ADJUST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ATNM-ADJUST-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3700-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3710-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF ATNT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3720-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = ATNM-SETTLE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-SETTLE-YTD - (WS-WORK-AMT * 3)
           COMPUTE ATNM-SURCHG-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-STATUS.
           EVALUATE ATNM-STATUS-CD
               WHEN 'DF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 3
               MOVE 'Y' TO ATNM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG BAND TIER FOR THE SETTLE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3750-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-SETTLE-YTD NOT < WS-SURCHG-BAND-LOW(WS-TX)
                   MOVE WS-SURCHG-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3760-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-CLOSE-DATE > 0 AND ATNM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNT-CASSETTE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNT-CASSETTE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNT-CASSETTE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATNT-CASSETTE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNT-CASSETTE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURCHG AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3770-ACCRUE-SURCHG.
           IF ATNM-SURCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-SURCHG-AMT * ATNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ATNT-DISPENSED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASSETTE FEE ON THE SETTLE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3780-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ATNM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ATNM-CASSETTE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   COMPUTE ATNM-CASSETTE-AMT ROUNDED = ATNM-SETTLE-YTD *
                       ATNM-DISCOUNT / 200
               WHEN ATNM-NETWORK-CD = 'A'
                   MOVE 20.00 TO ATNM-CASSETTE-AMT
               WHEN ATNM-NETWORK-CD = 'C'
                   MOVE 12.00 TO ATNM-CASSETTE-AMT
               WHEN OTHER
                   COMPUTE ATNM-CASSETTE-AMT ROUNDED = ATNM-SETTLE-YTD *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-CASSETTE-AMT > 0 AND ATNM-CASSETTE-AMT < 2.00
               MOVE 2.00 TO ATNM-CASSETTE-AMT
           END-IF
           IF ATNM-CASSETTE-AMT > 250.00
               MOVE 250.00 TO ATNM-CASSETTE-AMT
           END-IF
           ADD ATNM-CASSETTE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ADJUST AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3790-FLAG-ADJUST.
           IF ATNM-ADJUST-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNM-ADJUST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3800-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E620 OPERATOR REQUIRED
      * E349 ADJUST AMT NEGATIVE
      * E561 ADJUST AMT OVER 50000
      * E697 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3810-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNT-OPERATOR = SPACES
                   MOVE 'E620' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-AMT < 0
                   MOVE 'E349' TO WS-REJECT-CODE
               WHEN ATNM-ADJUST-AMT > 50000.00
                   MOVE 'E561' TO WS-REJECT-CODE
               WHEN ATNM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E697' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * WD -> BI WHEN EXPIRY DATE IS PAST
      * BI -> TR WHEN EXPIRY DATE IS PAST
      * TR -> AD WHEN EXPIRY DATE IS PAST
      * AD -> RV WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3820-SET-TRAN.
           EVALUATE ATNT-TRAN-CD
               WHEN 'WD'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BI' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BI'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TR' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TR'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO ATNT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SURCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISPENSED YTD.
      *----------------------------------------------------------------*
       3830-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-DISPENSED-YTD ROUNDED =
                   ATNT-SURCHG-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-DISPENSED-YTD
           END-IF
           ADD ATNM-DISPENSED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE NETWORK CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-NETWORK.
           EVALUATE ATNM-NETWORK-CD
               WHEN 'A'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'C'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3850-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = ATNT-SETTLE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ATNT-SETTLE-AMT - (WS-WORK-AMT * 2)
           COMPUTE ATNT-CASSETTE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISPENSED YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3860-ACCRUE-DISPENSED.
           IF ATNM-DISPENSED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-DISPENSED-YTD * ATNM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ATNT-SURCHG-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * F -> G WHEN LAST PAY DATE IS PAST
      * G -> H WHEN LAST PAY DATE IS PAST
      * H -> P WHEN LAST PAY DATE IS PAST
      * P -> V WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-REGION.
           EVALUATE ATNM-REGION-CD
               WHEN 'F'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ATNM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-CHANNEL.
           EVALUATE ATNT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ADJUST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASSETTE AMT.
      *----------------------------------------------------------------*
       3890-PRORATE-CASSETTE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-CASSETTE-AMT ROUNDED =
                   ATNM-ADJUST-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-CASSETTE-AMT
           END-IF
           ADD ATNM-CASSETTE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISPENSED YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-DISPENSED.
           IF ATNM-DISPENSED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-DISPENSED-YTD * ATNM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ATNT-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISPENSED FEE ON THE SURCHG YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3910-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ATNT-DISPENSED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-HOLD-FLAG = 'Y'
                   COMPUTE ATNT-DISPENSED-AMT ROUNDED = ATNM-SURCHG-YTD
                       * ATNM-DISCOUNT / 200
               WHEN ATNT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO ATNT-DISPENSED-AMT
               WHEN ATNT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO ATNT-DISPENSED-AMT
               WHEN OTHER
                   COMPUTE ATNT-DISPENSED-AMT ROUNDED = ATNM-SURCHG-YTD
                       * ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNT-DISPENSED-AMT > 0 AND ATNT-DISPENSED-AMT < 5.00
               MOVE 5.00 TO ATNT-DISPENSED-AMT
           END-IF
           IF ATNT-DISPENSED-AMT > 150.00
               MOVE 150.00 TO ATNT-DISPENSED-AMT
           END-IF
           ADD ATNT-DISPENSED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3920-COUNT-NOTICE.
           IF ATNM-NOTICE-CNT < 9999999
               ADD 1 TO ATNM-NOTICE-CNT
           END-IF
           IF ATNM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E771 REF NO REQUIRED
      * E738 CASSETTE AMT NEGATIVE
      * E077 CASSETTE AMT OVER 99999
      * E938 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3930-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-REF-NO = SPACES
                   MOVE 'E771' TO WS-REJECT-CODE
               WHEN ATNT-CASSETTE-AMT < 0
                   MOVE 'E738' TO WS-REJECT-CODE
               WHEN ATNT-CASSETTE-AMT > 99999.00
                   MOVE 'E077' TO WS-REJECT-CODE
               WHEN ATNM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E938' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AK -> HX WHEN LAST PAY DATE IS PAST
      * HX -> PR WHEN LAST PAY DATE IS PAST
      * PR -> XD WHEN LAST PAY DATE IS PAST
      * XD -> ZJ WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-REASON.
           EVALUATE ATNM-REASON-CD
               WHEN 'AK'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJ' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E240 DESC REQUIRED
      * E943 SURCHG AMT NEGATIVE
      * E774 SURCHG AMT OVER 10000
      * E035 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3950-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-DESC = SPACES
                   MOVE 'E240' TO WS-REJECT-CODE
               WHEN ATNT-SURCHG-AMT < 0
                   MOVE 'E943' TO WS-REJECT-CODE
               WHEN ATNT-SURCHG-AMT > 10000.00
                   MOVE 'E774' TO WS-REJECT-CODE
               WHEN ATNM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E035' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISPENSED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADJUST YTD.
      *----------------------------------------------------------------*
       3960-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-ADJUST-YTD ROUNDED =
                   ATNM-DISPENSED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-ADJUST-YTD
           END-IF
           ADD ATNM-ADJUST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3970-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-EXPIRY-DATE > 0 AND ATNM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-SURCHG-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3980-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AK -> HX WHEN EFFECTIVE DATE IS PAST
      * HX -> PR WHEN EFFECTIVE DATE IS PAST
      * PR -> XD WHEN EFFECTIVE DATE IS PAST
      * XD -> ZJ WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3990-SET-REASON.
           EVALUATE ATNM-REASON-CD
               WHEN 'AK'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJ' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ADJUST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4000-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ATNM-ADJUST-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG FEE ON THE SURCHG YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4010-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ATNT-SURCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   COMPUTE ATNT-SURCHG-AMT ROUNDED = ATNM-SURCHG-YTD *
                       ATNM-DISCOUNT / 200
               WHEN ATNT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO ATNT-SURCHG-AMT
               WHEN ATNT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO ATNT-SURCHG-AMT
               WHEN OTHER
                   COMPUTE ATNT-SURCHG-AMT ROUNDED = ATNM-SURCHG-YTD *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNT-SURCHG-AMT > 0 AND ATNT-SURCHG-AMT < 10.00
               MOVE 10.00 TO ATNT-SURCHG-AMT
           END-IF
           IF ATNT-SURCHG-AMT > 25.00
               MOVE 25.00 TO ATNT-SURCHG-AMT
           END-IF
           ADD ATNT-SURCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AK -> HX WHEN EFFECTIVE DATE IS PAST
      * HX -> PR WHEN EFFECTIVE DATE IS PAST
      * PR -> XD WHEN EFFECTIVE DATE IS PAST
      * XD -> ZJ WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4020-SET-REASON.
           EVALUATE ATNM-REASON-CD
               WHEN 'AK'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HX' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HX'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PR' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PR'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XD'
                   IF ATNM-EFFECTIVE-DATE > 0 AND ATNM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZJ' TO ATNM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE ADJUST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4030-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ATNM-ADJUST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISPENSED YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4040-ACCRUE-DISPENSED.
           IF ATNM-DISPENSED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-DISPENSED-YTD * ATNM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ATNM-ADJUST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4050-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ATNM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ATNM-DISPENSED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4060-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 6
               MOVE 'Y' TO ATNM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E516 CUST ID REQUIRED
      * E526 DISPENSED AMT NEGATIVE
      * E907 DISPENSED AMT OVER 250000
      * E424 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4070-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CUST-ID = SPACES
                   MOVE 'E516' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-AMT < 0
                   MOVE 'E526' TO WS-REJECT-CODE
               WHEN ATNM-DISPENSED-AMT > 250000.00
                   MOVE 'E907' TO WS-REJECT-CODE
               WHEN ATNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E424' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4080-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-LAST-PAY-DATE > 0 AND ATNM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-ADJUST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4090-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-CASSETTE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INTERCHG FEE ON THE SURCHG AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4100-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-INTERCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   COMPUTE ATNM-INTERCHG-AMT ROUNDED = ATNM-SURCHG-AMT *
                       ATNM-FEE-PCT / 200
               WHEN ATNT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO ATNM-INTERCHG-AMT
               WHEN ATNT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO ATNM-INTERCHG-AMT
               WHEN OTHER
                   COMPUTE ATNM-INTERCHG-AMT ROUNDED = ATNM-SURCHG-AMT *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNM-INTERCHG-AMT > 0 AND ATNM-INTERCHG-AMT < 1.00
               MOVE 1.00 TO ATNM-INTERCHG-AMT
           END-IF
           IF ATNM-INTERCHG-AMT > 35.00
               MOVE 35.00 TO ATNM-INTERCHG-AMT
           END-IF
           ADD ATNM-INTERCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE DISPENSED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ADJUST YTD.
      *----------------------------------------------------------------*
       4110-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-ADJUST-YTD ROUNDED =
                   ATNM-DISPENSED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-ADJUST-YTD
           END-IF
           ADD ATNM-ADJUST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4120-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 6
               MOVE 'Y' TO ATNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4130-DESCRIBE-REASON.
           EVALUATE ATNM-REASON-CD
               WHEN 'AK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4140-DESCRIBE-TRAN.
           EVALUATE ATNT-TRAN-CD
               WHEN 'WD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BI'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CASSETTE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4150-SPLIT-CASSETTE.
           COMPUTE WS-WORK-AMT = ATNM-CASSETTE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ATNM-CASSETTE-YTD - (WS-WORK-AMT * 3)
           COMPUTE ATNM-CASSETTE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4160-COUNT-PAYMENT.
           IF ATNM-PAYMENT-CNT < 9999999
               ADD 1 TO ATNM-PAYMENT-CNT
           END-IF
           IF ATNM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO ATNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISPENSED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SURCHG AMT.
      *----------------------------------------------------------------*
       4170-PRORATE-SURCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNT-SURCHG-AMT ROUNDED =
                   ATNM-DISPENSED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNT-SURCHG-AMT
           END-IF
           ADD ATNT-SURCHG-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4180-FLAG-SETTLE.
           IF ATNM-SETTLE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNM-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISPENSED AMT.
      *----------------------------------------------------------------*
       4190-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNT-DISPENSED-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNT-DISPENSED-AMT
           END-IF
           ADD ATNT-DISPENSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE INTERCHG AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4200-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ATNM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ATNT-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-INTERCHG-AMT *
                       ATNM-FEE-PCT / 200
               WHEN ATNM-NETWORK-CD = 'A'
                   MOVE 20.00 TO ATNT-SETTLE-AMT
               WHEN ATNM-NETWORK-CD = 'C'
                   MOVE 20.00 TO ATNT-SETTLE-AMT
               WHEN OTHER
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-INTERCHG-AMT *
                       ATNM-FEE-PCT / 100
           END-EVALUATE
           IF ATNT-SETTLE-AMT > 0 AND ATNT-SETTLE-AMT < 1.00
               MOVE 1.00 TO ATNT-SETTLE-AMT
           END-IF
           IF ATNT-SETTLE-AMT > 150.00
               MOVE 150.00 TO ATNT-SETTLE-AMT
           END-IF
           ADD ATNT-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SURCHG AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4210-FLAG-SURCHG.
           IF ATNT-SURCHG-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ATNT-SURCHG-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASSETTE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4220-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ATNT-CHANNEL-CD
                   ADD ATNM-CASSETTE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ATNT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ATNM-CASSETTE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTERCHG FEE ON THE DISPENSED AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4230-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ATNM-INTERCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-HOLD-FLAG = 'Y'
                   COMPUTE ATNM-INTERCHG-AMT ROUNDED =
                       ATNM-DISPENSED-AMT * ATNM-DISCOUNT / 200
               WHEN ATNT-TRAN-CD = 'WD'
                   MOVE 15.00 TO ATNM-INTERCHG-AMT
               WHEN ATNT-TRAN-CD = 'BI'
                   MOVE 5.00 TO ATNM-INTERCHG-AMT
               WHEN OTHER
                   COMPUTE ATNM-INTERCHG-AMT ROUNDED =
                       ATNM-DISPENSED-AMT * ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-INTERCHG-AMT > 0 AND ATNM-INTERCHG-AMT < 2.00
               MOVE 2.00 TO ATNM-INTERCHG-AMT
           END-IF
           IF ATNM-INTERCHG-AMT > 150.00
               MOVE 150.00 TO ATNM-INTERCHG-AMT
           END-IF
           ADD ATNM-INTERCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4240-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF ATNM-OPEN-DATE > 0 AND ATNM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNT-DISPENSED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4250-COUNT-ITEM.
           IF ATNM-ITEM-CNT < 9999999
               ADD 1 TO ATNM-ITEM-CNT
           END-IF
           IF ATNM-ITEM-CNT NOT < 10
               MOVE 'Y' TO ATNM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISPENSED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4260-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-DISPENSED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SURCHG YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4270-LIMIT-SURCHG.
           IF ATNM-SURCHG-YTD < 25.00
               MOVE 25.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-YTD > 500.00
               MOVE 500.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISPENSED YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4280-FLAG-DISPENSED.
           IF ATNM-DISPENSED-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ATNM-DISPENSED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADJUST YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4290-SPLIT-ADJUST.
           COMPUTE WS-WORK-AMT = ATNM-ADJUST-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ATNM-ADJUST-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4300-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-MATURITY-DATE > 0 AND ATNM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-DISPENSED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG BAND TIER FOR THE SURCHG AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4310-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-BAND-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-SURCHG-AMT NOT < WS-SURCHG-BAND-LOW(WS-TX)
                   MOVE WS-SURCHG-BAND-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURCHG FEE ON THE CASSETTE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4320-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SURCHG-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ATNM-SURCHG-AMT ROUNDED = ATNM-CASSETTE-AMT *
                       ATNM-DISCOUNT / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 7.00 TO ATNM-SURCHG-AMT
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 12.00 TO ATNM-SURCHG-AMT
               WHEN OTHER
                   COMPUTE ATNM-SURCHG-AMT ROUNDED = ATNM-CASSETTE-AMT *
                       ATNM-DISCOUNT / 100
           END-EVALUATE
           IF ATNM-SURCHG-AMT > 0 AND ATNM-SURCHG-AMT < 2.00
               MOVE 2.00 TO ATNM-SURCHG-AMT
           END-IF
           IF ATNM-SURCHG-AMT > 250.00
               MOVE 250.00 TO ATNM-SURCHG-AMT
           END-IF
           ADD ATNM-SURCHG-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SURCHG YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4330-LIMIT-SURCHG.
           IF ATNM-SURCHG-YTD < 50.00
               MOVE 50.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ATNM-SURCHG-YTD > 1000.00
               MOVE 1000.00 TO ATNM-SURCHG-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TERMINAL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4340-DESCRIBE-TERMINAL.
           EVALUATE ATNM-TERMINAL-CD
               WHEN 'BJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4350-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ATNM-NOTICE-DATE > 0 AND ATNM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ATNM-INTERCHG-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4360-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF ATNT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG BANDX TIER FOR THE SURCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4370-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-BANDX-RATE(1) TO ATNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ATNM-SURCHG-YTD NOT < WS-SURCHG-BANDX-LOW(WS-TX)
                   MOVE WS-SURCHG-BANDX-RATE(WS-TX) TO ATNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADJUST GRID TIER FOR THE DISPENSED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4380-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-ADJUST-GRID-RATE(1) TO ATNM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ATNM-DISPENSED-YTD NOT < WS-ADJUST-GRID-LOW(WS-TX)
                   MOVE WS-ADJUST-GRID-RATE(WS-TX) TO ATNM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISPENSED AMT.
      *----------------------------------------------------------------*
       4390-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNT-DISPENSED-AMT ROUNDED =
                   ATNT-SETTLE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNT-DISPENSED-AMT
           END-IF
           ADD ATNT-DISPENSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG BAND TIER FOR THE INTERCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4400-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNM-INTERCHG-YTD NOT < WS-SURCHG-BAND-LOW(WS-TX)
                   MOVE WS-SURCHG-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E677 CUST ID REQUIRED
      * E078 SETTLE YTD NEGATIVE
      * E322 SETTLE YTD OVER 50000
      * E172 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4410-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CUST-ID = SPACES
                   MOVE 'E677' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-YTD < 0
                   MOVE 'E078' TO WS-REJECT-CODE
               WHEN ATNM-SETTLE-YTD > 50000.00
                   MOVE 'E322' TO WS-REJECT-CODE
               WHEN ATNM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E172' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURCHG BAND TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4420-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURCHG-BAND-RATE(1) TO ATNM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ATNT-SETTLE-AMT NOT < WS-SURCHG-BAND-LOW(WS-TX)
                   MOVE WS-SURCHG-BAND-RATE(WS-TX) TO ATNM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISPENSED AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4430-ACCRUE-DISPENSED.
           IF ATNM-DISPENSED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-DISPENSED-AMT * ATNM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTERCHG AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4440-ACCRUE-INTERCHG.
           IF ATNM-INTERCHG-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-INTERCHG-AMT * ATNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADJUST GRID TIER FOR THE INTERCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4450-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ADJUST-GRID-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ATNM-INTERCHG-YTD NOT < WS-ADJUST-GRID-LOW(WS-TX)
                   MOVE WS-ADJUST-GRID-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4460-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ATNM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ADJUST YTD.
      *----------------------------------------------------------------*
       4470-PRORATE-ADJUST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-ADJUST-YTD ROUNDED =
                   ATNT-SURCHG-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ATNM-ADJUST-YTD
           END-IF
           ADD ATNM-ADJUST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4480-COUNT-NOTICE.
           IF ATNM-NOTICE-CNT < 9999999
               ADD 1 TO ATNM-NOTICE-CNT
           END-IF
           IF ATNM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO ATNM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4490-COUNT-SEQ.
           IF ATNT-SEQ < 9999999
               ADD 1 TO ATNT-SEQ
           END-IF
           IF ATNT-SEQ NOT < 5
               MOVE 'Y' TO ATNM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURCHG AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISPENSED AMT.
      *----------------------------------------------------------------*
       4500-PRORATE-DISPENSED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-DISPENSED-AMT ROUNDED =
                   ATNT-SURCHG-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-DISPENSED-AMT
           END-IF
           ADD ATNM-DISPENSED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4510-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ATNM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ATNM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE SETTLE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4520-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ATNM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ATNT-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-STOP-FLAG = 'Y'
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-SETTLE-YTD *
                       ATNM-RATE / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 5.00 TO ATNT-SETTLE-AMT
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 3.00 TO ATNT-SETTLE-AMT
               WHEN OTHER
                   COMPUTE ATNT-SETTLE-AMT ROUNDED = ATNM-SETTLE-YTD *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNT-SETTLE-AMT > 0 AND ATNT-SETTLE-AMT < 10.00
               MOVE 10.00 TO ATNT-SETTLE-AMT
           END-IF
           IF ATNT-SETTLE-AMT > 25.00
               MOVE 25.00 TO ATNT-SETTLE-AMT
           END-IF
           ADD ATNT-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4530-COUNT-RETRY.
           IF ATNM-RETRY-CNT < 9999999
               ADD 1 TO ATNM-RETRY-CNT
           END-IF
           IF ATNM-RETRY-CNT NOT < 6
               MOVE 'Y' TO ATNT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4540-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ATNM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SETTLE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNM-REVIEW-FLAG = 'Y'
                   COMPUTE ATNM-SETTLE-YTD ROUNDED = WS-WORK-AMT-2 *
                       ATNM-RATE / 200
               WHEN ATNM-BRANCH-CD = 'DU'
                   MOVE 20.00 TO ATNM-SETTLE-YTD
               WHEN ATNM-BRANCH-CD = 'HX'
                   MOVE 7.00 TO ATNM-SETTLE-YTD
               WHEN OTHER
                   COMPUTE ATNM-SETTLE-YTD ROUNDED = WS-WORK-AMT-2 *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-SETTLE-YTD > 0 AND ATNM-SETTLE-YTD < 2.00
               MOVE 2.00 TO ATNM-SETTLE-YTD
           END-IF
           IF ATNM-SETTLE-YTD > 100.00
               MOVE 100.00 TO ATNM-SETTLE-YTD
           END-IF
           ADD ATNM-SETTLE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE INTERCHG TIER TIER FOR THE SURCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4550-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INTERCHG-TIER-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ATNM-SURCHG-YTD NOT < WS-INTERCHG-TIER-LOW(WS-TX)
                   MOVE WS-INTERCHG-TIER-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASSETTE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4560-ACCRUE-CASSETTE.
           IF ATNM-CASSETTE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ATNM-CASSETTE-AMT * ATNM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ATNT-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE ADJUST GRID TIER FOR THE DISPENSED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4570-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-ADJUST-GRID-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ATNM-DISPENSED-YTD NOT < WS-ADJUST-GRID-LOW(WS-TX)
                   MOVE WS-ADJUST-GRID-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SURCHG AMT.
      *----------------------------------------------------------------*
       4580-PRORATE-SURCHG.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ATNM-SURCHG-AMT ROUNDED =
                   ATNM-SETTLE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ATNM-SURCHG-AMT
           END-IF
           ADD ATNM-SURCHG-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE INTERCHG TIER TIER FOR THE SURCHG YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4590-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INTERCHG-TIER-RATE(1) TO ATNM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ATNM-SURCHG-YTD NOT < WS-INTERCHG-TIER-LOW(WS-TX)
                   MOVE WS-INTERCHG-TIER-RATE(WS-TX) TO ATNM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ATNM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ATNM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SETTLE FEE ON THE SURCHG AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4600-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ATNM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ATNM-SETTLE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ATNT-REVERSAL-FLAG = 'Y'
                   COMPUTE ATNM-SETTLE-AMT ROUNDED = ATNT-SURCHG-AMT *
                       ATNM-RATE / 200
               WHEN ATNM-STATUS-CD = 'DF'
                   MOVE 10.00 TO ATNM-SETTLE-AMT
               WHEN ATNM-STATUS-CD = 'DP'
                   MOVE 10.00 TO ATNM-SETTLE-AMT
               WHEN OTHER
                   COMPUTE ATNM-SETTLE-AMT ROUNDED = ATNT-SURCHG-AMT *
                       ATNM-RATE / 100
           END-EVALUATE
           IF ATNM-SETTLE-AMT > 0 AND ATNM-SETTLE-AMT < 10.00
               MOVE 10.00 TO ATNM-SETTLE-AMT
           END-IF
           IF ATNM-SETTLE-AMT > 75.00
               MOVE 75.00 TO ATNM-SETTLE-AMT
           END-IF
           ADD ATNM-SETTLE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E740 CONTACT REQUIRED
      * E049 DISPENSED AMT NEGATIVE
      * E642 DISPENSED AMT OVER 1000000
      * E695 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4610-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ATNM-CONTACT = SPACES
                   MOVE 'E740' TO WS-REJECT-CODE
               WHEN ATNT-DISPENSED-AMT < 0
                   MOVE 'E049' TO WS-REJECT-CODE
               WHEN ATNT-DISPENSED-AMT > 1000000.00
                   MOVE 'E642' TO WS-REJECT-CODE
               WHEN ATNM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E695' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE ATN FIELDS TO ATVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-ATVAL.
           INITIALIZE ATVAL-PARMS
           MOVE ATNM-ADJUST-AMT
             TO ATVAL-ADJUST-AMT
           MOVE ATNM-INTERCHG-YTD
             TO ATVAL-INTERCHG-YTD
           MOVE ATNM-CASSETTE-AMT
             TO ATVAL-CASSETTE-AMT
           MOVE ATNM-SETTLE-YTD
             TO ATVAL-SETTLE-YTD
           MOVE ATNM-SURCHG-AMT
             TO ATVAL-SURCHG-AMT
           MOVE ATNM-SURCHG-YTD
             TO ATVAL-SURCHG-YTD
           MOVE ATNM-NOTICE-DATE
             TO ATVAL-NOTICE-DATE
           MOVE ATNM-NEXT-REVIEW-DATE
             TO ATVAL-NEXT-REVIEW-DATE
           MOVE ATNM-MATURITY-DATE
             TO ATVAL-MATURITY-DATE
           MOVE ATNM-OPEN-DATE
             TO ATVAL-OPEN-DATE
           MOVE ATNM-TERMINAL-CD
             TO ATVAL-TERMINAL-CD
           MOVE ATNM-BRANCH-CD
             TO ATVAL-BRANCH-CD
           MOVE ATNM-STATUS-CD
             TO ATVAL-STATUS-CD
           MOVE ATNM-REASON-CD
             TO ATVAL-REASON-CD
           MOVE ATNM-SUSPEND-FLAG
             TO ATVAL-SUSPEND-FLAG
           MOVE ATNM-EXEMPT-FLAG
             TO ATVAL-EXEMPT-FLAG
           MOVE ATNM-REVIEW-FLAG
             TO ATVAL-REVIEW-FLAG
           MOVE ATNM-NOTICE-CNT
             TO ATVAL-NOTICE-CNT
           MOVE ATNM-PAYMENT-CNT
             TO ATVAL-PAYMENT-CNT
           MOVE ATNM-DISCOUNT
             TO ATVAL-DISCOUNT
           MOVE ATNM-RATE
             TO ATVAL-RATE
           MOVE ATNM-CONTACT
             TO ATVAL-CONTACT
           MOVE ATNM-CUST-ID
             TO ATVAL-CUST-ID
           CALL 'ATVAL' USING ATVAL-PARMS
           EVALUATE ATVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ATVAL-ADJUST-AMT
                     TO ATNM-ADJUST-AMT
                   MOVE ATVAL-SETTLE-YTD
                     TO ATNM-SETTLE-YTD
                   MOVE ATVAL-SURCHG-AMT
                     TO ATNM-SURCHG-AMT
                   MOVE ATVAL-REVIEW-FLAG
                     TO ATNM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-ATVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE ATNT-KEY TO RD-KEY
           MOVE ATNT-TRAN-CD TO RD-CODE
           MOVE ATNT-SURCHG-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE ATNT-KEY TO RD-KEY
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
           DISPLAY 'ATNPOST READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ATNPOST' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
