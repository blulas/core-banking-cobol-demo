      *================================================================*
      * PROGRAM  : TXXEDIT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TELEX WIRE MESSAGING - INPUT EDIT. READS THE TLX    *
      *            TRANSACTIONS, UPDATES THE TLX MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TESTED TELEX FUNDS TRANSFER     *
      *            MESSAGES AND TEST KEY VALIDATION                    *
      * INPUT    : TLXMST (KSDS), TLXTRN, BANKCTL                      *
      * OUTPUT   : TLXOUT, TXXEDRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TXFEE, TXELG, TXPRC     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-06-23 DPS  ORIGINAL                                       *
      * 1996-04-17 BHS  NEW SETTLE REPORT COLUMNS CHG8812              *
      * 1998-11-15 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-11-08 TWH  PERFORMANCE - CHARGE READ AHEAD REQ1541        *
      * 1999-12-23 MKL  CORRESP TABLE EXPANDED PRB0736                 *
      * 1999-12-03 DPS  NEW CORRESP REPORT COLUMNS PRB6679             *
      * 1999-03-07 EWS  CHARGE ROUNDING - HALF UP REQ8911              *
      * 2000-10-05 PQR  WIRE CUTOFF MOVED PRB3438                      *
      * 2001-09-20 TWH  WIRE EXCEPTIONS TO OPS QUEUE INC0397           *
      * 2002-07-22 LMB  QUEUED RERUN PROTECTION REQ3568                *
      * 2003-11-02 MKL  WIRE STATUS CODES ADDED CHG4561                *
      * 2005-01-19 GAF  NEW COVER REPORT COLUMNS INC8273               *
      * 2010-11-09 MKL  COVER NOTICE TEXT PER LEGAL INC3520            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXXEDIT.
       AUTHOR. D P SHAW.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TLXMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TLXM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TLXTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TLXOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TXXEDRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TLXMST.
       FD  TRAN-FILE.
       COPY TLXTRN.
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
                                           VALUE 13.000000.
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
      * WS-CORRESP-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CORRESP-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000222500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000709500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000780000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000609500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002950000000741000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012950000000139000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013050000000783000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013150000000761000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013200000000637000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013450000000841000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015950000000757000'.
       01  WS-CORRESP-TIER-TBL REDEFINES WS-CORRESP-TIER-VALUES.
           05  WS-CORRESP-TIER-ROW OCCURS 11 TIMES.
               10  WS-CORRESP-TIER-LOW     PIC 9(09)V99.
               10  WS-CORRESP-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COVER-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COVER-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000016000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000512500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000587000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000865000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005350000000839500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010350000000639500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012850000000281000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013100000000690500'.
       01  WS-COVER-SCHED-TBL REDEFINES WS-COVER-SCHED-VALUES.
           05  WS-COVER-SCHED-ROW OCCURS 8 TIMES.
               10  WS-COVER-SCHED-LOW      PIC 9(09)V99.
               10  WS-COVER-SCHED-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SETTLE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SETTLE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000529500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000823000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000831000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000255500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000537000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014000000000464000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024000000000235000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026500000000525500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026550000000690500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026650000000827500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031650000000475500'.
       01  WS-SETTLE-BAND-TBL REDEFINES WS-SETTLE-BAND-VALUES.
           05  WS-SETTLE-BAND-ROW OCCURS 11 TIMES.
               10  WS-SETTLE-BAND-LOW      PIC 9(09)V99.
               10  WS-SETTLE-BAND-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-WIRE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-WIRE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000673500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000288500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000695500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000800000000103000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001300000000645000'.
       01  WS-WIRE-TIER-TBL REDEFINES WS-WIRE-TIER-VALUES.
           05  WS-WIRE-TIER-ROW OCCURS 5 TIMES.
               10  WS-WIRE-TIER-LOW        PIC 9(09)V99.
               10  WS-WIRE-TIER-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TXXEDIT  TELEX WIRE MESSAGING'.
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
       COPY TXFEEP.
       COPY TXELGP.
       COPY TXPRCP.

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
               MOVE 'TLXMST' TO ABND-FILE-NAME
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
           MOVE TLXT-KEY TO TLXM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ACCUM-TRAN
               PERFORM 3010-FLAG-SETTLE
               PERFORM 3020-LIMIT-CORRESP
               PERFORM 3030-ACCRUE-DISCOUNT
               PERFORM 3040-ASSESS-STOP
               PERFORM 3050-ASSESS-NOTICE-SENT
               PERFORM 3060-PRORATE-SETTLE
               PERFORM 3070-ACCRUE-WIRE
               PERFORM 3080-DESCRIBE-TYPE
               PERFORM 3090-SELECT-RATE
               PERFORM 3100-ASSESS-STOP
               PERFORM 3110-AGE-EFFECTIVE
               PERFORM 3120-CHECK-NEXT-REVIEW
               PERFORM 3130-EDIT-REF-NO
               PERFORM 3140-SELECT-FEE-PCT
               PERFORM 3150-ACCRUE-CHARGE
               PERFORM 3160-SPLIT-CORRESP
               PERFORM 3170-SET-BRANCH
               PERFORM 3180-LIMIT-SETTLE
               PERFORM 3190-CHECK-OPEN
               PERFORM 3200-AGE-NEXT-REVIEW
               PERFORM 3210-COUNT-NOTICE
               PERFORM 3220-SET-TYPE
               PERFORM 3230-AGE-EXPIRY
               PERFORM 3240-EDIT-CONTACT
               PERFORM 3250-LIMIT-SETTLE
               PERFORM 3260-SPLIT-SETTLE
               PERFORM 3270-AGE-NEXT-DUE
               PERFORM 3280-FLAG-SETTLE
               PERFORM 3290-SET-MSG-TYPE
               PERFORM 3300-PRORATE-QUEUED
               PERFORM 3310-SPLIT-CHARGE
               PERFORM 3320-ASSESS-OVERRIDE
               PERFORM 3330-CHECK-LAST-ACT
               PERFORM 3340-SET-REASON
               PERFORM 3350-COUNT-ITEM
               PERFORM 3360-ACCRUE-WIRE
               PERFORM 3370-SELECT-DISCOUNT
               PERFORM 3380-ACCRUE-SETTLE
               PERFORM 3390-ACCUM-REGION
               PERFORM 3400-AGE-NOTICE
               PERFORM 3410-SPLIT-CHARGE
               PERFORM 3420-ACCUM-CHANNEL
               PERFORM 3430-ACCRUE-QUEUED
               PERFORM 3440-PRORATE-QUEUED
               PERFORM 3450-EDIT-MEMO
               PERFORM 3460-DESCRIBE-REASON
               PERFORM 3470-ACCRUE-QUEUED
               PERFORM 3480-COUNT-ITEM
               PERFORM 3490-FLAG-CORRESP
               PERFORM 3500-DESCRIBE-TYPE
               PERFORM 3510-ACCUM-REGION
               PERFORM 3520-SPLIT-CHARGE
               PERFORM 3530-LIMIT-CORRESP
               PERFORM 3540-ACCRUE-COVER
               PERFORM 3550-LIMIT-QUEUED
               PERFORM 3560-SELECT-FEE-PCT
               PERFORM 3570-CHECK-POST
               PERFORM 3580-ASSESS-REVERSAL
               PERFORM 3590-FLAG-SETTLE
               PERFORM 3600-ASSESS-NOTICE-SENT
               PERFORM 3610-ACCUM-TYPE
               PERFORM 3620-SPLIT-CORRESP
               PERFORM 3630-ACCUM-CHANNEL
               PERFORM 3640-COUNT-ITEM
               PERFORM 3650-LIMIT-CORRESP
               PERFORM 3660-LIMIT-CORRESP
               PERFORM 3670-COUNT-PAYMENT
               PERFORM 3680-SELECT-FEE-PCT
               PERFORM 3690-FLAG-QUEUED
               PERFORM 3700-SET-BRANCH
               PERFORM 3710-AGE-EFFECTIVE
               PERFORM 3720-CHECK-NOTICE
               PERFORM 3730-ACCRUE-COVER
               PERFORM 3740-SET-TYPE
               PERFORM 3750-EDIT-ADDR
               PERFORM 3760-LIMIT-ITEM
               PERFORM 3770-LIMIT-CHARGE
               PERFORM 3780-PRORATE-QUEUED
               PERFORM 3790-ACCRUE-RATE
               PERFORM 3800-CHECK-EFFECTIVE
               PERFORM 3810-PRORATE-COVER
               PERFORM 3820-ACCUM-BRANCH
               PERFORM 3830-FLAG-QUEUED
               PERFORM 3840-LIMIT-WIRE
               PERFORM 3850-LIMIT-CORRESP
               PERFORM 3860-LIMIT-QUEUED
               PERFORM 3870-AGE-POST
               PERFORM 3880-SET-TYPE
               PERFORM 3890-SPLIT-SETTLE
               PERFORM 3900-SELECT-DISCOUNT
               PERFORM 3910-AGE-EFFECTIVE
               PERFORM 3920-EDIT-REF-NO
               PERFORM 3930-SELECT-FEE-PCT
               PERFORM 3940-DESCRIBE-TRAN
               PERFORM 3950-ASSESS-OVERRIDE
               PERFORM 3960-SET-CHANNEL
               PERFORM 3970-SELECT-DISCOUNT
               PERFORM 3980-FLAG-CHARGE
               PERFORM 3990-SELECT-FEE-PCT
               PERFORM 4000-PRORATE-QUEUED
               PERFORM 4010-ASSESS-NOTICE-SENT
               PERFORM 4020-EDIT-CUST-ID
               PERFORM 4030-SELECT-RATE
               PERFORM 4040-PRORATE-QUEUED
               PERFORM 4050-AGE-NEXT-DUE
               PERFORM 4060-EDIT-CONTACT
               PERFORM 4070-SELECT-DISCOUNT
               PERFORM 4080-ACCUM-CHANNEL
               PERFORM 4090-ASSESS-STOP
               PERFORM 4100-ACCRUE-COVER
               PERFORM 4110-SPLIT-SETTLE
               PERFORM 4120-SPLIT-COVER
               PERFORM 4130-AGE-EFFECTIVE
               PERFORM 4140-SELECT-FEE-PCT
               PERFORM 4150-ASSESS-REVERSAL
               PERFORM 4160-FLAG-ITEM
               PERFORM 4170-ACCRUE-FEE-PCT
               PERFORM 4180-AGE-EFFECTIVE
               PERFORM 4190-ASSESS-OVERRIDE
               PERFORM 4200-ACCUM-TEST-KEY
               PERFORM 4210-SET-TYPE
               PERFORM 4220-SPLIT-SETTLE
               PERFORM 4230-SELECT-FEE-PCT
               PERFORM 4240-PRORATE-CHARGE
               PERFORM 4250-ACCRUE-QUEUED
               PERFORM 4260-LIMIT-QUEUED
               PERFORM 4270-SET-BRANCH
               PERFORM 4280-ACCUM-REASON
               PERFORM 4290-SET-REGION
               PERFORM 4300-SET-REGION
               PERFORM 4310-EDIT-CONTACT
               PERFORM 4320-ACCUM-TYPE
               PERFORM 4330-CHECK-LAST-ACT
               PERFORM 4340-DESCRIBE-TRAN
               PERFORM 4350-AGE-LAST-ACT
               PERFORM 4360-PRORATE-WIRE
               PERFORM 4370-SELECT-FEE-PCT
               PERFORM 4380-ACCUM-TRAN
               PERFORM 4390-AGE-EFFECTIVE
               PERFORM 4400-EDIT-OPERATOR
               PERFORM 4410-ACCRUE-WIRE
               PERFORM 4420-FLAG-SETTLE
               PERFORM 4430-PRORATE-SETTLE
               PERFORM 4440-SELECT-FEE-PCT
               PERFORM 4450-LIMIT-QUEUED
               PERFORM 4460-SPLIT-QUEUED
               PERFORM 4470-EDIT-REF-NO
               PERFORM 4480-CHECK-POST
               PERFORM 4490-ASSESS-NOTICE-SENT
               PERFORM 4500-SELECT-RATE
               PERFORM 4510-ACCRUE-CORRESP
               PERFORM 4520-SET-TYPE
               PERFORM 4530-COUNT-NOTICE
               PERFORM 4540-EDIT-NAME
               PERFORM 4550-SET-REASON
               PERFORM 4560-LIMIT-SETTLE
               PERFORM 4570-AGE-EFF
               PERFORM 4580-EDIT-MEMO
               PERFORM 4590-CHECK-EFF
               PERFORM 4600-AGE-LAST-ACT
               PERFORM 4610-EDIT-OPERATOR
               PERFORM 4620-CHECK-CLOSE
               PERFORM 4630-ACCRUE-CORRESP
               PERFORM 4640-PRORATE-SETTLE
               PERFORM 4650-SPLIT-WIRE
               PERFORM 4660-FLAG-CHARGE
               PERFORM 4670-ACCRUE-CHARGE
               PERFORM 4680-CHECK-OPEN
               PERFORM 4690-SET-TYPE
               PERFORM 4700-ASSESS-REVERSAL
               PERFORM 4710-LIMIT-CHARGE
               PERFORM 4720-ASSESS-STOP
               PERFORM 4730-SELECT-FEE-PCT
               PERFORM 4740-SET-MSG-TYPE
               PERFORM 4750-EDIT-REJECT
               PERFORM 4760-SELECT-DISCOUNT
               PERFORM 4770-FLAG-QUEUED
               PERFORM 4780-ASSESS-OVERRIDE
               PERFORM 4790-CHECK-NEXT-REVIEW
               PERFORM 4800-SET-TRAN
               PERFORM 4810-LIMIT-WIRE
               PERFORM 4820-ASSESS-REVIEW
               PERFORM 7010-CALL-TXFEE
               PERFORM 7000-CALL-TXELG
               PERFORM 7020-CALL-TXPRC
               REWRITE TLX-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TLXMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TLX-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ACCUMULATE QUEUED AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-TRAN-CD
                   ADD TLXM-QUEUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-QUEUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3010-FLAG-SETTLE.
           IF TLXM-SETTLE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-SETTLE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3020-LIMIT-CORRESP.
           IF TLXM-CORRESP-YTD < 100.00
               MOVE 100.00 TO TLXM-CORRESP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CORRESP-YTD > 5000.00
               MOVE 5000.00 TO TLXM-CORRESP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TLXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TLXM-SETTLE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE QUEUED YTD FEE ON THE WIRE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3040-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TLXM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TLXM-QUEUED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TLXM-QUEUED-YTD ROUNDED = TLXT-WIRE-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXM-TYPE-CD = 'K'
                   MOVE 12.00 TO TLXM-QUEUED-YTD
               WHEN TLXM-TYPE-CD = 'L'
                   MOVE 3.00 TO TLXM-QUEUED-YTD
               WHEN OTHER
                   COMPUTE TLXM-QUEUED-YTD ROUNDED = TLXT-WIRE-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXM-QUEUED-YTD > 0 AND TLXM-QUEUED-YTD < 2.00
               MOVE 2.00 TO TLXM-QUEUED-YTD
           END-IF
           IF TLXM-QUEUED-YTD > 100.00
               MOVE 100.00 TO TLXM-QUEUED-YTD
           END-IF
           ADD TLXM-QUEUED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGE YTD FEE ON THE COVER AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3050-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TLXM-CHARGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TLXM-CHARGE-YTD ROUNDED = TLXT-COVER-AMT *
                       TLXM-RATE / 200
               WHEN TLXT-TRAN-CD = 'IN'
                   MOVE 7.00 TO TLXM-CHARGE-YTD
               WHEN TLXT-TRAN-CD = 'OT'
                   MOVE 15.00 TO TLXM-CHARGE-YTD
               WHEN OTHER
                   COMPUTE TLXM-CHARGE-YTD ROUNDED = TLXT-COVER-AMT *
                       TLXM-RATE / 100
           END-EVALUATE
           IF TLXM-CHARGE-YTD > 0 AND TLXM-CHARGE-YTD < 2.00
               MOVE 2.00 TO TLXM-CHARGE-YTD
           END-IF
           IF TLXM-CHARGE-YTD > 100.00
               MOVE 100.00 TO TLXM-CHARGE-YTD
           END-IF
           ADD TLXM-CHARGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE CHARGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       3060-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-SETTLE-AMT ROUNDED =
                   TLXM-CHARGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-SETTLE-AMT
           END-IF
           ADD TLXM-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3070-ACCRUE-WIRE.
           IF TLXT-WIRE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXT-WIRE-AMT * TLXM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLXM-WIRE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3080-DESCRIBE-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'M'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'P'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'W'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP TIER TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-TIER-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TLXT-QUEUED-AMT NOT < WS-CORRESP-TIER-LOW(WS-TX)
                   MOVE WS-CORRESP-TIER-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE QUEUED FEE ON THE SETTLE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3100-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TLXM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TLXT-QUEUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   COMPUTE TLXT-QUEUED-AMT ROUNDED = TLXT-SETTLE-AMT *
                       TLXM-RATE / 200
               WHEN TLXT-TRAN-CD = 'IN'
                   MOVE 7.00 TO TLXT-QUEUED-AMT
               WHEN TLXT-TRAN-CD = 'OT'
                   MOVE 3.00 TO TLXT-QUEUED-AMT
               WHEN OTHER
                   COMPUTE TLXT-QUEUED-AMT ROUNDED = TLXT-SETTLE-AMT *
                       TLXM-RATE / 100
           END-EVALUATE
           IF TLXT-QUEUED-AMT > 0 AND TLXT-QUEUED-AMT < 5.00
               MOVE 5.00 TO TLXT-QUEUED-AMT
           END-IF
           IF TLXT-QUEUED-AMT > 100.00
               MOVE 100.00 TO TLXT-QUEUED-AMT
           END-IF
           ADD TLXT-QUEUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3110-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EFFECTIVE-DATE > 0 AND TLXM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-CORRESP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-CORRESP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-CORRESP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-CORRESP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CORRESP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3120-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TLXT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E354 REF NO REQUIRED
      * E432 WORK AMT 2 NEGATIVE
      * E551 WORK AMT 2 OVER 1000000
      * E581 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3130-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-REF-NO = SPACES
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E432' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E551' TO WS-REJECT-CODE
               WHEN TLXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E581' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3140-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-TIER-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-CORRESP-TIER-LOW(WS-TX)
                   MOVE WS-CORRESP-TIER-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3150-ACCRUE-CHARGE.
           IF TLXM-CHARGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CHARGE-YTD * TLXM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLXM-CHARGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CORRESP AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3160-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = TLXM-CORRESP-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLXM-CORRESP-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLXM-COVER-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN EFF DATE IS PAST
      * KR -> LD WHEN EFF DATE IS PAST
      * LD -> LS WHEN EFF DATE IS PAST
      * LS -> XL WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3170-SET-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SETTLE YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3180-LIMIT-SETTLE.
           IF TLXM-SETTLE-YTD < 25.00
               MOVE 25.00 TO TLXM-SETTLE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-SETTLE-YTD > 2500.00
               MOVE 2500.00 TO TLXM-SETTLE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLXM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TLXM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-REVIEW-DATE > 0 AND TLXM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-QUEUED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-QUEUED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-QUEUED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-QUEUED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-QUEUED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-NOTICE.
           IF TLXM-NOTICE-CNT < 9999999
               ADD 1 TO TLXM-NOTICE-CNT
           END-IF
           IF TLXM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN POST DATE IS PAST
      * L -> M WHEN POST DATE IS PAST
      * M -> P WHEN POST DATE IS PAST
      * P -> W WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3220-SET-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3230-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EXPIRY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E451 CONTACT REQUIRED
      * E069 SETTLE YTD NEGATIVE
      * E910 SETTLE YTD OVER 99999
      * E239 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CONTACT = SPACES
                   MOVE 'E451' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-YTD < 0
                   MOVE 'E069' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-YTD > 99999.00
                   MOVE 'E910' TO WS-REJECT-CODE
               WHEN TLXM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E239' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3250-LIMIT-SETTLE.
           IF TLXM-SETTLE-AMT < 100.00
               MOVE 100.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-SETTLE-AMT > 500.00
               MOVE 500.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3260-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TLXT-SETTLE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TLXT-SETTLE-AMT - (WS-WORK-AMT * 12)
           COMPUTE TLXT-WIRE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3270-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3280-FLAG-SETTLE.
           IF TLXM-SETTLE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MSG TYPE CD:
      * LG -> LX WHEN LAST PAY DATE IS PAST
      * LX -> ND WHEN LAST PAY DATE IS PAST
      * ND -> PT WHEN LAST PAY DATE IS PAST
      * PT -> LG WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3290-SET-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LX' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LX'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LG' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CHARGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO QUEUED AMT.
      *----------------------------------------------------------------*
       3300-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXT-QUEUED-AMT ROUNDED =
                   TLXM-CHARGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXT-QUEUED-AMT
           END-IF
           ADD TLXT-QUEUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CHARGE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = TLXM-CHARGE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TLXM-CHARGE-YTD - (WS-WORK-AMT * 3)
           COMPUTE TLXM-SETTLE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COVER YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3320-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TLXM-COVER-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   COMPUTE TLXM-COVER-YTD ROUNDED = WS-WORK-AMT-2 *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-BRANCH-CD = 'CC'
                   MOVE 5.00 TO TLXM-COVER-YTD
               WHEN TLXM-BRANCH-CD = 'KR'
                   MOVE 15.00 TO TLXM-COVER-YTD
               WHEN OTHER
                   COMPUTE TLXM-COVER-YTD ROUNDED = WS-WORK-AMT-2 *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXM-COVER-YTD > 0 AND TLXM-COVER-YTD < 5.00
               MOVE 5.00 TO TLXM-COVER-YTD
           END-IF
           IF TLXM-COVER-YTD > 75.00
               MOVE 75.00 TO TLXM-COVER-YTD
           END-IF
           ADD TLXM-COVER-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TLXT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DGN -> JBT WHEN NOTICE DATE IS PAST
      * JBT -> LSS WHEN NOTICE DATE IS PAST
      * LSS -> RDP WHEN NOTICE DATE IS PAST
      * RDP -> RPL WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3340-SET-REASON.
           EVALUATE TLXM-REASON-CD
               WHEN 'DGN'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBT' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBT'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LSS' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LSS'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RDP' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RDP'
                   IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RPL' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3350-COUNT-ITEM.
           IF TLXM-ITEM-CNT < 9999999
               ADD 1 TO TLXM-ITEM-CNT
           END-IF
           IF TLXM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3360-ACCRUE-WIRE.
           IF TLXM-WIRE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-WIRE-YTD * TLXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TLXM-CORRESP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER SCHED TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3370-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-SCHED-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TLXT-SETTLE-AMT NOT < WS-COVER-SCHED-LOW(WS-TX)
                   MOVE WS-COVER-SCHED-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SETTLE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3380-ACCRUE-SETTLE.
           IF TLXM-SETTLE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-SETTLE-AMT * TLXM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SETTLE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3390-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REGION-CD
                   ADD TLXM-SETTLE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-SETTLE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NOTICE-DATE > 0 AND TLXM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE TLXM-CHARGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3420-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON QUEUED YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-QUEUED.
           IF TLXM-QUEUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-QUEUED-YTD * TLXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TLXT-WIRE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WIRE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO QUEUED AMT.
      *----------------------------------------------------------------*
       3440-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-QUEUED-AMT ROUNDED =
                   TLXT-WIRE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-QUEUED-AMT
           END-IF
           ADD TLXM-QUEUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E894 MEMO REQUIRED
      * E026 SETTLE YTD NEGATIVE
      * E093 SETTLE YTD OVER 5000
      * E712 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-MEMO = SPACES
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-YTD < 0
                   MOVE 'E026' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-YTD > 5000.00
                   MOVE 'E093' TO WS-REJECT-CODE
               WHEN TLXM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E712' TO WS-REJECT-CODE
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
       3460-DESCRIBE-REASON.
           EVALUATE TLXM-REASON-CD
               WHEN 'DGN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JBT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LSS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RDP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RPL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SNE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UJS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON QUEUED AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3470-ACCRUE-QUEUED.
           IF TLXM-QUEUED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-QUEUED-AMT * TLXM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TLXM-COVER-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3480-COUNT-ITEM.
           IF TLXM-ITEM-CNT < 9999999
               ADD 1 TO TLXM-ITEM-CNT
           END-IF
           IF TLXM-ITEM-CNT NOT < 12
               MOVE 'Y' TO TLXM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3490-FLAG-CORRESP.
           IF TLXM-CORRESP-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXM-CORRESP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'P'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'W'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Z'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WIRE YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REGION-CD
                   ADD TLXM-WIRE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-WIRE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-CHARGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE TLXM-CHARGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3530-LIMIT-CORRESP.
           IF TLXM-CORRESP-YTD < 50.00
               MOVE 50.00 TO TLXM-CORRESP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CORRESP-YTD > 2500.00
               MOVE 2500.00 TO TLXM-CORRESP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COVER AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3540-ACCRUE-COVER.
           IF TLXM-COVER-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-COVER-AMT * TLXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLXM-CORRESP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3550-LIMIT-QUEUED.
           IF TLXM-QUEUED-YTD < 10.00
               MOVE 10.00 TO TLXM-QUEUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-QUEUED-YTD > 50000.00
               MOVE 50000.00 TO TLXM-QUEUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER SCHED TIER FOR THE COVER YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3560-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-SCHED-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TLXM-COVER-YTD NOT < WS-COVER-SCHED-LOW(WS-TX)
                   MOVE WS-COVER-SCHED-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3570-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF TLXT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGE FEE ON THE QUEUED AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3580-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TLXM-CHARGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-STOP-FLAG = 'Y'
                   COMPUTE TLXM-CHARGE-AMT ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-RATE / 200
               WHEN TLXM-REGION-CD = 'G'
                   MOVE 10.00 TO TLXM-CHARGE-AMT
               WHEN TLXM-REGION-CD = 'M'
                   MOVE 12.00 TO TLXM-CHARGE-AMT
               WHEN OTHER
                   COMPUTE TLXM-CHARGE-AMT ROUNDED = TLXM-QUEUED-AMT *
                       TLXM-RATE / 100
           END-EVALUATE
           IF TLXM-CHARGE-AMT > 0 AND TLXM-CHARGE-AMT < 5.00
               MOVE 5.00 TO TLXM-CHARGE-AMT
           END-IF
           IF TLXM-CHARGE-AMT > 25.00
               MOVE 25.00 TO TLXM-CHARGE-AMT
           END-IF
           ADD TLXM-CHARGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3590-FLAG-SETTLE.
           IF TLXM-SETTLE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLXM-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COVER FEE ON THE QUEUED YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3600-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TLXT-COVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   COMPUTE TLXT-COVER-AMT ROUNDED = TLXM-QUEUED-YTD *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-BRANCH-CD = 'CC'
                   MOVE 3.00 TO TLXT-COVER-AMT
               WHEN TLXM-BRANCH-CD = 'KR'
                   MOVE 5.00 TO TLXT-COVER-AMT
               WHEN OTHER
                   COMPUTE TLXT-COVER-AMT ROUNDED = TLXM-QUEUED-YTD *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXT-COVER-AMT > 0 AND TLXT-COVER-AMT < 10.00
               MOVE 10.00 TO TLXT-COVER-AMT
           END-IF
           IF TLXT-COVER-AMT > 25.00
               MOVE 25.00 TO TLXT-COVER-AMT
           END-IF
           ADD TLXT-COVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE QUEUED AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3610-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TYPE-CD
                   ADD TLXT-QUEUED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXT-QUEUED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CORRESP YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3620-SPLIT-CORRESP.
           COMPUTE WS-WORK-AMT = TLXM-CORRESP-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TLXM-CORRESP-YTD - (WS-WORK-AMT * 4)
           COMPUTE TLXT-WIRE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WIRE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3630-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-CHANNEL-CD
                   ADD TLXT-WIRE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXT-WIRE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3640-COUNT-ITEM.
           IF TLXM-ITEM-CNT < 9999999
               ADD 1 TO TLXM-ITEM-CNT
           END-IF
           IF TLXM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3650-LIMIT-CORRESP.
           IF TLXM-CORRESP-YTD < 0.00
               MOVE 0.00 TO TLXM-CORRESP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CORRESP-YTD > 1000.00
               MOVE 1000.00 TO TLXM-CORRESP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3660-LIMIT-CORRESP.
           IF TLXM-CORRESP-AMT < 10.00
               MOVE 10.00 TO TLXM-CORRESP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CORRESP-AMT > 500.00
               MOVE 500.00 TO TLXM-CORRESP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-PAYMENT.
           IF TLXM-PAYMENT-CNT < 9999999
               ADD 1 TO TLXM-PAYMENT-CNT
           END-IF
           IF TLXM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BAND TIER FOR THE CORRESP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BAND-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TLXM-CORRESP-AMT NOT < WS-SETTLE-BAND-LOW(WS-TX)
                   MOVE WS-SETTLE-BAND-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3690-FLAG-QUEUED.
           IF TLXT-QUEUED-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXT-QUEUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN LAST PAY DATE IS PAST
      * KR -> LD WHEN LAST PAY DATE IS PAST
      * LD -> LS WHEN LAST PAY DATE IS PAST
      * LS -> XL WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3700-SET-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EFFECTIVE-DATE > 0 AND TLXM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3720-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TLXT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COVER YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3730-ACCRUE-COVER.
           IF TLXM-COVER-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-COVER-YTD * TLXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLXM-SETTLE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN EXPIRY DATE IS PAST
      * L -> M WHEN EXPIRY DATE IS PAST
      * M -> P WHEN EXPIRY DATE IS PAST
      * P -> W WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E870 ADDR REQUIRED
      * E065 WIRE AMT NEGATIVE
      * E367 WIRE AMT OVER 1000000
      * E472 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3750-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-ADDR = SPACES
                   MOVE 'E870' TO WS-REJECT-CODE
               WHEN TLXT-WIRE-AMT < 0
                   MOVE 'E065' TO WS-REJECT-CODE
               WHEN TLXT-WIRE-AMT > 1000000.00
                   MOVE 'E367' TO WS-REJECT-CODE
               WHEN TLXT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E472' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3760-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3770-LIMIT-CHARGE.
           IF TLXM-CHARGE-AMT < 0.00
               MOVE 0.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CHARGE-AMT > 10000.00
               MOVE 10000.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WIRE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO QUEUED YTD.
      *----------------------------------------------------------------*
       3780-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-QUEUED-YTD ROUNDED =
                   TLXM-WIRE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-QUEUED-YTD
           END-IF
           ADD TLXM-QUEUED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3790-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TLXM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TLXM-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TLXM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SETTLE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COVER AMT.
      *----------------------------------------------------------------*
       3810-PRORATE-COVER.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXT-COVER-AMT ROUNDED =
                   TLXT-SETTLE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXT-COVER-AMT
           END-IF
           ADD TLXT-COVER-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CORRESP YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3820-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-BRANCH-CD
                   ADD TLXM-CORRESP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-CORRESP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3830-FLAG-QUEUED.
           IF TLXM-QUEUED-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TLXM-QUEUED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WIRE YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3840-LIMIT-WIRE.
           IF TLXM-WIRE-YTD < 50.00
               MOVE 50.00 TO TLXM-WIRE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-WIRE-YTD > 50000.00
               MOVE 50000.00 TO TLXM-WIRE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CORRESP AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3850-LIMIT-CORRESP.
           IF TLXM-CORRESP-AMT < 0.00
               MOVE 0.00 TO TLXM-CORRESP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CORRESP-AMT > 500.00
               MOVE 500.00 TO TLXM-CORRESP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3860-LIMIT-QUEUED.
           IF TLXM-QUEUED-AMT < 25.00
               MOVE 25.00 TO TLXM-QUEUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-QUEUED-AMT > 50000.00
               MOVE 50000.00 TO TLXM-QUEUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3870-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-COVER-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN LAST ACT DATE IS PAST
      * L -> M WHEN LAST ACT DATE IS PAST
      * M -> P WHEN LAST ACT DATE IS PAST
      * P -> W WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SETTLE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TLXM-SETTLE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = TLXM-SETTLE-YTD - (WS-WORK-AMT * 12)
           COMPUTE TLXM-CHARGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WIRE TIER TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3900-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-WIRE-TIER-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TLXM-QUEUED-AMT NOT < WS-WIRE-TIER-LOW(WS-TX)
                   MOVE WS-WIRE-TIER-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EFFECTIVE-DATE > 0 AND TLXM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-CHARGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-CHARGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-CHARGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-CHARGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CHARGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E387 REF NO REQUIRED
      * E463 SETTLE AMT NEGATIVE
      * E275 SETTLE AMT OVER 50000
      * E411 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3920-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-REF-NO = SPACES
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-AMT < 0
                   MOVE 'E463' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-AMT > 50000.00
                   MOVE 'E275' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E411' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER SCHED TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-SCHED-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TLXM-SETTLE-AMT NOT < WS-COVER-SCHED-LOW(WS-TX)
                   MOVE WS-COVER-SCHED-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3940-DESCRIBE-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'OT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CHARGE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3950-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-CHARGE-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXT-TRAN-CD = 'IN'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN TLXT-TRAN-CD = 'OT'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-CHARGE-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-CHANNEL.
           EVALUATE TLXT-CHANNEL-CD
               WHEN 'BRN'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TLXT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP TIER TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-TIER-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TLXT-QUEUED-AMT NOT < WS-CORRESP-TIER-LOW(WS-TX)
                   MOVE WS-CORRESP-TIER-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3980-FLAG-CHARGE.
           IF TLXM-CHARGE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TLXM-CHARGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WIRE TIER TIER FOR THE CORRESP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3990-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WIRE-TIER-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TLXM-CORRESP-YTD NOT < WS-WIRE-TIER-LOW(WS-TX)
                   MOVE WS-WIRE-TIER-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE QUEUED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO QUEUED AMT.
      *----------------------------------------------------------------*
       4000-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-QUEUED-AMT ROUNDED =
                   TLXT-QUEUED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-QUEUED-AMT
           END-IF
           ADD TLXM-QUEUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE QUEUED FEE ON THE CHARGE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4010-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TLXT-QUEUED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   COMPUTE TLXT-QUEUED-AMT ROUNDED = TLXM-CHARGE-YTD *
                       TLXM-FEE-PCT / 200
               WHEN TLXM-BRANCH-CD = 'CC'
                   MOVE 15.00 TO TLXT-QUEUED-AMT
               WHEN TLXM-BRANCH-CD = 'KR'
                   MOVE 15.00 TO TLXT-QUEUED-AMT
               WHEN OTHER
                   COMPUTE TLXT-QUEUED-AMT ROUNDED = TLXM-CHARGE-YTD *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXT-QUEUED-AMT > 0 AND TLXT-QUEUED-AMT < 1.00
               MOVE 1.00 TO TLXT-QUEUED-AMT
           END-IF
           IF TLXT-QUEUED-AMT > 75.00
               MOVE 75.00 TO TLXT-QUEUED-AMT
           END-IF
           ADD TLXT-QUEUED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E735 CUST ID REQUIRED
      * E427 WIRE AMT NEGATIVE
      * E225 WIRE AMT OVER 50000
      * E123 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4020-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CUST-ID = SPACES
                   MOVE 'E735' TO WS-REJECT-CODE
               WHEN TLXT-WIRE-AMT < 0
                   MOVE 'E427' TO WS-REJECT-CODE
               WHEN TLXT-WIRE-AMT > 50000.00
                   MOVE 'E225' TO WS-REJECT-CODE
               WHEN TLXM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E123' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE WIRE TIER TIER FOR THE WIRE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4030-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-WIRE-TIER-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TLXM-WIRE-YTD NOT < WS-WIRE-TIER-LOW(WS-TX)
                   MOVE WS-WIRE-TIER-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COVER YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO QUEUED AMT.
      *----------------------------------------------------------------*
       4040-PRORATE-QUEUED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-QUEUED-AMT ROUNDED =
                   TLXM-COVER-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-QUEUED-AMT
           END-IF
           ADD TLXM-QUEUED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E790 CONTACT REQUIRED
      * E251 WIRE AMT NEGATIVE
      * E590 WIRE AMT OVER 250000
      * E595 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4060-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CONTACT = SPACES
                   MOVE 'E790' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-AMT < 0
                   MOVE 'E251' TO WS-REJECT-CODE
               WHEN TLXM-WIRE-AMT > 250000.00
                   MOVE 'E590' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E595' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COVER SCHED TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-COVER-SCHED-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TLXT-SETTLE-AMT NOT < WS-COVER-SCHED-LOW(WS-TX)
                   MOVE WS-COVER-SCHED-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CORRESP FEE ON THE QUEUED YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4090-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TLXM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TLXM-CORRESP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   COMPUTE TLXM-CORRESP-AMT ROUNDED = TLXM-QUEUED-YTD *
                       TLXM-RATE / 200
               WHEN TLXM-REASON-CD = 'DGN'
                   MOVE 5.00 TO TLXM-CORRESP-AMT
               WHEN TLXM-REASON-CD = 'JBT'
                   MOVE 3.00 TO TLXM-CORRESP-AMT
               WHEN OTHER
                   COMPUTE TLXM-CORRESP-AMT ROUNDED = TLXM-QUEUED-YTD *
                       TLXM-RATE / 100
           END-EVALUATE
           IF TLXM-CORRESP-AMT > 0 AND TLXM-CORRESP-AMT < 1.00
               MOVE 1.00 TO TLXM-CORRESP-AMT
           END-IF
           IF TLXM-CORRESP-AMT > 150.00
               MOVE 150.00 TO TLXM-CORRESP-AMT
           END-IF
           ADD TLXM-CORRESP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COVER AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4100-ACCRUE-COVER.
           IF TLXT-COVER-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXT-COVER-AMT * TLXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLXT-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SETTLE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4110-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TLXM-SETTLE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TLXM-SETTLE-YTD - (WS-WORK-AMT * 3)
           COMPUTE TLXT-SETTLE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COVER AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4120-SPLIT-COVER.
           COMPUTE WS-WORK-AMT = TLXT-COVER-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TLXT-COVER-AMT - (WS-WORK-AMT * 12)
           COMPUTE TLXM-CORRESP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4130-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EFFECTIVE-DATE > 0 AND TLXM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE WIRE TIER TIER FOR THE COVER YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4140-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WIRE-TIER-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TLXM-COVER-YTD NOT < WS-WIRE-TIER-LOW(WS-TX)
                   MOVE WS-WIRE-TIER-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE QUEUED AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4150-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TLXT-QUEUED-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXM-MSG-TYPE-CD = 'LG'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN TLXM-MSG-TYPE-CD = 'LX'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TLXT-QUEUED-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4160-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4170-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TLXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TLXM-CHARGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4180-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EFFECTIVE-DATE > 0 AND TLXM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-QUEUED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SETTLE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4190-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TLXM-SETTLE-YTD *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-TEST-KEY-CD = 'EA'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN TLXM-TEST-KEY-CD = 'GA'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = TLXM-SETTLE-YTD *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE QUEUED YTD BY TEST KEY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4200-ACCUM-TEST-KEY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TEST-KEY-CD
                   ADD TLXM-QUEUED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TEST-KEY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-QUEUED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN LAST ACT DATE IS PAST
      * L -> M WHEN LAST ACT DATE IS PAST
      * M -> P WHEN LAST ACT DATE IS PAST
      * P -> W WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SETTLE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4220-SPLIT-SETTLE.
           COMPUTE WS-WORK-AMT = TLXM-SETTLE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TLXM-SETTLE-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP TIER TIER FOR THE WIRE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-TIER-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TLXT-WIRE-AMT NOT < WS-CORRESP-TIER-LOW(WS-TX)
                   MOVE WS-CORRESP-TIER-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WIRE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CHARGE YTD.
      *----------------------------------------------------------------*
       4240-PRORATE-CHARGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-CHARGE-YTD ROUNDED =
                   TLXM-WIRE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TLXM-CHARGE-YTD
           END-IF
           ADD TLXM-CHARGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON QUEUED YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-QUEUED.
           IF TLXM-QUEUED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-QUEUED-YTD * TLXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TLXM-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4260-LIMIT-QUEUED.
           IF TLXT-QUEUED-AMT < 25.00
               MOVE 25.00 TO TLXT-QUEUED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-QUEUED-AMT > 1000.00
               MOVE 1000.00 TO TLXT-QUEUED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CC -> KR WHEN LAST PAY DATE IS PAST
      * KR -> LD WHEN LAST PAY DATE IS PAST
      * LD -> LS WHEN LAST PAY DATE IS PAST
      * LS -> XL WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4270-SET-BRANCH.
           EVALUATE TLXM-BRANCH-CD
               WHEN 'CC'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KR' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KR'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LS' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LS'
                   IF TLXM-LAST-PAY-DATE > 0 AND TLXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XL' TO TLXM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4280-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * G -> M WHEN POST DATE IS PAST
      * M -> T WHEN POST DATE IS PAST
      * T -> G WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4290-SET-REGION.
           EVALUATE TLXM-REGION-CD
               WHEN 'G'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TLXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TLXT-POST-DATE > 0 AND TLXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO TLXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * G -> M WHEN NEXT DUE DATE IS PAST
      * M -> T WHEN NEXT DUE DATE IS PAST
      * T -> G WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4300-SET-REGION.
           EVALUATE TLXM-REGION-CD
               WHEN 'G'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO TLXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO TLXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E055 CONTACT REQUIRED
      * E517 SETTLE AMT NEGATIVE
      * E064 SETTLE AMT OVER 1000000
      * E409 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4310-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-CONTACT = SPACES
                   MOVE 'E055' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-AMT < 0
                   MOVE 'E517' TO WS-REJECT-CODE
               WHEN TLXM-SETTLE-AMT > 1000000.00
                   MOVE 'E064' TO WS-REJECT-CODE
               WHEN TLXM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CORRESP AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4320-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXM-TYPE-CD
                   ADD TLXM-CORRESP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TLXM-CORRESP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4330-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TLXM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4340-DESCRIBE-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'OT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4350-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-WIRE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CHARGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WIRE AMT.
      *----------------------------------------------------------------*
       4360-PRORATE-WIRE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXT-WIRE-AMT ROUNDED =
                   TLXM-CHARGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXT-WIRE-AMT
           END-IF
           ADD TLXT-WIRE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP TIER TIER FOR THE QUEUED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4370-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-TIER-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TLXT-QUEUED-AMT NOT < WS-CORRESP-TIER-LOW(WS-TX)
                   MOVE WS-CORRESP-TIER-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4380-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TLXT-TRAN-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TLXT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4390-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-EFFECTIVE-DATE > 0 AND TLXM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXT-SETTLE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E267 OPERATOR REQUIRED
      * E859 SETTLE AMT NEGATIVE
      * E730 SETTLE AMT OVER 5000
      * E945 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4400-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-OPERATOR = SPACES
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN TLXT-SETTLE-AMT < 0
                   MOVE 'E859' TO WS-REJECT-CODE
               WHEN TLXT-SETTLE-AMT > 5000.00
                   MOVE 'E730' TO WS-REJECT-CODE
               WHEN TLXM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E945' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WIRE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4410-ACCRUE-WIRE.
           IF TLXM-WIRE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-WIRE-AMT * TLXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLXM-COVER-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SETTLE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4420-FLAG-SETTLE.
           IF TLXT-SETTLE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXT-SETTLE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CORRESP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       4430-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXM-SETTLE-AMT ROUNDED =
                   TLXM-CORRESP-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXM-SETTLE-AMT
           END-IF
           ADD TLXM-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE WIRE TIER TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4440-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-WIRE-TIER-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF TLXT-SETTLE-AMT NOT < WS-WIRE-TIER-LOW(WS-TX)
                   MOVE WS-WIRE-TIER-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED YTD IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4450-LIMIT-QUEUED.
           IF TLXM-QUEUED-YTD < 100.00
               MOVE 100.00 TO TLXM-QUEUED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-QUEUED-YTD > 5000.00
               MOVE 5000.00 TO TLXM-QUEUED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT QUEUED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4460-SPLIT-QUEUED.
           COMPUTE WS-WORK-AMT = TLXT-QUEUED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TLXT-QUEUED-AMT - (WS-WORK-AMT * 4)
           COMPUTE TLXM-CHARGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E598 REF NO REQUIRED
      * E605 QUEUED AMT NEGATIVE
      * E218 QUEUED AMT OVER 250000
      * E605 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4470-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-REF-NO = SPACES
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-AMT < 0
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-AMT > 250000.00
                   MOVE 'E218' TO WS-REJECT-CODE
               WHEN TLXM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4480-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF TLXT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WIRE FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4490-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TLXM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TLXT-WIRE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-PREFERRED-FLAG = 'Y'
                   COMPUTE TLXT-WIRE-AMT ROUNDED = WS-WORK-AMT *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-TYPE-CD = 'K'
                   MOVE 15.00 TO TLXT-WIRE-AMT
               WHEN TLXM-TYPE-CD = 'L'
                   MOVE 15.00 TO TLXT-WIRE-AMT
               WHEN OTHER
                   COMPUTE TLXT-WIRE-AMT ROUNDED = WS-WORK-AMT *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF TLXT-WIRE-AMT > 0 AND TLXT-WIRE-AMT < 2.00
               MOVE 2.00 TO TLXT-WIRE-AMT
           END-IF
           IF TLXT-WIRE-AMT > 35.00
               MOVE 35.00 TO TLXT-WIRE-AMT
           END-IF
           ADD TLXT-WIRE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BAND TIER FOR THE QUEUED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4500-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BAND-RATE(1) TO TLXM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TLXM-QUEUED-YTD NOT < WS-SETTLE-BAND-LOW(WS-TX)
                   MOVE WS-SETTLE-BAND-RATE(WS-TX) TO TLXM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CORRESP AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4510-ACCRUE-CORRESP.
           IF TLXM-CORRESP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CORRESP-AMT * TLXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO TLXT-SETTLE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN NEXT DUE DATE IS PAST
      * L -> M WHEN NEXT DUE DATE IS PAST
      * M -> P WHEN NEXT DUE DATE IS PAST
      * P -> W WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4520-SET-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TLXM-NEXT-DUE-DATE > 0 AND TLXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4530-COUNT-NOTICE.
           IF TLXM-NOTICE-CNT < 9999999
               ADD 1 TO TLXM-NOTICE-CNT
           END-IF
           IF TLXM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E889 NAME REQUIRED
      * E739 WORK AMT 2 NEGATIVE
      * E468 WORK AMT 2 OVER 250000
      * E410 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4540-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXM-NAME = SPACES
                   MOVE 'E889' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E468' TO WS-REJECT-CODE
               WHEN TLXT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E410' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * DGN -> JBT WHEN CLOSE DATE IS PAST
      * JBT -> LSS WHEN CLOSE DATE IS PAST
      * LSS -> RDP WHEN CLOSE DATE IS PAST
      * RDP -> RPL WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4550-SET-REASON.
           EVALUATE TLXM-REASON-CD
               WHEN 'DGN'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JBT' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JBT'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LSS' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LSS'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RDP' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RDP'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RPL' TO TLXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SETTLE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4560-LIMIT-SETTLE.
           IF TLXM-SETTLE-AMT < 10.00
               MOVE 10.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-SETTLE-AMT > 50000.00
               MOVE 50000.00 TO TLXM-SETTLE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4570-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF TLXT-EFF-DATE > 0 AND TLXT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-WIRE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-WIRE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TLXM-WIRE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TLXM-WIRE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-WIRE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E197 MEMO REQUIRED
      * E210 COVER AMT NEGATIVE
      * E797 COVER AMT OVER 5000
      * E657 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4580-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-MEMO = SPACES
                   MOVE 'E197' TO WS-REJECT-CODE
               WHEN TLXM-COVER-AMT < 0
                   MOVE 'E210' TO WS-REJECT-CODE
               WHEN TLXM-COVER-AMT > 5000.00
                   MOVE 'E797' TO WS-REJECT-CODE
               WHEN TLXM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E657' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4590-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF TLXT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TLXM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4600-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TLXM-CORRESP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E282 OPERATOR REQUIRED
      * E346 COVER YTD NEGATIVE
      * E666 COVER YTD OVER 250000
      * E074 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4610-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-OPERATOR = SPACES
                   MOVE 'E282' TO WS-REJECT-CODE
               WHEN TLXM-COVER-YTD < 0
                   MOVE 'E346' TO WS-REJECT-CODE
               WHEN TLXM-COVER-YTD > 250000.00
                   MOVE 'E666' TO WS-REJECT-CODE
               WHEN TLXM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E074' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4620-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TLXM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TLXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CORRESP YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4630-ACCRUE-CORRESP.
           IF TLXM-CORRESP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CORRESP-YTD * TLXM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TLXT-COVER-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CORRESP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SETTLE AMT.
      *----------------------------------------------------------------*
       4640-PRORATE-SETTLE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TLXT-SETTLE-AMT ROUNDED =
                   TLXM-CORRESP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TLXT-SETTLE-AMT
           END-IF
           ADD TLXT-SETTLE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WIRE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4650-SPLIT-WIRE.
           COMPUTE WS-WORK-AMT = TLXT-WIRE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TLXT-WIRE-AMT - (WS-WORK-AMT * 3)
           COMPUTE TLXM-CORRESP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4660-FLAG-CHARGE.
           IF TLXM-CHARGE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXM-CHARGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4670-ACCRUE-CHARGE.
           IF TLXM-CHARGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TLXM-CHARGE-AMT * TLXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TLXT-WIRE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4680-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TLXM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TLXM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * K -> L WHEN EXPIRY DATE IS PAST
      * L -> M WHEN EXPIRY DATE IS PAST
      * M -> P WHEN EXPIRY DATE IS PAST
      * P -> W WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4690-SET-TYPE.
           EVALUATE TLXM-TYPE-CD
               WHEN 'K'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TLXM-EXPIRY-DATE > 0 AND TLXM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO TLXM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE COVER FEE ON THE SETTLE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4700-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TLXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TLXT-COVER-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-STOP-FLAG = 'Y'
                   COMPUTE TLXT-COVER-AMT ROUNDED = TLXT-SETTLE-AMT *
                       TLXM-RATE / 200
               WHEN TLXM-BRANCH-CD = 'CC'
                   MOVE 5.00 TO TLXT-COVER-AMT
               WHEN TLXM-BRANCH-CD = 'KR'
                   MOVE 20.00 TO TLXT-COVER-AMT
               WHEN OTHER
                   COMPUTE TLXT-COVER-AMT ROUNDED = TLXT-SETTLE-AMT *
                       TLXM-RATE / 100
           END-EVALUATE
           IF TLXT-COVER-AMT > 0 AND TLXT-COVER-AMT < 5.00
               MOVE 5.00 TO TLXT-COVER-AMT
           END-IF
           IF TLXT-COVER-AMT > 75.00
               MOVE 75.00 TO TLXT-COVER-AMT
           END-IF
           ADD TLXT-COVER-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CHARGE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4710-LIMIT-CHARGE.
           IF TLXM-CHARGE-AMT < 50.00
               MOVE 50.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXM-CHARGE-AMT > 50000.00
               MOVE 50000.00 TO TLXM-CHARGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGE FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4720-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TLXM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TLXM-CHARGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   COMPUTE TLXM-CHARGE-AMT ROUNDED = WS-WORK-AMT *
                       TLXM-FEE-PCT / 200
               WHEN TLXT-TRAN-CD = 'IN'
                   MOVE 3.00 TO TLXM-CHARGE-AMT
               WHEN TLXT-TRAN-CD = 'OT'
                   MOVE 3.00 TO TLXM-CHARGE-AMT
               WHEN OTHER
                   COMPUTE TLXM-CHARGE-AMT ROUNDED = WS-WORK-AMT *
                       TLXM-FEE-PCT / 100
           END-EVALUATE
           IF TLXM-CHARGE-AMT > 0 AND TLXM-CHARGE-AMT < 1.00
               MOVE 1.00 TO TLXM-CHARGE-AMT
           END-IF
           IF TLXM-CHARGE-AMT > 50.00
               MOVE 50.00 TO TLXM-CHARGE-AMT
           END-IF
           ADD TLXM-CHARGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE SETTLE BAND TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4730-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SETTLE-BAND-RATE(1) TO TLXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TLXM-SETTLE-AMT NOT < WS-SETTLE-BAND-LOW(WS-TX)
                   MOVE WS-SETTLE-BAND-RATE(WS-TX) TO TLXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR MSG TYPE CD:
      * LG -> LX WHEN CLOSE DATE IS PAST
      * LX -> ND WHEN CLOSE DATE IS PAST
      * ND -> PT WHEN CLOSE DATE IS PAST
      * PT -> LG WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4740-SET-MSG-TYPE.
           EVALUATE TLXM-MSG-TYPE-CD
               WHEN 'LG'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LX' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LX'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TLXM-CLOSE-DATE > 0 AND TLXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LG' TO TLXM-MSG-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E372 REJECT CD REQUIRED
      * E200 QUEUED AMT NEGATIVE
      * E213 QUEUED AMT OVER 1000000
      * E630 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4750-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TLXT-REJECT-CD = SPACES
                   MOVE 'E372' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-AMT < 0
                   MOVE 'E200' TO WS-REJECT-CODE
               WHEN TLXM-QUEUED-AMT > 1000000.00
                   MOVE 'E213' TO WS-REJECT-CODE
               WHEN TLXM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E630' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CORRESP TIER TIER FOR THE SETTLE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4760-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CORRESP-TIER-RATE(1) TO TLXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF TLXT-SETTLE-AMT NOT < WS-CORRESP-TIER-LOW(WS-TX)
                   MOVE WS-CORRESP-TIER-RATE(WS-TX) TO TLXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TLXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TLXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * QUEUED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4770-FLAG-QUEUED.
           IF TLXT-QUEUED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TLXT-QUEUED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CHARGE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4780-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-CHARGE-YTD *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-TEST-KEY-CD = 'EA'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN TLXM-TEST-KEY-CD = 'GA'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-CHARGE-YTD *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4790-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TLXM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TLXM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TLXM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * IN -> OT WHEN LAST ACT DATE IS PAST
      * OT -> RJ WHEN LAST ACT DATE IS PAST
      * RJ -> RP WHEN LAST ACT DATE IS PAST
      * RP -> CN WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4800-SET-TRAN.
           EVALUATE TLXT-TRAN-CD
               WHEN 'IN'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OT' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'OT'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJ' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJ'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RP' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RP'
                   IF TLXM-LAST-ACT-DATE > 0 AND TLXM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO TLXT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WIRE AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4810-LIMIT-WIRE.
           IF TLXT-WIRE-AMT < 10.00
               MOVE 10.00 TO TLXT-WIRE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TLXT-WIRE-AMT > 500.00
               MOVE 500.00 TO TLXT-WIRE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE SETTLE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4820-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TLXM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TLXM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-SETTLE-AMT *
                       TLXM-DISCOUNT / 200
               WHEN TLXM-TYPE-CD = 'K'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN TLXM-TYPE-CD = 'L'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = TLXM-SETTLE-AMT *
                       TLXM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 150.00
               MOVE 150.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PASS THE TLX FIELDS TO TXFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-TXFEE.
           INITIALIZE TXFEE-PARMS
           MOVE TLXM-SETTLE-AMT
             TO TXFEE-SETTLE-AMT
           MOVE TLXM-SETTLE-YTD
             TO TXFEE-SETTLE-YTD
           MOVE TLXM-COVER-AMT
             TO TXFEE-COVER-AMT
           MOVE TLXM-QUEUED-YTD
             TO TXFEE-QUEUED-YTD
           MOVE TLXM-WIRE-YTD
             TO TXFEE-WIRE-YTD
           MOVE TLXM-CORRESP-AMT
             TO TXFEE-CORRESP-AMT
           MOVE TLXM-CLOSE-DATE
             TO TXFEE-CLOSE-DATE
           MOVE TLXM-LAST-PAY-DATE
             TO TXFEE-LAST-PAY-DATE
           MOVE TLXM-LAST-ACT-DATE
             TO TXFEE-LAST-ACT-DATE
           MOVE TLXM-NEXT-DUE-DATE
             TO TXFEE-NEXT-DUE-DATE
           MOVE TLXM-REGION-CD
             TO TXFEE-REGION-CD
           MOVE TLXM-TYPE-CD
             TO TXFEE-TYPE-CD
           MOVE TLXM-BRANCH-CD
             TO TXFEE-BRANCH-CD
           MOVE TLXM-TEST-KEY-CD
             TO TXFEE-TEST-KEY-CD
           MOVE TLXM-NOTICE-SENT-FLAG
             TO TXFEE-NOTICE-SENT-FLAG
           MOVE TLXM-SUSPEND-FLAG
             TO TXFEE-SUSPEND-FLAG
           MOVE TLXM-PREFERRED-FLAG
             TO TXFEE-PREFERRED-FLAG
           MOVE TLXM-EXCEPT-CNT
             TO TXFEE-EXCEPT-CNT
           MOVE TLXM-ITEM-CNT
             TO TXFEE-ITEM-CNT
           MOVE TLXM-FEE-PCT
             TO TXFEE-FEE-PCT
           MOVE TLXM-RATE
             TO TXFEE-RATE
           MOVE TLXM-CUST-ID
             TO TXFEE-CUST-ID
           MOVE TLXM-CONTACT
             TO TXFEE-CONTACT
           CALL 'TXFEE' USING TXFEE-PARMS
           EVALUATE TXFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TXFEE-SETTLE-AMT
                     TO TLXM-SETTLE-AMT
                   MOVE TXFEE-SETTLE-YTD
                     TO TLXM-SETTLE-YTD
                   MOVE TXFEE-QUEUED-YTD
                     TO TLXM-QUEUED-YTD
                   MOVE TXFEE-NOTICE-SENT-FLAG
                     TO TLXM-NOTICE-SENT-FLAG
                   MOVE TXFEE-SUSPEND-FLAG
                     TO TLXM-SUSPEND-FLAG
                   MOVE TXFEE-PREFERRED-FLAG
                     TO TLXM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-TXFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TLX FIELDS TO TXELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-TXELG.
           INITIALIZE TXELG-PARMS
           MOVE TLXM-WIRE-YTD
             TO TXELG-WIRE-YTD
           MOVE TLXM-CHARGE-AMT
             TO TXELG-CHARGE-AMT
           MOVE TLXM-QUEUED-AMT
             TO TXELG-QUEUED-AMT
           MOVE TLXM-CORRESP-YTD
             TO TXELG-CORRESP-YTD
           MOVE TLXM-CORRESP-AMT
             TO TXELG-CORRESP-AMT
           MOVE TLXM-WIRE-AMT
             TO TXELG-WIRE-AMT
           MOVE TLXM-LAST-PAY-DATE
             TO TXELG-LAST-PAY-DATE
           MOVE TLXM-NEXT-DUE-DATE
             TO TXELG-NEXT-DUE-DATE
           MOVE TLXM-EFFECTIVE-DATE
             TO TXELG-EFFECTIVE-DATE
           MOVE TLXM-CLOSE-DATE
             TO TXELG-CLOSE-DATE
           MOVE TLXM-MSG-TYPE-CD
             TO TXELG-MSG-TYPE-CD
           MOVE TLXM-REGION-CD
             TO TXELG-REGION-CD
           MOVE TLXM-TYPE-CD
             TO TXELG-TYPE-CD
           MOVE TLXM-REASON-CD
             TO TXELG-REASON-CD
           MOVE TLXM-SUSPEND-FLAG
             TO TXELG-SUSPEND-FLAG
           MOVE TLXM-STOP-FLAG
             TO TXELG-STOP-FLAG
           MOVE TLXM-OVERRIDE-FLAG
             TO TXELG-OVERRIDE-FLAG
           MOVE TLXM-PAYMENT-CNT
             TO TXELG-PAYMENT-CNT
           MOVE TLXM-NOTICE-CNT
             TO TXELG-NOTICE-CNT
           MOVE TLXM-FEE-PCT
             TO TXELG-FEE-PCT
           MOVE TLXM-RATE
             TO TXELG-RATE
           MOVE TLXM-ADDR
             TO TXELG-ADDR
           MOVE TLXM-DESC
             TO TXELG-DESC
           CALL 'TXELG' USING TXELG-PARMS
           EVALUATE TXELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TXELG-WIRE-YTD
                     TO TLXM-WIRE-YTD
                   MOVE TXELG-CHARGE-AMT
                     TO TLXM-CHARGE-AMT
                   MOVE TXELG-QUEUED-AMT
                     TO TLXM-QUEUED-AMT
                   MOVE TXELG-CORRESP-YTD
                     TO TLXM-CORRESP-YTD
                   MOVE TXELG-STOP-FLAG
                     TO TLXM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-TXELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TLX FIELDS TO TXPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-TXPRC.
           INITIALIZE TXPRC-PARMS
           MOVE TLXM-COVER-YTD
             TO TXPRC-COVER-YTD
           MOVE TLXM-QUEUED-AMT
             TO TXPRC-QUEUED-AMT
           MOVE TLXM-CORRESP-YTD
             TO TXPRC-CORRESP-YTD
           MOVE TLXM-SETTLE-YTD
             TO TXPRC-SETTLE-YTD
           MOVE TLXM-QUEUED-YTD
             TO TXPRC-QUEUED-YTD
           MOVE TLXM-SETTLE-AMT
             TO TXPRC-SETTLE-AMT
           MOVE TLXM-OPEN-DATE
             TO TXPRC-OPEN-DATE
           MOVE TLXM-NEXT-DUE-DATE
             TO TXPRC-NEXT-DUE-DATE
           MOVE TLXM-NOTICE-DATE
             TO TXPRC-NOTICE-DATE
           MOVE TLXM-EXPIRY-DATE
             TO TXPRC-EXPIRY-DATE
           MOVE TLXM-TEST-KEY-CD
             TO TXPRC-TEST-KEY-CD
           MOVE TLXM-REGION-CD
             TO TXPRC-REGION-CD
           MOVE TLXM-BRANCH-CD
             TO TXPRC-BRANCH-CD
           MOVE TLXM-REASON-CD
             TO TXPRC-REASON-CD
           MOVE TLXM-NOTICE-SENT-FLAG
             TO TXPRC-NOTICE-SENT-FLAG
           MOVE TLXM-STOP-FLAG
             TO TXPRC-STOP-FLAG
           MOVE TLXM-REVIEW-FLAG
             TO TXPRC-REVIEW-FLAG
           MOVE TLXM-PAYMENT-CNT
             TO TXPRC-PAYMENT-CNT
           MOVE TLXM-EXCEPT-CNT
             TO TXPRC-EXCEPT-CNT
           MOVE TLXM-RATE
             TO TXPRC-RATE
           MOVE TLXM-DISCOUNT
             TO TXPRC-DISCOUNT
           MOVE TLXM-ADDR
             TO TXPRC-ADDR
           MOVE TLXM-NAME
             TO TXPRC-NAME
           CALL 'TXPRC' USING TXPRC-PARMS
           EVALUATE TXPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TXPRC-COVER-YTD
                     TO TLXM-COVER-YTD
                   MOVE TXPRC-QUEUED-AMT
                     TO TLXM-QUEUED-AMT
                   MOVE TXPRC-CORRESP-YTD
                     TO TLXM-CORRESP-YTD
                   MOVE TXPRC-QUEUED-YTD
                     TO TLXM-QUEUED-YTD
                   MOVE TXPRC-SETTLE-AMT
                     TO TLXM-SETTLE-AMT
                   MOVE TXPRC-REVIEW-FLAG
                     TO TLXM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-TXPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TLXT-KEY TO RD-KEY
           MOVE TLXT-TRAN-CD TO RD-CODE
           MOVE TLXT-COVER-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TLXT-KEY TO RD-KEY
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
           DISPLAY 'TXXEDIT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TXXEDIT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
