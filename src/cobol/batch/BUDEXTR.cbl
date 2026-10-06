      *================================================================*
      * PROGRAM  : BUDEXTR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : BUMP-UP CERTIFICATES - NIGHTLY EXTRACT. READS THE   *
      *            BCD TRANSACTIONS, UPDATES THE BCD MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. BUMP-UP AND STEP-RATE  *
      *            CERTIFICATE RATE ELECTION AND ACCRUAL               *
      * INPUT    : BCDMST (KSDS), BCDTRN, BANKCTL                      *
      * OUTPUT   : BCDOUT, BUDEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, BUSTAT, BUVAL           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-05-07 RJT  ORIGINAL                                       *
      * 1994-12-18 LMB  PENALTY CUTOFF MOVED INC3856                   *
      * 1996-04-04 GAF  PERFORMANCE - ADDON READ AHEAD CHG3290         *
      * 2000-10-07 EWS  INTEREST RERUN PROTECTION PRB2503              *
      * 2000-11-26 MKL  PERFORMANCE - PRINCIPAL READ AHEAD REQ4211     *
      * 2001-02-17 CRP  INTEREST FEE SCHEDULE UPDATE PRB4273           *
      * 2001-11-26 PQR  AUDIT FINDING - BUMP CONTROL TOTALS PRB7183    *
      * 2002-02-26 GAF  PRINCIPAL CALCULATION CORRECTED INC2731        *
      * 2004-11-08 CRP  PERFORMANCE - INTEREST READ AHEAD CHG2169      *
      * 2005-12-26 MKL  PRINCIPAL STATUS CODES ADDED PRB6116           *
      * 2005-07-25 TWH  INTEREST CALCULATION CORRECTED INC5131         *
      * 2006-04-06 SKN  PERFORMANCE - BUMP READ AHEAD CHG7846          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUDEXTR.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "BCDMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS BCDM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "BCDTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "BCDOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "BUDEXRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY BCDMST.
       FD  TRAN-FILE.
       COPY BCDTRN.
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
                                           VALUE 14.000000.
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
      * WS-INTEREST-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000223500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000875500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000066500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000882500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011600000000230000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011700000000310000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016700000000095000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016950000000365000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021950000000709500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031950000000803000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032450000000501500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032550000000072000'.
       01  WS-INTEREST-TIER-TBL REDEFINES WS-INTEREST-TIER-VALUES.
           05  WS-INTEREST-TIER-ROW OCCURS 12 TIMES.
               10  WS-INTEREST-TIER-LOW    PIC 9(09)V99.
               10  WS-INTEREST-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INTEREST-TIERX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INTEREST-TIERX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000292000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000475500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000724000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003050000000241500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000013500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004050000000843500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006550000000540000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011550000000805500'.
       01  WS-INTEREST-TIERX-TBL REDEFINES WS-INTEREST-TIERX-VALUES.
           05  WS-INTEREST-TIERX-ROW OCCURS 8 TIMES.
               10  WS-INTEREST-TIERX-LOW   PIC 9(09)V99.
               10  WS-INTEREST-TIERX-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-BUMP-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-BUMP-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000818500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000006000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000212500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000512500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004050000000569000'.
       01  WS-BUMP-TIER-TBL REDEFINES WS-BUMP-TIER-VALUES.
           05  WS-BUMP-TIER-ROW OCCURS 5 TIMES.
               10  WS-BUMP-TIER-LOW        PIC 9(09)V99.
               10  WS-BUMP-TIER-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PRINCIPA-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINCIPA-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000626000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000788000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000516000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000650000000182500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010650000000672000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000183000'.
       01  WS-PRINCIPA-GRID-TBL REDEFINES WS-PRINCIPA-GRID-VALUES.
           05  WS-PRINCIPA-GRID-ROW OCCURS 6 TIMES.
               10  WS-PRINCIPA-GRID-LOW    PIC 9(09)V99.
               10  WS-PRINCIPA-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  BUDEXTR  BUMP-UP CERTIFICATES'.
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
       COPY BUSTATP.
       COPY BUVALP.

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
               MOVE 'BCDMST' TO ABND-FILE-NAME
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
           MOVE BCDT-KEY TO BCDM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-LIMIT-INTEREST
               PERFORM 3010-COUNT-CYCLE
               PERFORM 3020-ACCUM-ELECTION
               PERFORM 3030-SELECT-RATE
               PERFORM 3040-SELECT-RATE
               PERFORM 3050-EDIT-OPERATOR
               PERFORM 3060-SET-STEP-PLAN
               PERFORM 3070-LIMIT-ADDON
               PERFORM 3080-ACCRUE-INTEREST
               PERFORM 3090-SPLIT-PRINCIPAL
               PERFORM 3100-CHECK-NEXT-REVIEW
               PERFORM 3110-ASSESS-EXEMPT
               PERFORM 3120-ACCUM-CYCLE
               PERFORM 3130-AGE-LAST-PAY
               PERFORM 3140-COUNT-ITEM
               PERFORM 3150-EDIT-OPERATOR
               PERFORM 3160-SPLIT-BUMP
               PERFORM 3170-FLAG-PRINCIPAL
               PERFORM 3180-AGE-EFFECTIVE
               PERFORM 3190-CHECK-NEXT-DUE
               PERFORM 3200-SET-CHANNEL
               PERFORM 3210-ACCUM-CYCLE
               PERFORM 3220-CHECK-EFFECTIVE
               PERFORM 3230-SET-CHANNEL
               PERFORM 3240-SELECT-RATE
               PERFORM 3250-SET-CYCLE
               PERFORM 3260-EDIT-DESC
               PERFORM 3270-AGE-LAST-MAINT
               PERFORM 3280-SPLIT-ADDON
               PERFORM 3290-PRORATE-INTEREST
               PERFORM 3300-PRORATE-BUMP
               PERFORM 3310-FLAG-PENALTY
               PERFORM 3320-EDIT-DESC
               PERFORM 3330-PRORATE-INTEREST
               PERFORM 3340-AGE-MATURITY
               PERFORM 3350-CHECK-LAST-PAY
               PERFORM 3360-EDIT-OPERATOR
               PERFORM 3370-ASSESS-EXEMPT
               PERFORM 3380-CHECK-EFFECTIVE
               PERFORM 3390-LIMIT-PRINCIPAL
               PERFORM 3400-FLAG-ADDON
               PERFORM 3410-SELECT-RATE
               PERFORM 3420-PRORATE-INTEREST
               PERFORM 3430-SET-TRAN
               PERFORM 3440-AGE-EFF
               PERFORM 3450-ASSESS-OVERRIDE
               PERFORM 3460-SPLIT-BUMP
               PERFORM 3470-ACCUM-SOURCE
               PERFORM 3480-ACCUM-ELECTION
               PERFORM 3490-ACCUM-TRAN
               PERFORM 3500-SPLIT-INTEREST
               PERFORM 3510-AGE-LAST-PAY
               PERFORM 3520-AGE-EXPIRY
               PERFORM 3530-SPLIT-INTEREST
               PERFORM 3540-CHECK-POST
               PERFORM 3550-SET-CHANNEL
               PERFORM 3560-LIMIT-PENALTY
               PERFORM 3570-SPLIT-PRINCIPAL
               PERFORM 3580-SET-STEP-PLAN
               PERFORM 3590-CHECK-LAST-MAINT
               PERFORM 3600-EDIT-OPERATOR
               PERFORM 3610-PRORATE-ADDON
               PERFORM 3620-DESCRIBE-ELECTION
               PERFORM 3630-LIMIT-INTEREST
               PERFORM 3640-LIMIT-ADDON
               PERFORM 3650-FLAG-INTEREST
               PERFORM 3660-SET-CHANNEL
               PERFORM 3670-EDIT-MEMO
               PERFORM 3680-LIMIT-INTEREST
               PERFORM 3690-LIMIT-PRINCIPAL
               PERFORM 3700-ASSESS-REVERSAL
               PERFORM 3710-ASSESS-EXEMPT
               PERFORM 3720-AGE-NEXT-REVIEW
               PERFORM 3730-AGE-NEXT-REVIEW
               PERFORM 3740-FLAG-PRINCIPAL
               PERFORM 3750-COUNT-RETRY
               PERFORM 3760-ASSESS-REVERSAL
               PERFORM 3770-DESCRIBE-CHANNEL
               PERFORM 3780-SELECT-RATE
               PERFORM 3790-FLAG-ADDON
               PERFORM 3800-ACCRUE-MARGIN
               PERFORM 3810-SET-REGION
               PERFORM 3820-COUNT-SEQ
               PERFORM 3830-AGE-NEXT-REVIEW
               PERFORM 3840-EDIT-MEMO
               PERFORM 3850-ASSESS-HOLD
               PERFORM 3860-LIMIT-ITEM
               PERFORM 3870-ACCUM-CHANNEL
               PERFORM 3880-EDIT-NAME
               PERFORM 3890-SPLIT-PENALTY
               PERFORM 3900-FLAG-INTEREST
               PERFORM 3910-SET-ELECTION
               PERFORM 3920-ACCRUE-BUMP
               PERFORM 3930-ASSESS-EXEMPT
               PERFORM 3940-SELECT-RATE
               PERFORM 3950-CHECK-EXPIRY
               PERFORM 3960-AGE-LAST-MAINT
               PERFORM 3970-SELECT-RATE
               PERFORM 3980-EDIT-DESC
               PERFORM 3990-CHECK-EXPIRY
               PERFORM 4000-LIMIT-PENALTY
               PERFORM 4010-COUNT-SEQ
               PERFORM 4020-COUNT-RETRY
               PERFORM 4030-SPLIT-ADDON
               PERFORM 4040-SELECT-PENALTY-PCT
               PERFORM 4050-SET-TRAN
               PERFORM 4060-FLAG-PRINCIPAL
               PERFORM 4070-SET-STEP-PLAN
               PERFORM 7030-CALL-BUSTAT
               PERFORM 7010-CALL-BUVAL
               REWRITE BCD-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'BCDMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM BCD-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3000-LIMIT-INTEREST.
           IF BCDM-INTEREST-AMT < 0.00
               MOVE 0.00 TO BCDM-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-INTEREST-AMT > 10000.00
               MOVE 10000.00 TO BCDM-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-CYCLE.
           IF BCDM-CYCLE-CNT < 9999999
               ADD 1 TO BCDM-CYCLE-CNT
           END-IF
           IF BCDM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON AMT BY ELECTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3020-ACCUM-ELECTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-ELECTION-CD
                   ADD BCDT-ADDON-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-ELECTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-ADDON-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE ADDON AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BCDT-ADDON-AMT NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP TIER TIER FOR THE BUMP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-TIER-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF BCDT-BUMP-AMT NOT < WS-BUMP-TIER-LOW(WS-TX)
                   MOVE WS-BUMP-TIER-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E909 OPERATOR REQUIRED
      * E340 ADDON AMT NEGATIVE
      * E448 ADDON AMT OVER 99999
      * E357 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3050-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-OPERATOR = SPACES
                   MOVE 'E909' TO WS-REJECT-CODE
               WHEN BCDT-ADDON-AMT < 0
                   MOVE 'E340' TO WS-REJECT-CODE
               WHEN BCDT-ADDON-AMT > 99999.00
                   MOVE 'E448' TO WS-REJECT-CODE
               WHEN BCDM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E357' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN LAST PAY DATE IS PAST
      * DGU -> GWJ WHEN LAST PAY DATE IS PAST
      * GWJ -> RJL WHEN LAST PAY DATE IS PAST
      * RJL -> UPM WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3060-SET-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ADDON AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3070-LIMIT-ADDON.
           IF BCDT-ADDON-AMT < 25.00
               MOVE 25.00 TO BCDT-ADDON-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDT-ADDON-AMT > 10000.00
               MOVE 10000.00 TO BCDT-ADDON-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INTEREST YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-INTEREST.
           IF BCDM-INTEREST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDM-INTEREST-YTD * BCDM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BCDT-PENALTY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3090-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BCDM-PRINCIPAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BCDM-PRINCIPAL-AMT - (WS-WORK-AMT *
               2)
           COMPUTE BCDM-PENALTY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3100-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO BCDM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON YTD FEE ON THE BUMP YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3110-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BCDM-ADDON-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-REVIEW-FLAG = 'Y'
                   COMPUTE BCDM-ADDON-YTD ROUNDED = BCDM-BUMP-YTD *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDM-REGION-CD = 'BGJ'
                   MOVE 10.00 TO BCDM-ADDON-YTD
               WHEN BCDM-REGION-CD = 'LFX'
                   MOVE 12.00 TO BCDM-ADDON-YTD
               WHEN OTHER
                   COMPUTE BCDM-ADDON-YTD ROUNDED = BCDM-BUMP-YTD *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDM-ADDON-YTD > 0 AND BCDM-ADDON-YTD < 2.00
               MOVE 2.00 TO BCDM-ADDON-YTD
           END-IF
           IF BCDM-ADDON-YTD > 150.00
               MOVE 150.00 TO BCDM-ADDON-YTD
           END-IF
           ADD BCDM-ADDON-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PENALTY AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CYCLE-CD
                   ADD BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-PENALTY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3140-COUNT-ITEM.
           IF BCDM-ITEM-CNT < 9999999
               ADD 1 TO BCDM-ITEM-CNT
           END-IF
           IF BCDM-ITEM-CNT NOT < 5
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E283 OPERATOR REQUIRED
      * E401 PENALTY AMT NEGATIVE
      * E977 PENALTY AMT OVER 10000
      * E063 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3150-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-OPERATOR = SPACES
                   MOVE 'E283' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-AMT < 0
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-AMT > 10000.00
                   MOVE 'E977' TO WS-REJECT-CODE
               WHEN BCDT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E063' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BUMP AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3160-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDT-BUMP-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BCDT-BUMP-AMT - (WS-WORK-AMT * 3)
           COMPUTE BCDM-INTEREST-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3170-FLAG-PRINCIPAL.
           IF BCDM-PRINCIPAL-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDM-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PENALTY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3200-SET-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3210-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-CYCLE-CD
                   ADD BCDM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * EK -> JE WHEN POST DATE IS PAST
      * JE -> PN WHEN POST DATE IS PAST
      * PN -> WV WHEN POST DATE IS PAST
      * WV -> EK WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JE' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JE'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PN' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PN'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WV' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WV'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EK' TO BCDM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> S WHEN POST DATE IS PAST
      * S -> T WHEN POST DATE IS PAST
      * T -> U WHEN POST DATE IS PAST
      * U -> X WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3250-SET-CYCLE.
           EVALUATE BCDM-CYCLE-CD
               WHEN 'A'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO BCDM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E833 DESC REQUIRED
      * E010 BUMP AMT NEGATIVE
      * E052 BUMP AMT OVER 1000000
      * E624 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-DESC = SPACES
                   MOVE 'E833' TO WS-REJECT-CODE
               WHEN BCDT-BUMP-AMT < 0
                   MOVE 'E010' TO WS-REJECT-CODE
               WHEN BCDT-BUMP-AMT > 1000000.00
                   MOVE 'E052' TO WS-REJECT-CODE
               WHEN BCDM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E624' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3270-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-BUMP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-BUMP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-BUMP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-BUMP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-BUMP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ADDON AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3280-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BCDM-ADDON-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BCDM-ADDON-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INTEREST YTD.
      *----------------------------------------------------------------*
       3290-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-INTEREST-YTD ROUNDED =
                   BCDT-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-INTEREST-YTD
           END-IF
           ADD BCDM-INTEREST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BUMP YTD.
      *----------------------------------------------------------------*
       3300-PRORATE-BUMP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-BUMP-YTD ROUNDED =
                   BCDM-INTEREST-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-BUMP-YTD
           END-IF
           ADD BCDM-BUMP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PENALTY AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3310-FLAG-PENALTY.
           IF BCDM-PENALTY-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDM-PENALTY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E578 DESC REQUIRED
      * E625 PENALTY YTD NEGATIVE
      * E693 PENALTY YTD OVER 50000
      * E569 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3320-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-DESC = SPACES
                   MOVE 'E578' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-YTD < 0
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN BCDM-PENALTY-YTD > 50000.00
                   MOVE 'E693' TO WS-REJECT-CODE
               WHEN BCDM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E569' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PENALTY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       3330-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDT-INTEREST-AMT ROUNDED =
                   BCDT-PENALTY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDT-INTEREST-AMT
           END-IF
           ADD BCDT-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3340-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-MATURITY-DATE > 0 AND BCDM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDT-INTEREST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E725 OPERATOR REQUIRED
      * E344 WORK AMT NEGATIVE
      * E640 WORK AMT OVER 1000000
      * E341 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-OPERATOR = SPACES
                   MOVE 'E725' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E344' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 1000000.00
                   MOVE 'E640' TO WS-REJECT-CODE
               WHEN BCDM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E341' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PENALTY YTD FEE ON THE PENALTY AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3370-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PENALTY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-HOLD-FLAG = 'Y'
                   COMPUTE BCDM-PENALTY-YTD ROUNDED = BCDM-PENALTY-AMT *
                       BCDM-MARGIN / 200
               WHEN BCDM-SOURCE-CD = 'AG'
                   MOVE 15.00 TO BCDM-PENALTY-YTD
               WHEN BCDM-SOURCE-CD = 'BF'
                   MOVE 3.00 TO BCDM-PENALTY-YTD
               WHEN OTHER
                   COMPUTE BCDM-PENALTY-YTD ROUNDED = BCDM-PENALTY-AMT *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-PENALTY-YTD > 0 AND BCDM-PENALTY-YTD < 10.00
               MOVE 10.00 TO BCDM-PENALTY-YTD
           END-IF
           IF BCDM-PENALTY-YTD > 35.00
               MOVE 35.00 TO BCDM-PENALTY-YTD
           END-IF
           ADD BCDM-PENALTY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3380-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3390-LIMIT-PRINCIPAL.
           IF BCDM-PRINCIPAL-AMT < 0.00
               MOVE 0.00 TO BCDM-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PRINCIPAL-AMT > 500.00
               MOVE 500.00 TO BCDM-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADDON AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3400-FLAG-ADDON.
           IF BCDM-ADDON-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDM-ADDON-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3410-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BCDM-PRINCIPAL-AMT NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BUMP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INTEREST AMT.
      *----------------------------------------------------------------*
       3420-PRORATE-INTEREST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-INTEREST-AMT ROUNDED =
                   BCDT-BUMP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BCDM-INTEREST-AMT
           END-IF
           ADD BCDM-INTEREST-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BU -> AD WHEN POST DATE IS PAST
      * AD -> WD WHEN POST DATE IS PAST
      * WD -> MT WHEN POST DATE IS PAST
      * MT -> BU WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3430-SET-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MT' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF BCDT-POST-DATE > 0 AND BCDT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BU' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF BCDT-EFF-DATE > 0 AND BCDT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDT-BUMP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE BUMP YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3450-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BCDM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   COMPUTE BCDM-PRINCIPAL-AMT ROUNDED = BCDM-BUMP-YTD *
                       BCDM-MARGIN / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 15.00 TO BCDM-PRINCIPAL-AMT
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 5.00 TO BCDM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE BCDM-PRINCIPAL-AMT ROUNDED = BCDM-BUMP-YTD *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-PRINCIPAL-AMT > 0 AND BCDM-PRINCIPAL-AMT < 1.00
               MOVE 1.00 TO BCDM-PRINCIPAL-AMT
           END-IF
           IF BCDM-PRINCIPAL-AMT > 35.00
               MOVE 35.00 TO BCDM-PRINCIPAL-AMT
           END-IF
           ADD BCDM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT BUMP YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3460-SPLIT-BUMP.
           COMPUTE WS-WORK-AMT = BCDM-BUMP-YTD / 2
           COMPUTE WS-WORK-AMT-2 = BCDM-BUMP-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3470-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-SOURCE-CD
                   ADD BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON AMT BY ELECTION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3480-ACCUM-ELECTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDM-ELECTION-CD
                   ADD BCDT-ADDON-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDM-ELECTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDT-ADDON-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3490-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDT-TRAN-CD
                   ADD BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-ADDON-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INTEREST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3500-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDT-INTEREST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BCDT-INTEREST-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3510-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-PAY-DATE > 0 AND BCDM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-INTEREST-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INTEREST YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-INTEREST.
           COMPUTE WS-WORK-AMT = BCDM-INTEREST-YTD / 2
           COMPUTE WS-WORK-AMT-2 = BCDM-INTEREST-YTD - (WS-WORK-AMT * 2)
           COMPUTE BCDT-ADDON-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3540-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF BCDT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO BCDM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3550-SET-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BCDM-EFFECTIVE-DATE > 0 AND BCDM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PENALTY AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3560-LIMIT-PENALTY.
           IF BCDM-PENALTY-AMT < 0.00
               MOVE 0.00 TO BCDM-PENALTY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PENALTY-AMT > 500.00
               MOVE 500.00 TO BCDM-PENALTY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3570-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = BCDM-PRINCIPAL-YTD / 12
           COMPUTE WS-WORK-AMT-2 = BCDM-PRINCIPAL-YTD - (WS-WORK-AMT *
               12)
           COMPUTE BCDM-INTEREST-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN EXPIRY DATE IS PAST
      * DGU -> GWJ WHEN EXPIRY DATE IS PAST
      * GWJ -> RJL WHEN EXPIRY DATE IS PAST
      * RJL -> UPM WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3580-SET-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BCDT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E633 OPERATOR REQUIRED
      * E309 BUMP AMT NEGATIVE
      * E634 BUMP AMT OVER 99999
      * E505 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3600-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-OPERATOR = SPACES
                   MOVE 'E633' TO WS-REJECT-CODE
               WHEN BCDT-BUMP-AMT < 0
                   MOVE 'E309' TO WS-REJECT-CODE
               WHEN BCDT-BUMP-AMT > 99999.00
                   MOVE 'E634' TO WS-REJECT-CODE
               WHEN BCDM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INTEREST AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO ADDON YTD.
      *----------------------------------------------------------------*
       3610-PRORATE-ADDON.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BCDM-ADDON-YTD ROUNDED =
                   BCDT-INTEREST-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BCDM-ADDON-YTD
           END-IF
           ADD BCDM-ADDON-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE ELECTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3620-DESCRIBE-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'N'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'V'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3630-LIMIT-INTEREST.
           IF BCDM-INTEREST-AMT < 50.00
               MOVE 50.00 TO BCDM-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-INTEREST-AMT > 500.00
               MOVE 500.00 TO BCDM-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADDON AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3640-LIMIT-ADDON.
           IF BCDM-ADDON-AMT < 25.00
               MOVE 25.00 TO BCDM-ADDON-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-ADDON-AMT > 5000.00
               MOVE 5000.00 TO BCDM-ADDON-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3650-FLAG-INTEREST.
           IF BCDM-INTEREST-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDM-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT DUE DATE IS PAST
      * BAT -> MAL WHEN NEXT DUE DATE IS PAST
      * MAL -> TEL WHEN NEXT DUE DATE IS PAST
      * TEL -> ONL WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3660-SET-CHANNEL.
           EVALUATE BCDT-CHANNEL-CD
               WHEN 'BRN'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BCDM-NEXT-DUE-DATE > 0 AND BCDM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BCDT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E903 MEMO REQUIRED
      * E297 ADDON AMT NEGATIVE
      * E771 ADDON AMT OVER 10000
      * E175 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3670-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-MEMO = SPACES
                   MOVE 'E903' TO WS-REJECT-CODE
               WHEN BCDM-ADDON-AMT < 0
                   MOVE 'E297' TO WS-REJECT-CODE
               WHEN BCDM-ADDON-AMT > 10000.00
                   MOVE 'E771' TO WS-REJECT-CODE
               WHEN BCDT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E175' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3680-LIMIT-INTEREST.
           IF BCDM-INTEREST-AMT < 100.00
               MOVE 100.00 TO BCDM-INTEREST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-INTEREST-AMT > 500.00
               MOVE 500.00 TO BCDM-INTEREST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3690-LIMIT-PRINCIPAL.
           IF BCDM-PRINCIPAL-YTD < 10.00
               MOVE 10.00 TO BCDM-PRINCIPAL-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PRINCIPAL-YTD > 5000.00
               MOVE 5000.00 TO BCDM-PRINCIPAL-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INTEREST FEE ON THE ADDON YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3700-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BCDT-INTEREST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BCDT-INTEREST-AMT ROUNDED = BCDM-ADDON-YTD *
                       BCDM-PENALTY-PCT / 200
               WHEN BCDM-REGION-CD = 'BGJ'
                   MOVE 7.00 TO BCDT-INTEREST-AMT
               WHEN BCDM-REGION-CD = 'LFX'
                   MOVE 20.00 TO BCDT-INTEREST-AMT
               WHEN OTHER
                   COMPUTE BCDT-INTEREST-AMT ROUNDED = BCDM-ADDON-YTD *
                       BCDM-PENALTY-PCT / 100
           END-EVALUATE
           IF BCDT-INTEREST-AMT > 0 AND BCDT-INTEREST-AMT < 1.00
               MOVE 1.00 TO BCDT-INTEREST-AMT
           END-IF
           IF BCDT-INTEREST-AMT > 25.00
               MOVE 25.00 TO BCDT-INTEREST-AMT
           END-IF
           ADD BCDT-INTEREST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON FEE ON THE BUMP AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3710-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BCDT-ADDON-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   COMPUTE BCDT-ADDON-AMT ROUNDED = BCDM-BUMP-AMT *
                       BCDM-RATE / 200
               WHEN BCDM-CHANNEL-CD = 'EK'
                   MOVE 15.00 TO BCDT-ADDON-AMT
               WHEN BCDM-CHANNEL-CD = 'JE'
                   MOVE 5.00 TO BCDT-ADDON-AMT
               WHEN OTHER
                   COMPUTE BCDT-ADDON-AMT ROUNDED = BCDM-BUMP-AMT *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDT-ADDON-AMT > 0 AND BCDT-ADDON-AMT < 10.00
               MOVE 10.00 TO BCDT-ADDON-AMT
           END-IF
           IF BCDT-ADDON-AMT > 250.00
               MOVE 250.00 TO BCDT-ADDON-AMT
           END-IF
           ADD BCDT-ADDON-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3720-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-REVIEW-DATE > 0 AND BCDM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-BUMP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3730-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-REVIEW-DATE > 0 AND BCDM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-REVIEW-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINCIPAL YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3740-FLAG-PRINCIPAL.
           IF BCDM-PRINCIPAL-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BCDM-PRINCIPAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3750-COUNT-RETRY.
           IF BCDM-RETRY-CNT < 9999999
               ADD 1 TO BCDM-RETRY-CNT
           END-IF
           IF BCDM-RETRY-CNT NOT < 12
               MOVE 'Y' TO BCDM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BUMP FEE ON THE ADDON YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3760-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BCDT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BCDM-BUMP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   COMPUTE BCDM-BUMP-AMT ROUNDED = BCDM-ADDON-YTD *
                       BCDM-MARGIN / 200
               WHEN BCDT-TRAN-CD = 'BU'
                   MOVE 5.00 TO BCDM-BUMP-AMT
               WHEN BCDT-TRAN-CD = 'AD'
                   MOVE 10.00 TO BCDM-BUMP-AMT
               WHEN OTHER
                   COMPUTE BCDM-BUMP-AMT ROUNDED = BCDM-ADDON-YTD *
                       BCDM-MARGIN / 100
           END-EVALUATE
           IF BCDM-BUMP-AMT > 0 AND BCDM-BUMP-AMT < 2.00
               MOVE 2.00 TO BCDM-BUMP-AMT
           END-IF
           IF BCDM-BUMP-AMT > 35.00
               MOVE 35.00 TO BCDM-BUMP-AMT
           END-IF
           ADD BCDM-BUMP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-CHANNEL.
           EVALUATE BCDM-CHANNEL-CD
               WHEN 'EK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JE'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIERX TIER FOR THE PRINCIPAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3780-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIERX-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF BCDM-PRINCIPAL-YTD NOT < WS-INTEREST-TIERX-LOW(WS-TX)
                   MOVE WS-INTEREST-TIERX-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ADDON AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3790-FLAG-ADDON.
           IF BCDM-ADDON-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-ADDON-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3800-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * BCDM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BCDT-ADDON-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * BGJ -> LFX WHEN EXPIRY DATE IS PAST
      * LFX -> MLA WHEN EXPIRY DATE IS PAST
      * MLA -> ULA WHEN EXPIRY DATE IS PAST
      * ULA -> BGJ WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-REGION.
           EVALUATE BCDM-REGION-CD
               WHEN 'BGJ'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LFX' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LFX'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MLA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MLA'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ULA' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ULA'
                   IF BCDM-EXPIRY-DATE > 0 AND BCDM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BGJ' TO BCDM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3820-COUNT-SEQ.
           IF BCDT-SEQ < 9999999
               ADD 1 TO BCDT-SEQ
           END-IF
           IF BCDT-SEQ NOT < 10
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF BCDM-NEXT-REVIEW-DATE > 0 AND BCDM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E222 MEMO REQUIRED
      * E354 PENALTY AMT NEGATIVE
      * E465 PENALTY AMT OVER 99999
      * E664 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDT-MEMO = SPACES
                   MOVE 'E222' TO WS-REJECT-CODE
               WHEN BCDT-PENALTY-AMT < 0
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN BCDT-PENALTY-AMT > 99999.00
                   MOVE 'E465' TO WS-REJECT-CODE
               WHEN BCDM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E664' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ADDON FEE ON THE INTEREST AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3850-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BCDM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BCDT-ADDON-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-SUSPEND-FLAG = 'Y'
                   COMPUTE BCDT-ADDON-AMT ROUNDED = BCDT-INTEREST-AMT *
                       BCDM-RATE / 200
               WHEN BCDM-ELECTION-CD = 'L'
                   MOVE 20.00 TO BCDT-ADDON-AMT
               WHEN BCDM-ELECTION-CD = 'M'
                   MOVE 12.00 TO BCDT-ADDON-AMT
               WHEN OTHER
                   COMPUTE BCDT-ADDON-AMT ROUNDED = BCDT-INTEREST-AMT *
                       BCDM-RATE / 100
           END-EVALUATE
           IF BCDT-ADDON-AMT > 0 AND BCDT-ADDON-AMT < 10.00
               MOVE 10.00 TO BCDT-ADDON-AMT
           END-IF
           IF BCDT-ADDON-AMT > 25.00
               MOVE 25.00 TO BCDT-ADDON-AMT
           END-IF
           ADD BCDT-ADDON-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3860-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ADDON AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3870-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BCDT-CHANNEL-CD
                   ADD BCDM-ADDON-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BCDT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BCDM-ADDON-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E352 NAME REQUIRED
      * E168 ADDON AMT NEGATIVE
      * E438 ADDON AMT OVER 99999
      * E598 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3880-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-NAME = SPACES
                   MOVE 'E352' TO WS-REJECT-CODE
               WHEN BCDM-ADDON-AMT < 0
                   MOVE 'E168' TO WS-REJECT-CODE
               WHEN BCDM-ADDON-AMT > 99999.00
                   MOVE 'E438' TO WS-REJECT-CODE
               WHEN BCDM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PENALTY AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-PENALTY.
           COMPUTE WS-WORK-AMT = BCDT-PENALTY-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BCDT-PENALTY-AMT - (WS-WORK-AMT * 12)
           COMPUTE BCDM-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INTEREST AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3900-FLAG-INTEREST.
           IF BCDT-INTEREST-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BCDT-INTEREST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ELECTION CD:
      * L -> M WHEN NEXT REVIEW DATE IS PAST
      * M -> N WHEN NEXT REVIEW DATE IS PAST
      * N -> R WHEN NEXT REVIEW DATE IS PAST
      * R -> V WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-ELECTION.
           EVALUATE BCDM-ELECTION-CD
               WHEN 'L'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'N' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'V' TO BCDM-ELECTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON BUMP AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3920-ACCRUE-BUMP.
           IF BCDT-BUMP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BCDT-BUMP-AMT * BCDM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BCDM-INTEREST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PENALTY AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3930-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BCDM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BCDM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BCDM-PENALTY-AMT *
                       BCDM-RATE / 200
               WHEN BCDM-STEP-PLAN-CD = 'AYX'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN BCDM-STEP-PLAN-CD = 'DGU'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BCDM-PENALTY-AMT *
                       BCDM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE INTEREST TIER TIER FOR THE PENALTY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-INTEREST-TIER-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BCDM-PENALTY-YTD NOT < WS-INTEREST-TIER-LOW(WS-TX)
                   MOVE WS-INTEREST-TIER-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO BCDM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3960-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BCDM-ADDON-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE BUMP TIER TIER FOR THE INTEREST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-TIER-RATE(1) TO BCDM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF BCDM-INTEREST-YTD NOT < WS-BUMP-TIER-LOW(WS-TX)
                   MOVE WS-BUMP-TIER-RATE(WS-TX) TO BCDM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E598 DESC REQUIRED
      * E458 BUMP AMT NEGATIVE
      * E903 BUMP AMT OVER 25000
      * E304 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3980-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BCDM-DESC = SPACES
                   MOVE 'E598' TO WS-REJECT-CODE
               WHEN BCDT-BUMP-AMT < 0
                   MOVE 'E458' TO WS-REJECT-CODE
               WHEN BCDT-BUMP-AMT > 25000.00
                   MOVE 'E903' TO WS-REJECT-CODE
               WHEN BCDT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E304' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3990-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BCDM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BCDM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO BCDM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PENALTY AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4000-LIMIT-PENALTY.
           IF BCDM-PENALTY-AMT < 0.00
               MOVE 0.00 TO BCDM-PENALTY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BCDM-PENALTY-AMT > 2500.00
               MOVE 2500.00 TO BCDM-PENALTY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4010-COUNT-SEQ.
           IF BCDT-SEQ < 9999999
               ADD 1 TO BCDT-SEQ
           END-IF
           IF BCDT-SEQ NOT < 5
               MOVE 'Y' TO BCDM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4020-COUNT-RETRY.
           IF BCDM-RETRY-CNT < 9999999
               ADD 1 TO BCDM-RETRY-CNT
           END-IF
           IF BCDM-RETRY-CNT NOT < 3
               MOVE 'Y' TO BCDM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ADDON AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-ADDON.
           COMPUTE WS-WORK-AMT = BCDT-ADDON-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BCDT-ADDON-AMT - (WS-WORK-AMT * 4)
           COMPUTE BCDM-PENALTY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE BUMP TIER TIER FOR THE ADDON AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4040-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-BUMP-TIER-RATE(1) TO BCDM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF BCDT-ADDON-AMT NOT < WS-BUMP-TIER-LOW(WS-TX)
                   MOVE WS-BUMP-TIER-RATE(WS-TX) TO BCDM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BCDM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BCDM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BU -> AD WHEN LAST MAINT DATE IS PAST
      * AD -> WD WHEN LAST MAINT DATE IS PAST
      * WD -> MT WHEN LAST MAINT DATE IS PAST
      * MT -> BU WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4050-SET-TRAN.
           EVALUATE BCDT-TRAN-CD
               WHEN 'BU'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'AD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WD' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WD'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'MT' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MT'
                   IF BCDM-LAST-MAINT-DATE > 0 AND BCDM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BU' TO BCDT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4060-FLAG-PRINCIPAL.
           IF BCDM-PRINCIPAL-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BCDM-PRINCIPAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STEP PLAN CD:
      * AYX -> DGU WHEN NEXT REVIEW DATE IS PAST
      * DGU -> GWJ WHEN NEXT REVIEW DATE IS PAST
      * GWJ -> RJL WHEN NEXT REVIEW DATE IS PAST
      * RJL -> UPM WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4070-SET-STEP-PLAN.
           EVALUATE BCDM-STEP-PLAN-CD
               WHEN 'AYX'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DGU' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DGU'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GWJ' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GWJ'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RJL' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RJL'
                   IF BCDM-NEXT-REVIEW-DATE > 0 AND
                       BCDM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UPM' TO BCDM-STEP-PLAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE BCD FIELDS TO BUSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-BUSTAT.
           INITIALIZE BUSTAT-PARMS
           MOVE BCDM-PENALTY-YTD
             TO BUSTAT-PENALTY-YTD
           MOVE BCDM-ADDON-AMT
             TO BUSTAT-ADDON-AMT
           MOVE BCDM-BUMP-AMT
             TO BUSTAT-BUMP-AMT
           MOVE BCDM-PENALTY-AMT
             TO BUSTAT-PENALTY-AMT
           MOVE BCDM-PRINCIPAL-AMT
             TO BUSTAT-PRINCIPAL-AMT
           MOVE BCDM-INTEREST-YTD
             TO BUSTAT-INTEREST-YTD
           MOVE BCDM-LAST-PAY-DATE
             TO BUSTAT-LAST-PAY-DATE
           MOVE BCDM-EXPIRY-DATE
             TO BUSTAT-EXPIRY-DATE
           MOVE BCDM-MATURITY-DATE
             TO BUSTAT-MATURITY-DATE
           MOVE BCDM-LAST-STMT-DATE
             TO BUSTAT-LAST-STMT-DATE
           MOVE BCDM-ELECTION-CD
             TO BUSTAT-ELECTION-CD
           MOVE BCDM-CHANNEL-CD
             TO BUSTAT-CHANNEL-CD
           MOVE BCDM-SOURCE-CD
             TO BUSTAT-SOURCE-CD
           MOVE BCDM-STEP-PLAN-CD
             TO BUSTAT-STEP-PLAN-CD
           MOVE BCDM-OVERRIDE-FLAG
             TO BUSTAT-OVERRIDE-FLAG
           MOVE BCDM-REVIEW-FLAG
             TO BUSTAT-REVIEW-FLAG
           MOVE BCDM-STOP-FLAG
             TO BUSTAT-STOP-FLAG
           MOVE BCDM-CYCLE-CNT
             TO BUSTAT-CYCLE-CNT
           MOVE BCDM-ITEM-CNT
             TO BUSTAT-ITEM-CNT
           MOVE BCDM-PENALTY-PCT
             TO BUSTAT-PENALTY-PCT
           MOVE BCDM-RATE
             TO BUSTAT-RATE
           MOVE BCDM-ADDR
             TO BUSTAT-ADDR
           MOVE BCDM-NAME
             TO BUSTAT-NAME
           CALL 'BUSTAT' USING BUSTAT-PARMS
           EVALUATE BUSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BUSTAT-BUMP-AMT
                     TO BCDM-BUMP-AMT
                   MOVE BUSTAT-INTEREST-YTD
                     TO BCDM-INTEREST-YTD
                   MOVE BUSTAT-OVERRIDE-FLAG
                     TO BCDM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-BUSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE BCD FIELDS TO BUVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-BUVAL.
           INITIALIZE BUVAL-PARMS
           MOVE BCDM-PENALTY-AMT
             TO BUVAL-PENALTY-AMT
           MOVE BCDM-INTEREST-YTD
             TO BUVAL-INTEREST-YTD
           MOVE BCDM-BUMP-AMT
             TO BUVAL-BUMP-AMT
           MOVE BCDM-PRINCIPAL-YTD
             TO BUVAL-PRINCIPAL-YTD
           MOVE BCDM-ADDON-YTD
             TO BUVAL-ADDON-YTD
           MOVE BCDM-PRINCIPAL-AMT
             TO BUVAL-PRINCIPAL-AMT
           MOVE BCDM-LAST-STMT-DATE
             TO BUVAL-LAST-STMT-DATE
           MOVE BCDM-LAST-MAINT-DATE
             TO BUVAL-LAST-MAINT-DATE
           MOVE BCDM-EFFECTIVE-DATE
             TO BUVAL-EFFECTIVE-DATE
           MOVE BCDM-LAST-PAY-DATE
             TO BUVAL-LAST-PAY-DATE
           MOVE BCDM-STEP-PLAN-CD
             TO BUVAL-STEP-PLAN-CD
           MOVE BCDM-CHANNEL-CD
             TO BUVAL-CHANNEL-CD
           MOVE BCDM-SOURCE-CD
             TO BUVAL-SOURCE-CD
           MOVE BCDM-REGION-CD
             TO BUVAL-REGION-CD
           MOVE BCDM-EXEMPT-FLAG
             TO BUVAL-EXEMPT-FLAG
           MOVE BCDM-STOP-FLAG
             TO BUVAL-STOP-FLAG
           MOVE BCDM-OVERRIDE-FLAG
             TO BUVAL-OVERRIDE-FLAG
           MOVE BCDM-CYCLE-CNT
             TO BUVAL-CYCLE-CNT
           MOVE BCDM-ITEM-CNT
             TO BUVAL-ITEM-CNT
           MOVE BCDM-RATE
             TO BUVAL-RATE
           MOVE BCDM-MARGIN
             TO BUVAL-MARGIN
           MOVE BCDM-DESC
             TO BUVAL-DESC
           MOVE BCDM-NAME
             TO BUVAL-NAME
           CALL 'BUVAL' USING BUVAL-PARMS
           EVALUATE BUVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BUVAL-PENALTY-AMT
                     TO BCDM-PENALTY-AMT
                   MOVE BUVAL-PRINCIPAL-AMT
                     TO BCDM-PRINCIPAL-AMT
                   MOVE BUVAL-EXEMPT-FLAG
                     TO BCDM-EXEMPT-FLAG
                   MOVE BUVAL-OVERRIDE-FLAG
                     TO BCDM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-BUVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE BCDT-KEY TO RD-KEY
           MOVE BCDT-TRAN-CD TO RD-CODE
           MOVE BCDT-INTEREST-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE BCDT-KEY TO RD-KEY
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
           DISPLAY 'BUDEXTR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'BUDEXTR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
