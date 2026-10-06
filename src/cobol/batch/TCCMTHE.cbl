      *================================================================*
      * PROGRAM  : TCCMTHE                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TRAVELERS CHEQUES - MONTH-END. READS THE TRC        *
      *            TRANSACTIONS, UPDATES THE TRC MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TRAVELERS CHEQUE SALES,         *
      *            CONSIGNMENT INVENTORY AND REMITTANCE                *
      * INPUT    : TRCMST (KSDS), TRCTRN, BANKCTL                      *
      * OUTPUT   : TRCOUT, TCCMTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TCDTE, TCPRC, TCCAL     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-07-23 GAF  ORIGINAL                                       *
      * 1990-06-03 DPS  REMIT LIMIT REVISED CHG5247                    *
      * 1996-07-21 MKL  REFUND ROUNDING - HALF UP INC6651              *
      * 1999-09-26 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-05-17 BHS  REFUND FEE SCHEDULE UPDATE PRB3791             *
      * 2003-07-11 PQR  SHORTAGE CALCULATION CORRECTED REQ0988         *
      * 2004-01-28 EWS  FACE CALCULATION CORRECTED INC0791             *
      * 2006-02-13 EWS  REMIT CALCULATION CORRECTED PRB5965            *
      * 2008-04-02 SKN  NEW COMMISSION REPORT COLUMNS CHG7076          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCCMTHE.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TRCMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TRCM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TRCTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TRCOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TCCMTRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TRCMST.
       FD  TRAN-FILE.
       COPY TRCTRN.
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
                                           VALUE 18.000000.
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
      * WS-FACE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FACE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000449500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000786000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000617000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000412500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005550000000870500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000531500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006550000000765500'.
       01  WS-FACE-TIER-TBL REDEFINES WS-FACE-TIER-VALUES.
           05  WS-FACE-TIER-ROW OCCURS 7 TIMES.
               10  WS-FACE-TIER-LOW        PIC 9(09)V99.
               10  WS-FACE-TIER-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONSIGN-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONSIGN-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000651000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000315500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000297000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000308000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015550000000126500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015650000000065000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016650000000038000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016750000000017000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016800000000499000'.
       01  WS-CONSIGN-BAND-TBL REDEFINES WS-CONSIGN-BAND-VALUES.
           05  WS-CONSIGN-BAND-ROW OCCURS 9 TIMES.
               10  WS-CONSIGN-BAND-LOW     PIC 9(09)V99.
               10  WS-CONSIGN-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TCCMTHE  TRAVELERS CHEQUES'.
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
       COPY TCDTEP.
       COPY TCPRCP.
       COPY TCCALP.

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
               MOVE 'TRCMST' TO ABND-FILE-NAME
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
           MOVE TRCT-KEY TO TRCM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ACCUM-CHANNEL
               PERFORM 3010-ASSESS-STOP
               PERFORM 3020-ASSESS-STOP
               PERFORM 3030-COUNT-EXCEPT
               PERFORM 3040-LIMIT-FACE
               PERFORM 3050-SET-CYCLE
               PERFORM 3060-SELECT-MARGIN
               PERFORM 3070-ACCUM-CYCLE
               PERFORM 3080-SELECT-RATE
               PERFORM 3090-LIMIT-SHORTAGE
               PERFORM 3100-ACCUM-TRAN
               PERFORM 3110-AGE-LAST-STMT
               PERFORM 3120-AGE-LAST-ACT
               PERFORM 3130-SET-TRAN
               PERFORM 3140-PRORATE-COMMISSION
               PERFORM 3150-CHECK-NEXT-REVIEW
               PERFORM 3160-ACCRUE-FACE
               PERFORM 3170-LIMIT-REFUND
               PERFORM 3180-ACCUM-DENOM
               PERFORM 3190-COUNT-EXCEPT
               PERFORM 3200-AGE-EFF
               PERFORM 3210-CHECK-MATURITY
               PERFORM 3220-COUNT-CYCLE
               PERFORM 3230-PRORATE-REFUND
               PERFORM 3240-PRORATE-REFUND
               PERFORM 3250-EDIT-OPERATOR
               PERFORM 3260-PRORATE-REMIT
               PERFORM 3270-AGE-LAST-STMT
               PERFORM 3280-ACCRUE-REMIT
               PERFORM 3290-ACCUM-CHANNEL
               PERFORM 3300-AGE-POST
               PERFORM 3310-EDIT-NAME
               PERFORM 3320-ACCRUE-CONSIGN
               PERFORM 3330-ASSESS-NOTICE-SENT
               PERFORM 3340-SELECT-MARGIN
               PERFORM 3350-ASSESS-REVERSAL
               PERFORM 3360-SELECT-MARGIN
               PERFORM 3370-FLAG-CONSIGN
               PERFORM 3380-LIMIT-CONSIGN
               PERFORM 3390-FLAG-SHORTAGE
               PERFORM 3400-ACCUM-ISSUER
               PERFORM 3410-EDIT-REJECT
               PERFORM 3420-DESCRIBE-SOURCE
               PERFORM 3430-ACCUM-DENOM
               PERFORM 3440-LIMIT-REMIT
               PERFORM 3450-ASSESS-STOP
               PERFORM 3460-PRORATE-CONSIGN
               PERFORM 3470-SPLIT-REMIT
               PERFORM 3480-ASSESS-STOP
               PERFORM 3490-COUNT-EXCEPT
               PERFORM 3500-EDIT-REF-NO
               PERFORM 3510-FLAG-CONSIGN
               PERFORM 3520-EDIT-NAME
               PERFORM 3530-ACCUM-STATUS
               PERFORM 3540-PRORATE-REMIT
               PERFORM 3550-ACCRUE-REMIT
               PERFORM 3560-FLAG-SHORTAGE
               PERFORM 3570-EDIT-MEMO
               PERFORM 3580-LIMIT-CONSIGN
               PERFORM 3590-ACCRUE-REFUND
               PERFORM 3600-FLAG-REMIT
               PERFORM 3610-LIMIT-SHORTAGE
               PERFORM 3620-FLAG-CONSIGN
               PERFORM 3630-DESCRIBE-ISSUER
               PERFORM 3640-ACCRUE-REMIT
               PERFORM 3650-ASSESS-OVERRIDE
               PERFORM 3660-EDIT-MEMO
               PERFORM 3670-AGE-EFFECTIVE
               PERFORM 3680-AGE-EXPIRY
               PERFORM 3690-FLAG-CONSIGN
               PERFORM 3700-FLAG-REFUND
               PERFORM 3710-COUNT-CYCLE
               PERFORM 3720-LIMIT-ITEM
               PERFORM 3730-FLAG-FACE
               PERFORM 3740-ACCRUE-REMIT
               PERFORM 3750-COUNT-SEQ
               PERFORM 3760-PRORATE-REMIT
               PERFORM 3770-SELECT-FEE-PCT
               PERFORM 3780-COUNT-ITEM
               PERFORM 3790-COUNT-ITEM
               PERFORM 3800-SPLIT-REMIT
               PERFORM 3810-ACCRUE-CONSIGN
               PERFORM 3820-SET-ISSUER
               PERFORM 3830-SPLIT-COMMISSION
               PERFORM 3840-EDIT-REJECT
               PERFORM 3850-LIMIT-FACE
               PERFORM 3860-DESCRIBE-STATUS
               PERFORM 3870-LIMIT-REMIT
               PERFORM 3880-EDIT-REJECT
               PERFORM 3890-EDIT-OPERATOR
               PERFORM 3900-CHECK-EFFECTIVE
               PERFORM 3910-EDIT-MEMO
               PERFORM 3920-SET-SOURCE
               PERFORM 3930-DESCRIBE-STATUS
               PERFORM 3940-ACCRUE-CONSIGN
               PERFORM 3950-SPLIT-COMMISSION
               PERFORM 3960-SELECT-FEE-PCT
               PERFORM 3970-CHECK-EXPIRY
               PERFORM 3980-PRORATE-FACE
               PERFORM 3990-ASSESS-REVIEW
               PERFORM 4000-ASSESS-EXEMPT
               PERFORM 4010-PRORATE-FACE
               PERFORM 4020-FLAG-CONSIGN
               PERFORM 4030-PRORATE-REFUND
               PERFORM 4040-ASSESS-SUSPEND
               PERFORM 4050-SET-STATUS
               PERFORM 4060-SPLIT-REFUND
               PERFORM 4070-ACCRUE-MARGIN
               PERFORM 4080-ASSESS-SUSPEND
               PERFORM 4090-ASSESS-SUSPEND
               PERFORM 4100-ACCRUE-FACE
               PERFORM 4110-CHECK-EXPIRY
               PERFORM 4120-EDIT-REJECT
               PERFORM 4130-CHECK-MATURITY
               PERFORM 4140-CHECK-LAST-STMT
               PERFORM 4150-ASSESS-EXEMPT
               PERFORM 4160-COUNT-EXCEPT
               PERFORM 4170-SET-STATUS
               PERFORM 4180-SPLIT-SHORTAGE
               PERFORM 4190-DESCRIBE-ISSUER
               PERFORM 4200-CHECK-NEXT-DUE
               PERFORM 4210-SET-SOURCE
               PERFORM 4220-ACCRUE-RATE
               PERFORM 4230-SPLIT-COMMISSION
               PERFORM 4240-SET-CYCLE
               PERFORM 4250-DESCRIBE-BRANCH
               PERFORM 4260-ACCRUE-REFUND
               PERFORM 4270-CHECK-POST
               PERFORM 4280-COUNT-EXCEPT
               PERFORM 4290-EDIT-REF-NO
               PERFORM 4300-CHECK-LAST-PAY
               PERFORM 7010-CALL-TCDTE
               PERFORM 7020-CALL-TCPRC
               PERFORM 7000-CALL-TCCAL
               REWRITE TRC-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TRCMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TRC-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ACCUMULATE CONSIGN AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCT-CHANNEL-CD
                   ADD TRCM-CONSIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-CONSIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE YTD FEE ON THE COMMISSION YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3010-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRCM-SHORTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRCM-SHORTAGE-YTD ROUNDED =
                       TRCM-COMMISSION-YTD * TRCM-FEE-PCT / 200
               WHEN TRCM-CYCLE-CD = 'EPW'
                   MOVE 12.00 TO TRCM-SHORTAGE-YTD
               WHEN TRCM-CYCLE-CD = 'HHK'
                   MOVE 20.00 TO TRCM-SHORTAGE-YTD
               WHEN OTHER
                   COMPUTE TRCM-SHORTAGE-YTD ROUNDED =
                       TRCM-COMMISSION-YTD * TRCM-FEE-PCT / 100
           END-EVALUATE
           IF TRCM-SHORTAGE-YTD > 0 AND TRCM-SHORTAGE-YTD < 1.00
               MOVE 1.00 TO TRCM-SHORTAGE-YTD
           END-IF
           IF TRCM-SHORTAGE-YTD > 75.00
               MOVE 75.00 TO TRCM-SHORTAGE-YTD
           END-IF
           ADD TRCM-SHORTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE COMMISSION YTD FEE ON THE REMIT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3020-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRCM-COMMISSION-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-REVIEW-FLAG = 'Y'
                   COMPUTE TRCM-COMMISSION-YTD ROUNDED = TRCM-REMIT-AMT
                       * TRCM-MARGIN / 200
               WHEN TRCM-CYCLE-CD = 'EPW'
                   MOVE 20.00 TO TRCM-COMMISSION-YTD
               WHEN TRCM-CYCLE-CD = 'HHK'
                   MOVE 5.00 TO TRCM-COMMISSION-YTD
               WHEN OTHER
                   COMPUTE TRCM-COMMISSION-YTD ROUNDED = TRCM-REMIT-AMT
                       * TRCM-MARGIN / 100
           END-EVALUATE
           IF TRCM-COMMISSION-YTD > 0 AND TRCM-COMMISSION-YTD < 2.00
               MOVE 2.00 TO TRCM-COMMISSION-YTD
           END-IF
           IF TRCM-COMMISSION-YTD > 75.00
               MOVE 75.00 TO TRCM-COMMISSION-YTD
           END-IF
           ADD TRCM-COMMISSION-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-EXCEPT.
           IF TRCM-EXCEPT-CNT < 9999999
               ADD 1 TO TRCM-EXCEPT-CNT
           END-IF
           IF TRCM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO TRCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FACE YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3040-LIMIT-FACE.
           IF TRCM-FACE-YTD < 100.00
               MOVE 100.00 TO TRCM-FACE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-FACE-YTD > 50000.00
               MOVE 50000.00 TO TRCM-FACE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN EXPIRY DATE IS PAST
      * HHK -> JCB WHEN EXPIRY DATE IS PAST
      * JCB -> UCB WHEN EXPIRY DATE IS PAST
      * UCB -> XTY WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-CYCLE.
           EVALUATE TRCM-CYCLE-CD
               WHEN 'EPW'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HHK' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HHK'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JCB'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UCB'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XTY' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3060-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-BAND-RATE(1) TO TRCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-CONSIGN-BAND-LOW(WS-TX)
                   MOVE WS-CONSIGN-BAND-RATE(WS-TX) TO TRCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COMMISSION AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-CYCLE-CD
                   ADD TRCM-COMMISSION-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-COMMISSION-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE TIER TIER FOR THE SHORTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-TIER-RATE(1) TO TRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRCM-SHORTAGE-AMT NOT < WS-FACE-TIER-LOW(WS-TX)
                   MOVE WS-FACE-TIER-RATE(WS-TX) TO TRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3090-LIMIT-SHORTAGE.
           IF TRCM-SHORTAGE-YTD < 10.00
               MOVE 10.00 TO TRCM-SHORTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-SHORTAGE-YTD > 10000.00
               MOVE 10000.00 TO TRCM-SHORTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCT-TRAN-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3110-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-REMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRCM-REMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRCM-REMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRCM-REMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-REMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SL -> RF WHEN LAST STMT DATE IS PAST
      * RF -> RM WHEN LAST STMT DATE IS PAST
      * RM -> CN WHEN LAST STMT DATE IS PAST
      * CN -> VD WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3130-SET-TRAN.
           EVALUATE TRCT-TRAN-CD
               WHEN 'SL'
                   IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RF'
                   IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RM'
                   IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REFUND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COMMISSION AMT.
      *----------------------------------------------------------------*
       3140-PRORATE-COMMISSION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-COMMISSION-AMT ROUNDED =
                   TRCM-REFUND-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-COMMISSION-AMT
           END-IF
           ADD TRCM-COMMISSION-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TRCM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TRCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-FACE.
           IF TRCM-FACE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-FACE-AMT * TRCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TRCM-CONSIGN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3170-LIMIT-REFUND.
           IF TRCM-REFUND-YTD < 25.00
               MOVE 25.00 TO TRCM-REFUND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-REFUND-YTD > 50000.00
               MOVE 50000.00 TO TRCM-REFUND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONSIGN YTD BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-DENOM-CD
                   ADD TRCM-CONSIGN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-CONSIGN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-EXCEPT.
           IF TRCM-EXCEPT-CNT < 9999999
               ADD 1 TO TRCM-EXCEPT-CNT
           END-IF
           IF TRCM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF TRCT-EFF-DATE > 0 AND TRCT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TRCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3220-COUNT-CYCLE.
           IF TRCM-CYCLE-CNT < 9999999
               ADD 1 TO TRCM-CYCLE-CNT
           END-IF
           IF TRCM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO TRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       3230-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCT-REFUND-AMT ROUNDED =
                   TRCM-SHORTAGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCT-REFUND-AMT
           END-IF
           ADD TRCT-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE COMMISSION AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND YTD.
      *----------------------------------------------------------------*
       3240-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-REFUND-YTD ROUNDED =
                   TRCM-COMMISSION-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCM-REFUND-YTD
           END-IF
           ADD TRCM-REFUND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E117 OPERATOR REQUIRED
      * E925 WORK AMT 2 NEGATIVE
      * E920 WORK AMT 2 OVER 25000
      * E827 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-OPERATOR = SPACES
                   MOVE 'E117' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E925' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 25000.00
                   MOVE 'E920' TO WS-REJECT-CODE
               WHEN TRCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E827' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REMIT YTD.
      *----------------------------------------------------------------*
       3260-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-REMIT-YTD ROUNDED =
                   TRCM-REMIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCM-REMIT-YTD
           END-IF
           ADD TRCM-REMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3270-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-REMIT.
           IF TRCM-REMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-REMIT-AMT * TRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3290-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCT-CHANNEL-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF TRCT-POST-DATE > 0 AND TRCT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-REMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TRCM-REMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRCM-REMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCM-REMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-REMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E654 NAME REQUIRED
      * E382 COMMISSION AMT NEGATIVE
      * E636 COMMISSION AMT OVER 250000
      * E154 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3310-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-NAME = SPACES
                   MOVE 'E654' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-AMT < 0
                   MOVE 'E382' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-AMT > 250000.00
                   MOVE 'E636' TO WS-REJECT-CODE
               WHEN TRCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E154' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONSIGN YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3320-ACCRUE-CONSIGN.
           IF TRCM-CONSIGN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-CONSIGN-YTD * TRCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TRCM-REFUND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COMMISSION YTD FEE ON THE REMIT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3330-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN TRCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO TRCM-COMMISSION-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-EXEMPT-FLAG = 'Y'
                   COMPUTE TRCM-COMMISSION-YTD ROUNDED = TRCT-REMIT-AMT
                       * TRCM-RATE / 200
               WHEN TRCT-TRAN-CD = 'SL'
                   MOVE 20.00 TO TRCM-COMMISSION-YTD
               WHEN TRCT-TRAN-CD = 'RF'
                   MOVE 10.00 TO TRCM-COMMISSION-YTD
               WHEN OTHER
                   COMPUTE TRCM-COMMISSION-YTD ROUNDED = TRCT-REMIT-AMT
                       * TRCM-RATE / 100
           END-EVALUATE
           IF TRCM-COMMISSION-YTD > 0 AND TRCM-COMMISSION-YTD < 5.00
               MOVE 5.00 TO TRCM-COMMISSION-YTD
           END-IF
           IF TRCM-COMMISSION-YTD > 150.00
               MOVE 150.00 TO TRCM-COMMISSION-YTD
           END-IF
           ADD TRCM-COMMISSION-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN BAND TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3340-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-BAND-RATE(1) TO TRCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRCT-REFUND-AMT NOT < WS-CONSIGN-BAND-LOW(WS-TX)
                   MOVE WS-CONSIGN-BAND-RATE(WS-TX) TO TRCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONSIGN YTD FEE ON THE FACE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3350-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRCM-CONSIGN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRCM-CONSIGN-YTD ROUNDED = TRCM-FACE-YTD *
                       TRCM-MARGIN / 200
               WHEN TRCM-SOURCE-CD = 'EG'
                   MOVE 10.00 TO TRCM-CONSIGN-YTD
               WHEN TRCM-SOURCE-CD = 'GH'
                   MOVE 3.00 TO TRCM-CONSIGN-YTD
               WHEN OTHER
                   COMPUTE TRCM-CONSIGN-YTD ROUNDED = TRCM-FACE-YTD *
                       TRCM-MARGIN / 100
           END-EVALUATE
           IF TRCM-CONSIGN-YTD > 0 AND TRCM-CONSIGN-YTD < 10.00
               MOVE 10.00 TO TRCM-CONSIGN-YTD
           END-IF
           IF TRCM-CONSIGN-YTD > 150.00
               MOVE 150.00 TO TRCM-CONSIGN-YTD
           END-IF
           ADD TRCM-CONSIGN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN BAND TIER FOR THE CONSIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3360-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-BAND-RATE(1) TO TRCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRCT-CONSIGN-AMT NOT < WS-CONSIGN-BAND-LOW(WS-TX)
                   MOVE WS-CONSIGN-BAND-RATE(WS-TX) TO TRCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3370-FLAG-CONSIGN.
           IF TRCM-CONSIGN-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCM-CONSIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3380-LIMIT-CONSIGN.
           IF TRCT-CONSIGN-AMT < 0.00
               MOVE 0.00 TO TRCT-CONSIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCT-CONSIGN-AMT > 5000.00
               MOVE 5000.00 TO TRCT-CONSIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3390-FLAG-SHORTAGE.
           IF TRCM-SHORTAGE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-SHORTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY ISSUER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3400-ACCUM-ISSUER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-ISSUER-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-ISSUER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E020 REJECT CD REQUIRED
      * E669 CONSIGN AMT NEGATIVE
      * E753 CONSIGN AMT OVER 250000
      * E199 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-REJECT-CD = SPACES
                   MOVE 'E020' TO WS-REJECT-CODE
               WHEN TRCM-CONSIGN-AMT < 0
                   MOVE 'E669' TO WS-REJECT-CODE
               WHEN TRCM-CONSIGN-AMT > 250000.00
                   MOVE 'E753' TO WS-REJECT-CODE
               WHEN TRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E199' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3420-DESCRIBE-SOURCE.
           EVALUATE TRCM-SOURCE-CD
               WHEN 'EG'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CONSIGN AMT BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-DENOM-CD
                   ADD TRCT-CONSIGN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCT-CONSIGN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * REMIT YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3440-LIMIT-REMIT.
           IF TRCM-REMIT-YTD < 10.00
               MOVE 10.00 TO TRCM-REMIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-REMIT-YTD > 50000.00
               MOVE 50000.00 TO TRCM-REMIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FACE YTD FEE ON THE CONSIGN AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3450-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRCM-FACE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRCM-FACE-YTD ROUNDED = TRCM-CONSIGN-AMT *
                       TRCM-FEE-PCT / 200
               WHEN TRCT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO TRCM-FACE-YTD
               WHEN TRCT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO TRCM-FACE-YTD
               WHEN OTHER
                   COMPUTE TRCM-FACE-YTD ROUNDED = TRCM-CONSIGN-AMT *
                       TRCM-FEE-PCT / 100
           END-EVALUATE
           IF TRCM-FACE-YTD > 0 AND TRCM-FACE-YTD < 2.00
               MOVE 2.00 TO TRCM-FACE-YTD
           END-IF
           IF TRCM-FACE-YTD > 25.00
               MOVE 25.00 TO TRCM-FACE-YTD
           END-IF
           ADD TRCM-FACE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONSIGN YTD.
      *----------------------------------------------------------------*
       3460-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-CONSIGN-YTD ROUNDED =
                   TRCM-SHORTAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-CONSIGN-YTD
           END-IF
           ADD TRCM-CONSIGN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT REMIT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3470-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = TRCM-REMIT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRCM-REMIT-AMT - (WS-WORK-AMT * 12)
           COMPUTE TRCM-CONSIGN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND YTD FEE ON THE COMMISSION AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3480-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRCM-REFUND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRCM-REFUND-YTD ROUNDED = TRCM-COMMISSION-AMT
                       * TRCM-RATE / 200
               WHEN TRCT-TRAN-CD = 'SL'
                   MOVE 3.00 TO TRCM-REFUND-YTD
               WHEN TRCT-TRAN-CD = 'RF'
                   MOVE 3.00 TO TRCM-REFUND-YTD
               WHEN OTHER
                   COMPUTE TRCM-REFUND-YTD ROUNDED = TRCM-COMMISSION-AMT
                       * TRCM-RATE / 100
           END-EVALUATE
           IF TRCM-REFUND-YTD > 0 AND TRCM-REFUND-YTD < 2.00
               MOVE 2.00 TO TRCM-REFUND-YTD
           END-IF
           IF TRCM-REFUND-YTD > 100.00
               MOVE 100.00 TO TRCM-REFUND-YTD
           END-IF
           ADD TRCM-REFUND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3490-COUNT-EXCEPT.
           IF TRCM-EXCEPT-CNT < 9999999
               ADD 1 TO TRCM-EXCEPT-CNT
           END-IF
           IF TRCM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO TRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E657 REF NO REQUIRED
      * E568 SHORTAGE AMT NEGATIVE
      * E203 SHORTAGE AMT OVER 50000
      * E714 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3500-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-REF-NO = SPACES
                   MOVE 'E657' TO WS-REJECT-CODE
               WHEN TRCM-SHORTAGE-AMT < 0
                   MOVE 'E568' TO WS-REJECT-CODE
               WHEN TRCM-SHORTAGE-AMT > 50000.00
                   MOVE 'E203' TO WS-REJECT-CODE
               WHEN TRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E714' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3510-FLAG-CONSIGN.
           IF TRCT-CONSIGN-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCT-CONSIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E375 NAME REQUIRED
      * E609 SHORTAGE YTD NEGATIVE
      * E343 SHORTAGE YTD OVER 5000
      * E142 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3520-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-NAME = SPACES
                   MOVE 'E375' TO WS-REJECT-CODE
               WHEN TRCM-SHORTAGE-YTD < 0
                   MOVE 'E609' TO WS-REJECT-CODE
               WHEN TRCM-SHORTAGE-YTD > 5000.00
                   MOVE 'E343' TO WS-REJECT-CODE
               WHEN TRCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E142' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COMMISSION YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3530-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-STATUS-CD
                   ADD TRCM-COMMISSION-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-COMMISSION-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REMIT AMT.
      *----------------------------------------------------------------*
       3540-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-REMIT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-REMIT-AMT
           END-IF
           ADD TRCM-REMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3550-ACCRUE-REMIT.
           IF TRCT-REMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCT-REMIT-AMT * TRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRCM-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3560-FLAG-SHORTAGE.
           IF TRCM-SHORTAGE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-SHORTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E604 MEMO REQUIRED
      * E101 COMMISSION YTD NEGATIVE
      * E674 COMMISSION YTD OVER 50000
      * E916 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3570-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-MEMO = SPACES
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-YTD < 0
                   MOVE 'E101' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-YTD > 50000.00
                   MOVE 'E674' TO WS-REJECT-CODE
               WHEN TRCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E916' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3580-LIMIT-CONSIGN.
           IF TRCM-CONSIGN-AMT < 100.00
               MOVE 100.00 TO TRCM-CONSIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-CONSIGN-AMT > 10000.00
               MOVE 10000.00 TO TRCM-CONSIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3590-ACCRUE-REFUND.
           IF TRCM-REFUND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-REFUND-YTD * TRCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TRCM-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REMIT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3600-FLAG-REMIT.
           IF TRCM-REMIT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCM-REMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3610-LIMIT-SHORTAGE.
           IF TRCM-SHORTAGE-AMT < 0.00
               MOVE 0.00 TO TRCM-SHORTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-SHORTAGE-AMT > 10000.00
               MOVE 10000.00 TO TRCM-SHORTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3620-FLAG-CONSIGN.
           IF TRCM-CONSIGN-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-CONSIGN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3640-ACCRUE-REMIT.
           IF TRCM-REMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-REMIT-AMT * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRCM-CONSIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE CONSIGN AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3650-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TRCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TRCT-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-STOP-FLAG = 'Y'
                   COMPUTE TRCT-REFUND-AMT ROUNDED = TRCM-CONSIGN-AMT *
                       TRCM-MARGIN / 200
               WHEN TRCM-DENOM-CD = 'G'
                   MOVE 12.00 TO TRCT-REFUND-AMT
               WHEN TRCM-DENOM-CD = 'M'
                   MOVE 3.00 TO TRCT-REFUND-AMT
               WHEN OTHER
                   COMPUTE TRCT-REFUND-AMT ROUNDED = TRCM-CONSIGN-AMT *
                       TRCM-MARGIN / 100
           END-EVALUATE
           IF TRCT-REFUND-AMT > 0 AND TRCT-REFUND-AMT < 1.00
               MOVE 1.00 TO TRCT-REFUND-AMT
           END-IF
           IF TRCT-REFUND-AMT > 150.00
               MOVE 150.00 TO TRCT-REFUND-AMT
           END-IF
           ADD TRCT-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E096 MEMO REQUIRED
      * E582 CONSIGN AMT NEGATIVE
      * E781 CONSIGN AMT OVER 50000
      * E431 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-MEMO = SPACES
                   MOVE 'E096' TO WS-REJECT-CODE
               WHEN TRCM-CONSIGN-AMT < 0
                   MOVE 'E582' TO WS-REJECT-CODE
               WHEN TRCM-CONSIGN-AMT > 50000.00
                   MOVE 'E781' TO WS-REJECT-CODE
               WHEN TRCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E431' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3670-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCT-REMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONSIGN AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3690-FLAG-CONSIGN.
           IF TRCT-CONSIGN-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCT-CONSIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * REFUND YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3700-FLAG-REFUND.
           IF TRCM-REFUND-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-REFUND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3710-COUNT-CYCLE.
           IF TRCM-CYCLE-CNT < 9999999
               ADD 1 TO TRCM-CYCLE-CNT
           END-IF
           IF TRCM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO TRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3720-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3730-FLAG-FACE.
           IF TRCM-FACE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRCM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-REMIT.
           IF TRCM-REMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-REMIT-AMT * TRCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TRCM-FACE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3750-COUNT-SEQ.
           IF TRCT-SEQ < 9999999
               ADD 1 TO TRCT-SEQ
           END-IF
           IF TRCT-SEQ NOT < 3
               MOVE 'Y' TO TRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3760-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TRCM-REMIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN BAND TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3770-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-BAND-RATE(1) TO TRCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRCM-REFUND-AMT NOT < WS-CONSIGN-BAND-LOW(WS-TX)
                   MOVE WS-CONSIGN-BAND-RATE(WS-TX) TO TRCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3780-COUNT-ITEM.
           IF TRCM-ITEM-CNT < 9999999
               ADD 1 TO TRCM-ITEM-CNT
           END-IF
           IF TRCM-ITEM-CNT NOT < 12
               MOVE 'Y' TO TRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3790-COUNT-ITEM.
           IF TRCM-ITEM-CNT < 9999999
               ADD 1 TO TRCM-ITEM-CNT
           END-IF
           IF TRCM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REMIT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3800-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = TRCM-REMIT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRCM-REMIT-AMT - (WS-WORK-AMT * 4)
           COMPUTE TRCM-REMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONSIGN AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-CONSIGN.
           IF TRCT-CONSIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCT-CONSIGN-AMT * TRCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRCM-COMMISSION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ISSUER CD:
      * AF -> CJ WHEN EFFECTIVE DATE IS PAST
      * CJ -> DU WHEN EFFECTIVE DATE IS PAST
      * DU -> JY WHEN EFFECTIVE DATE IS PAST
      * JY -> RR WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3820-SET-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJ' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJ'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DU'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JY' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF TRCM-EFFECTIVE-DATE > 0 AND TRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RR' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCT-COMMISSION-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRCT-COMMISSION-AMT - (WS-WORK-AMT *
               4)
           COMPUTE TRCM-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E781 REJECT CD REQUIRED
      * E748 COMMISSION AMT NEGATIVE
      * E812 COMMISSION AMT OVER 10000
      * E935 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3840-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-REJECT-CD = SPACES
                   MOVE 'E781' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-AMT < 0
                   MOVE 'E748' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-AMT > 10000.00
                   MOVE 'E812' TO WS-REJECT-CODE
               WHEN TRCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E935' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FACE YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3850-LIMIT-FACE.
           IF TRCM-FACE-YTD < 100.00
               MOVE 100.00 TO TRCM-FACE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-FACE-YTD > 2500.00
               MOVE 2500.00 TO TRCM-FACE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3860-DESCRIBE-STATUS.
           EVALUATE TRCM-STATUS-CD
               WHEN 'HK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REMIT YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3870-LIMIT-REMIT.
           IF TRCM-REMIT-YTD < 0.00
               MOVE 0.00 TO TRCM-REMIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-REMIT-YTD > 2500.00
               MOVE 2500.00 TO TRCM-REMIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E749 REJECT CD REQUIRED
      * E031 COMMISSION YTD NEGATIVE
      * E922 COMMISSION YTD OVER 99999
      * E267 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3880-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-REJECT-CD = SPACES
                   MOVE 'E749' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-YTD < 0
                   MOVE 'E031' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-YTD > 99999.00
                   MOVE 'E922' TO WS-REJECT-CODE
               WHEN TRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E754 OPERATOR REQUIRED
      * E979 CONSIGN AMT NEGATIVE
      * E328 CONSIGN AMT OVER 10000
      * E227 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-OPERATOR = SPACES
                   MOVE 'E754' TO WS-REJECT-CODE
               WHEN TRCM-CONSIGN-AMT < 0
                   MOVE 'E979' TO WS-REJECT-CODE
               WHEN TRCM-CONSIGN-AMT > 10000.00
                   MOVE 'E328' TO WS-REJECT-CODE
               WHEN TRCM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E227' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TRCM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E474 MEMO REQUIRED
      * E475 REMIT AMT NEGATIVE
      * E864 REMIT AMT OVER 99999
      * E671 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-MEMO = SPACES
                   MOVE 'E474' TO WS-REJECT-CODE
               WHEN TRCT-REMIT-AMT < 0
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN TRCT-REMIT-AMT > 99999.00
                   MOVE 'E864' TO WS-REJECT-CODE
               WHEN TRCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E671' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * EG -> GH WHEN NEXT DUE DATE IS PAST
      * GH -> LC WHEN NEXT DUE DATE IS PAST
      * LC -> LU WHEN NEXT DUE DATE IS PAST
      * LU -> YS WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3920-SET-SOURCE.
           EVALUATE TRCM-SOURCE-CD
               WHEN 'EG'
                   IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GH' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GH'
                   IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LC' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LC'
                   IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LU' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LU'
                   IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YS' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-STATUS.
           EVALUATE TRCM-STATUS-CD
               WHEN 'HK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONSIGN AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3940-ACCRUE-CONSIGN.
           IF TRCM-CONSIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-CONSIGN-AMT * TRCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TRCT-CONSIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3950-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCM-COMMISSION-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TRCM-COMMISSION-AMT - (WS-WORK-AMT *
               3)
           COMPUTE TRCM-FACE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE TIER TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-TIER-RATE(1) TO TRCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TRCT-REFUND-AMT NOT < WS-FACE-TIER-LOW(WS-TX)
                   MOVE WS-FACE-TIER-RATE(WS-TX) TO TRCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3970-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TRCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REFUND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FACE AMT.
      *----------------------------------------------------------------*
       3980-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-FACE-AMT ROUNDED =
                   TRCT-REFUND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-FACE-AMT
           END-IF
           ADD TRCM-FACE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE SHORTAGE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3990-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TRCM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TRCM-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TRCM-REFUND-AMT ROUNDED = TRCM-SHORTAGE-AMT *
                       TRCM-MARGIN / 200
               WHEN TRCT-TRAN-CD = 'SL'
                   MOVE 15.00 TO TRCM-REFUND-AMT
               WHEN TRCT-TRAN-CD = 'RF'
                   MOVE 5.00 TO TRCM-REFUND-AMT
               WHEN OTHER
                   COMPUTE TRCM-REFUND-AMT ROUNDED = TRCM-SHORTAGE-AMT *
                       TRCM-MARGIN / 100
           END-EVALUATE
           IF TRCM-REFUND-AMT > 0 AND TRCM-REFUND-AMT < 10.00
               MOVE 10.00 TO TRCM-REFUND-AMT
           END-IF
           IF TRCM-REFUND-AMT > 100.00
               MOVE 100.00 TO TRCM-REFUND-AMT
           END-IF
           ADD TRCM-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CONSIGN YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4000-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRCM-CONSIGN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRCM-CONSIGN-YTD ROUNDED = WS-WORK-AMT-2 *
                       TRCM-FEE-PCT / 200
               WHEN TRCM-STATUS-CD = 'HK'
                   MOVE 10.00 TO TRCM-CONSIGN-YTD
               WHEN TRCM-STATUS-CD = 'RU'
                   MOVE 12.00 TO TRCM-CONSIGN-YTD
               WHEN OTHER
                   COMPUTE TRCM-CONSIGN-YTD ROUNDED = WS-WORK-AMT-2 *
                       TRCM-FEE-PCT / 100
           END-EVALUATE
           IF TRCM-CONSIGN-YTD > 0 AND TRCM-CONSIGN-YTD < 10.00
               MOVE 10.00 TO TRCM-CONSIGN-YTD
           END-IF
           IF TRCM-CONSIGN-YTD > 100.00
               MOVE 100.00 TO TRCM-CONSIGN-YTD
           END-IF
           ADD TRCM-CONSIGN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE REFUND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FACE YTD.
      *----------------------------------------------------------------*
       4010-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-FACE-YTD ROUNDED =
                   TRCM-REFUND-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-FACE-YTD
           END-IF
           ADD TRCM-FACE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CONSIGN AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4020-FLAG-CONSIGN.
           IF TRCM-CONSIGN-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-CONSIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REFUND YTD.
      *----------------------------------------------------------------*
       4030-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-REFUND-YTD ROUNDED =
                   TRCM-FACE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-REFUND-YTD
           END-IF
           ADD TRCM-REFUND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE COMMISSION YTD FEE ON THE CONSIGN AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4040-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRCM-COMMISSION-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRCM-COMMISSION-YTD ROUNDED =
                       TRCM-CONSIGN-AMT * TRCM-MARGIN / 200
               WHEN TRCM-CYCLE-CD = 'EPW'
                   MOVE 3.00 TO TRCM-COMMISSION-YTD
               WHEN TRCM-CYCLE-CD = 'HHK'
                   MOVE 5.00 TO TRCM-COMMISSION-YTD
               WHEN OTHER
                   COMPUTE TRCM-COMMISSION-YTD ROUNDED =
                       TRCM-CONSIGN-AMT * TRCM-MARGIN / 100
           END-EVALUATE
           IF TRCM-COMMISSION-YTD > 0 AND TRCM-COMMISSION-YTD < 2.00
               MOVE 2.00 TO TRCM-COMMISSION-YTD
           END-IF
           IF TRCM-COMMISSION-YTD > 25.00
               MOVE 25.00 TO TRCM-COMMISSION-YTD
           END-IF
           ADD TRCM-COMMISSION-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * HK -> RU WHEN EXPIRY DATE IS PAST
      * RU -> WA WHEN EXPIRY DATE IS PAST
      * WA -> HK WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4050-SET-STATUS.
           EVALUATE TRCM-STATUS-CD
               WHEN 'HK'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO TRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WA' TO TRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WA'
                   IF TRCM-EXPIRY-DATE > 0 AND TRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HK' TO TRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REFUND YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4060-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = TRCM-REFUND-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TRCM-REFUND-YTD - (WS-WORK-AMT * 4)
           COMPUTE TRCT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4070-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRCM-FACE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COMMISSION FEE ON THE REMIT YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4080-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRCT-COMMISSION-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-REVIEW-FLAG = 'Y'
                   COMPUTE TRCT-COMMISSION-AMT ROUNDED = TRCM-REMIT-YTD
                       * TRCM-FEE-PCT / 200
               WHEN TRCT-TRAN-CD = 'SL'
                   MOVE 3.00 TO TRCT-COMMISSION-AMT
               WHEN TRCT-TRAN-CD = 'RF'
                   MOVE 15.00 TO TRCT-COMMISSION-AMT
               WHEN OTHER
                   COMPUTE TRCT-COMMISSION-AMT ROUNDED = TRCM-REMIT-YTD
                       * TRCM-FEE-PCT / 100
           END-EVALUATE
           IF TRCT-COMMISSION-AMT > 0 AND TRCT-COMMISSION-AMT < 10.00
               MOVE 10.00 TO TRCT-COMMISSION-AMT
           END-IF
           IF TRCT-COMMISSION-AMT > 25.00
               MOVE 25.00 TO TRCT-COMMISSION-AMT
           END-IF
           ADD TRCT-COMMISSION-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CONSIGN FEE ON THE CONSIGN YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4090-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRCT-CONSIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCT-REVERSAL-FLAG = 'Y'
                   COMPUTE TRCT-CONSIGN-AMT ROUNDED = TRCM-CONSIGN-YTD *
                       TRCM-MARGIN / 200
               WHEN TRCT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO TRCT-CONSIGN-AMT
               WHEN TRCT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO TRCT-CONSIGN-AMT
               WHEN OTHER
                   COMPUTE TRCT-CONSIGN-AMT ROUNDED = TRCM-CONSIGN-YTD *
                       TRCM-MARGIN / 100
           END-EVALUATE
           IF TRCT-CONSIGN-AMT > 0 AND TRCT-CONSIGN-AMT < 5.00
               MOVE 5.00 TO TRCT-CONSIGN-AMT
           END-IF
           IF TRCT-CONSIGN-AMT > 35.00
               MOVE 35.00 TO TRCT-CONSIGN-AMT
           END-IF
           ADD TRCT-CONSIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4100-ACCRUE-FACE.
           IF TRCM-FACE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-FACE-AMT * TRCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRCM-REMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4110-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E495 REJECT CD REQUIRED
      * E718 CONSIGN AMT NEGATIVE
      * E504 CONSIGN AMT OVER 99999
      * E122 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4120-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-REJECT-CD = SPACES
                   MOVE 'E495' TO WS-REJECT-CODE
               WHEN TRCM-CONSIGN-AMT < 0
                   MOVE 'E718' TO WS-REJECT-CODE
               WHEN TRCM-CONSIGN-AMT > 99999.00
                   MOVE 'E504' TO WS-REJECT-CODE
               WHEN TRCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E122' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4130-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4140-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TRCM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO TRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FACE FEE ON THE SHORTAGE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4150-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRCM-FACE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRCM-FACE-AMT ROUNDED = TRCM-SHORTAGE-AMT *
                       TRCM-RATE / 200
               WHEN TRCT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO TRCM-FACE-AMT
               WHEN TRCT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO TRCM-FACE-AMT
               WHEN OTHER
                   COMPUTE TRCM-FACE-AMT ROUNDED = TRCM-SHORTAGE-AMT *
                       TRCM-RATE / 100
           END-EVALUATE
           IF TRCM-FACE-AMT > 0 AND TRCM-FACE-AMT < 1.00
               MOVE 1.00 TO TRCM-FACE-AMT
           END-IF
           IF TRCM-FACE-AMT > 75.00
               MOVE 75.00 TO TRCM-FACE-AMT
           END-IF
           ADD TRCM-FACE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4160-COUNT-EXCEPT.
           IF TRCM-EXCEPT-CNT < 9999999
               ADD 1 TO TRCM-EXCEPT-CNT
           END-IF
           IF TRCM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * HK -> RU WHEN LAST STMT DATE IS PAST
      * RU -> WA WHEN LAST STMT DATE IS PAST
      * WA -> HK WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4170-SET-STATUS.
           EVALUATE TRCM-STATUS-CD
               WHEN 'HK'
                   IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RU' TO TRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WA' TO TRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WA'
                   IF TRCM-LAST-STMT-DATE > 0 AND TRCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HK' TO TRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4180-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE TRCM-SHORTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4190-DESCRIBE-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4200-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TRCM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * EG -> GH WHEN LAST PAY DATE IS PAST
      * GH -> LC WHEN LAST PAY DATE IS PAST
      * LC -> LU WHEN LAST PAY DATE IS PAST
      * LU -> YS WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-SOURCE.
           EVALUATE TRCM-SOURCE-CD
               WHEN 'EG'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GH' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GH'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LC' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LC'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LU' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LU'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YS' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4220-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * TRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRCM-CONSIGN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4230-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCM-COMMISSION-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TRCM-COMMISSION-YTD - (WS-WORK-AMT *
               4)
           COMPUTE TRCM-REMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN NEXT DUE DATE IS PAST
      * HHK -> JCB WHEN NEXT DUE DATE IS PAST
      * JCB -> UCB WHEN NEXT DUE DATE IS PAST
      * UCB -> XTY WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4240-SET-CYCLE.
           EVALUATE TRCM-CYCLE-CD
               WHEN 'EPW'
                   IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HHK' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HHK'
                   IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JCB'
                   IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UCB'
                   IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XTY' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4250-DESCRIBE-BRANCH.
           EVALUATE TRCM-BRANCH-CD
               WHEN 'EG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4260-ACCRUE-REFUND.
           IF TRCM-REFUND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-REFUND-YTD * TRCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TRCM-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4270-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF TRCT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO TRCM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4280-COUNT-EXCEPT.
           IF TRCM-EXCEPT-CNT < 9999999
               ADD 1 TO TRCM-EXCEPT-CNT
           END-IF
           IF TRCM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO TRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E167 REF NO REQUIRED
      * E429 CONSIGN AMT NEGATIVE
      * E789 CONSIGN AMT OVER 10000
      * E128 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4290-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-REF-NO = SPACES
                   MOVE 'E167' TO WS-REJECT-CODE
               WHEN TRCM-CONSIGN-AMT < 0
                   MOVE 'E429' TO WS-REJECT-CODE
               WHEN TRCM-CONSIGN-AMT > 10000.00
                   MOVE 'E789' TO WS-REJECT-CODE
               WHEN TRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E128' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4300-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO TRCM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE TRC FIELDS TO TCDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-TCDTE.
           INITIALIZE TCDTE-PARMS
           MOVE TRCM-SHORTAGE-AMT
             TO TCDTE-SHORTAGE-AMT
           MOVE TRCM-COMMISSION-YTD
             TO TCDTE-COMMISSION-YTD
           MOVE TRCM-REFUND-YTD
             TO TCDTE-REFUND-YTD
           MOVE TRCM-SHORTAGE-YTD
             TO TCDTE-SHORTAGE-YTD
           MOVE TRCM-CONSIGN-AMT
             TO TCDTE-CONSIGN-AMT
           MOVE TRCM-REFUND-AMT
             TO TCDTE-REFUND-AMT
           MOVE TRCM-MATURITY-DATE
             TO TCDTE-MATURITY-DATE
           MOVE TRCM-CLOSE-DATE
             TO TCDTE-CLOSE-DATE
           MOVE TRCM-NEXT-DUE-DATE
             TO TCDTE-NEXT-DUE-DATE
           MOVE TRCM-LAST-ACT-DATE
             TO TCDTE-LAST-ACT-DATE
           MOVE TRCM-BRANCH-CD
             TO TCDTE-BRANCH-CD
           MOVE TRCM-ISSUER-CD
             TO TCDTE-ISSUER-CD
           MOVE TRCM-CYCLE-CD
             TO TCDTE-CYCLE-CD
           MOVE TRCM-STATUS-CD
             TO TCDTE-STATUS-CD
           MOVE TRCM-STOP-FLAG
             TO TCDTE-STOP-FLAG
           MOVE TRCM-OVERRIDE-FLAG
             TO TCDTE-OVERRIDE-FLAG
           MOVE TRCM-REVIEW-FLAG
             TO TCDTE-REVIEW-FLAG
           MOVE TRCM-EXCEPT-CNT
             TO TCDTE-EXCEPT-CNT
           MOVE TRCM-ITEM-CNT
             TO TCDTE-ITEM-CNT
           MOVE TRCM-RATE
             TO TCDTE-RATE
           MOVE TRCM-MARGIN
             TO TCDTE-MARGIN
           MOVE TRCM-REF-NO
             TO TCDTE-REF-NO
           MOVE TRCM-CUST-ID
             TO TCDTE-CUST-ID
           CALL 'TCDTE' USING TCDTE-PARMS
           EVALUATE TCDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TCDTE-SHORTAGE-YTD
                     TO TRCM-SHORTAGE-YTD
                   MOVE TCDTE-CONSIGN-AMT
                     TO TRCM-CONSIGN-AMT
                   MOVE TCDTE-OVERRIDE-FLAG
                     TO TRCM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-TCDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TRC FIELDS TO TCPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-TCPRC.
           INITIALIZE TCPRC-PARMS
           MOVE TRCM-REMIT-YTD
             TO TCPRC-REMIT-YTD
           MOVE TRCM-REFUND-YTD
             TO TCPRC-REFUND-YTD
           MOVE TRCM-CONSIGN-YTD
             TO TCPRC-CONSIGN-YTD
           MOVE TRCM-SHORTAGE-YTD
             TO TCPRC-SHORTAGE-YTD
           MOVE TRCM-FACE-YTD
             TO TCPRC-FACE-YTD
           MOVE TRCM-CONSIGN-AMT
             TO TCPRC-CONSIGN-AMT
           MOVE TRCM-MATURITY-DATE
             TO TCPRC-MATURITY-DATE
           MOVE TRCM-CLOSE-DATE
             TO TCPRC-CLOSE-DATE
           MOVE TRCM-NEXT-REVIEW-DATE
             TO TCPRC-NEXT-REVIEW-DATE
           MOVE TRCM-EXPIRY-DATE
             TO TCPRC-EXPIRY-DATE
           MOVE TRCM-STATUS-CD
             TO TCPRC-STATUS-CD
           MOVE TRCM-BRANCH-CD
             TO TCPRC-BRANCH-CD
           MOVE TRCM-CYCLE-CD
             TO TCPRC-CYCLE-CD
           MOVE TRCM-ISSUER-CD
             TO TCPRC-ISSUER-CD
           MOVE TRCM-REVIEW-FLAG
             TO TCPRC-REVIEW-FLAG
           MOVE TRCM-NOTICE-SENT-FLAG
             TO TCPRC-NOTICE-SENT-FLAG
           MOVE TRCM-SUSPEND-FLAG
             TO TCPRC-SUSPEND-FLAG
           MOVE TRCM-EXCEPT-CNT
             TO TCPRC-EXCEPT-CNT
           MOVE TRCM-CYCLE-CNT
             TO TCPRC-CYCLE-CNT
           MOVE TRCM-FEE-PCT
             TO TCPRC-FEE-PCT
           MOVE TRCM-MARGIN
             TO TCPRC-MARGIN
           MOVE TRCM-NAME
             TO TCPRC-NAME
           MOVE TRCM-CONTACT
             TO TCPRC-CONTACT
           CALL 'TCPRC' USING TCPRC-PARMS
           EVALUATE TCPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TCPRC-REMIT-YTD
                     TO TRCM-REMIT-YTD
                   MOVE TCPRC-REFUND-YTD
                     TO TRCM-REFUND-YTD
                   MOVE TCPRC-CONSIGN-YTD
                     TO TRCM-CONSIGN-YTD
                   MOVE TCPRC-CONSIGN-AMT
                     TO TRCM-CONSIGN-AMT
                   MOVE TCPRC-SUSPEND-FLAG
                     TO TRCM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-TCPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TRC FIELDS TO TCCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-TCCAL.
           INITIALIZE TCCAL-PARMS
           MOVE TRCM-COMMISSION-YTD
             TO TCCAL-COMMISSION-YTD
           MOVE TRCM-SHORTAGE-YTD
             TO TCCAL-SHORTAGE-YTD
           MOVE TRCM-COMMISSION-AMT
             TO TCCAL-COMMISSION-AMT
           MOVE TRCM-SHORTAGE-AMT
             TO TCCAL-SHORTAGE-AMT
           MOVE TRCM-CONSIGN-AMT
             TO TCCAL-CONSIGN-AMT
           MOVE TRCM-CONSIGN-YTD
             TO TCCAL-CONSIGN-YTD
           MOVE TRCM-MATURITY-DATE
             TO TCCAL-MATURITY-DATE
           MOVE TRCM-LAST-ACT-DATE
             TO TCCAL-LAST-ACT-DATE
           MOVE TRCM-EFFECTIVE-DATE
             TO TCCAL-EFFECTIVE-DATE
           MOVE TRCM-NEXT-DUE-DATE
             TO TCCAL-NEXT-DUE-DATE
           MOVE TRCM-BRANCH-CD
             TO TCCAL-BRANCH-CD
           MOVE TRCM-DENOM-CD
             TO TCCAL-DENOM-CD
           MOVE TRCM-ISSUER-CD
             TO TCCAL-ISSUER-CD
           MOVE TRCM-SOURCE-CD
             TO TCCAL-SOURCE-CD
           MOVE TRCM-EXEMPT-FLAG
             TO TCCAL-EXEMPT-FLAG
           MOVE TRCM-REVIEW-FLAG
             TO TCCAL-REVIEW-FLAG
           MOVE TRCM-STOP-FLAG
             TO TCCAL-STOP-FLAG
           MOVE TRCM-EXCEPT-CNT
             TO TCCAL-EXCEPT-CNT
           MOVE TRCM-ITEM-CNT
             TO TCCAL-ITEM-CNT
           MOVE TRCM-RATE
             TO TCCAL-RATE
           MOVE TRCM-FEE-PCT
             TO TCCAL-FEE-PCT
           MOVE TRCM-NAME
             TO TCCAL-NAME
           MOVE TRCM-ADDR
             TO TCCAL-ADDR
           CALL 'TCCAL' USING TCCAL-PARMS
           EVALUATE TCCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TCCAL-CONSIGN-YTD
                     TO TRCM-CONSIGN-YTD
                   MOVE TCCAL-EXEMPT-FLAG
                     TO TRCM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-TCCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TRCT-KEY TO RD-KEY
           MOVE TRCT-TRAN-CD TO RD-CODE
           MOVE TRCT-COMMISSION-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TRCT-KEY TO RD-KEY
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
           DISPLAY 'TCCMTHE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TCCMTHE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
