      *================================================================*
      * PROGRAM  : TCCYREN                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TRAVELERS CHEQUES - YEAR-END. READS THE TRC         *
      *            TRANSACTIONS, UPDATES THE TRC MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TRAVELERS CHEQUE SALES,         *
      *            CONSIGNMENT INVENTORY AND REMITTANCE                *
      * INPUT    : TRCMST (KSDS), TRCTRN, BANKCTL                      *
      * OUTPUT   : TRCOUT, TCCYRRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TCDTE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-07-19 SKN  ORIGINAL                                       *
      * 1997-11-02 DPS  REFUND LIMIT REVISED INC1517                   *
      * 1998-06-26 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-05-01 DPS  PERFORMANCE - CONSIGN READ AHEAD PRB7141       *
      * 2000-09-07 LMB  SHORTAGE FEE SCHEDULE UPDATE INC4940           *
      * 2007-06-09 JLM  FACE CALCULATION CORRECTED REQ8622             *
      * 2012-04-19 TWH  NEW REMIT REPORT COLUMNS INC4846               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCCYREN.
       AUTHOR. T W HOLT.

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
           SELECT REPORT-FILE ASSIGN TO "TCCYRRPT"
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
                                           VALUE 17.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 28.
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
      * WS-CONSIGN-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONSIGN-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000851500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000042500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000861000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000800000000198500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000100500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002050000000803500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002300000000474000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002800000000157000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003300000000337000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000258500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000100500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004600000000111000'.
       01  WS-CONSIGN-GRID-TBL REDEFINES WS-CONSIGN-GRID-VALUES.
           05  WS-CONSIGN-GRID-ROW OCCURS 12 TIMES.
               10  WS-CONSIGN-GRID-LOW     PIC 9(09)V99.
               10  WS-CONSIGN-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONSIGN-GRIDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONSIGN-GRIDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000096500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000112000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000202000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000800000000559000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000900000000751500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000950000000744000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000026500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000308000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000792500'.
       01  WS-CONSIGN-GRIDX-TBL REDEFINES WS-CONSIGN-GRIDX-VALUES.
           05  WS-CONSIGN-GRIDX-ROW OCCURS 9 TIMES.
               10  WS-CONSIGN-GRIDX-LOW    PIC 9(09)V99.
               10  WS-CONSIGN-GRIDX-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CONSIGN-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CONSIGN-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000885000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000819000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000578500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000560500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008600000000719000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009100000000236000'.
       01  WS-CONSIGN-BAND-TBL REDEFINES WS-CONSIGN-BAND-VALUES.
           05  WS-CONSIGN-BAND-ROW OCCURS 6 TIMES.
               10  WS-CONSIGN-BAND-LOW     PIC 9(09)V99.
               10  WS-CONSIGN-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TCCYREN  TRAVELERS CHEQUES'.
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
               PERFORM 3000-SELECT-MARGIN
               PERFORM 3010-AGE-NEXT-DUE
               PERFORM 3020-COUNT-EXCEPT
               PERFORM 3030-COUNT-ITEM
               PERFORM 3040-PRORATE-FACE
               PERFORM 3050-ACCRUE-CONSIGN
               PERFORM 3060-SET-CYCLE
               PERFORM 3070-FLAG-COMMISSION
               PERFORM 3080-SET-SOURCE
               PERFORM 3090-ASSESS-SUSPEND
               PERFORM 3100-ASSESS-STOP
               PERFORM 3110-EDIT-REJECT
               PERFORM 3120-PRORATE-FACE
               PERFORM 3130-ACCRUE-COMMISSION
               PERFORM 3140-SET-CYCLE
               PERFORM 3150-SELECT-RATE
               PERFORM 3160-COUNT-SEQ
               PERFORM 3170-FLAG-SHORTAGE
               PERFORM 3180-SPLIT-REMIT
               PERFORM 3190-PRORATE-SHORTAGE
               PERFORM 3200-SELECT-RATE
               PERFORM 3210-FLAG-CONSIGN
               PERFORM 3220-FLAG-ITEM
               PERFORM 3230-PRORATE-REFUND
               PERFORM 3240-FLAG-CONSIGN
               PERFORM 3250-CHECK-POST
               PERFORM 3260-PRORATE-CONSIGN
               PERFORM 3270-AGE-LAST-PAY
               PERFORM 3280-FLAG-COMMISSION
               PERFORM 3290-SET-BRANCH
               PERFORM 3300-SPLIT-REMIT
               PERFORM 3310-SELECT-RATE
               PERFORM 3320-PRORATE-REFUND
               PERFORM 3330-ASSESS-EXEMPT
               PERFORM 3340-LIMIT-REFUND
               PERFORM 3350-COUNT-EXCEPT
               PERFORM 3360-SPLIT-COMMISSION
               PERFORM 3370-ASSESS-REVIEW
               PERFORM 3380-LIMIT-REFUND
               PERFORM 3390-SELECT-MARGIN
               PERFORM 3400-LIMIT-CONSIGN
               PERFORM 3410-EDIT-MEMO
               PERFORM 3420-COUNT-CYCLE
               PERFORM 3430-PRORATE-CONSIGN
               PERFORM 3440-EDIT-OPERATOR
               PERFORM 3450-DESCRIBE-CHANNEL
               PERFORM 3460-AGE-NEXT-REVIEW
               PERFORM 3470-ACCRUE-FACE
               PERFORM 3480-DESCRIBE-ISSUER
               PERFORM 3490-PRORATE-SHORTAGE
               PERFORM 3500-AGE-NEXT-DUE
               PERFORM 3510-ASSESS-OVERRIDE
               PERFORM 3520-SET-TRAN
               PERFORM 3530-LIMIT-FACE
               PERFORM 3540-PRORATE-CONSIGN
               PERFORM 3550-SPLIT-REFUND
               PERFORM 3560-FLAG-SHORTAGE
               PERFORM 3570-PRORATE-COMMISSION
               PERFORM 3580-DESCRIBE-CYCLE
               PERFORM 3590-SELECT-RATE
               PERFORM 3600-EDIT-CUST-ID
               PERFORM 3610-SET-CYCLE
               PERFORM 3620-SELECT-RATE
               PERFORM 3630-ASSESS-SUSPEND
               PERFORM 3640-PRORATE-FACE
               PERFORM 3650-EDIT-DESC
               PERFORM 3660-AGE-LAST-STMT
               PERFORM 3670-SPLIT-REFUND
               PERFORM 3680-SELECT-MARGIN
               PERFORM 3690-ACCUM-DENOM
               PERFORM 3700-ACCRUE-CONSIGN
               PERFORM 3710-FLAG-FACE
               PERFORM 3720-AGE-NEXT-DUE
               PERFORM 3730-ASSESS-EXEMPT
               PERFORM 3740-SELECT-FEE-PCT
               PERFORM 3750-AGE-MATURITY
               PERFORM 3760-LIMIT-REMIT
               PERFORM 3770-COUNT-ITEM
               PERFORM 3780-SPLIT-SHORTAGE
               PERFORM 3790-AGE-CLOSE
               PERFORM 3800-SELECT-RATE
               PERFORM 3810-SET-CHANNEL
               PERFORM 3820-LIMIT-REMIT
               PERFORM 3830-SELECT-FEE-PCT
               PERFORM 3840-ACCUM-CHANNEL
               PERFORM 3850-CHECK-POST
               PERFORM 3860-DESCRIBE-SOURCE
               PERFORM 3870-CHECK-CLOSE
               PERFORM 3880-LIMIT-REMIT
               PERFORM 3890-COUNT-ITEM
               PERFORM 3900-DESCRIBE-TRAN
               PERFORM 3910-SPLIT-COMMISSION
               PERFORM 3920-COUNT-RETRY
               PERFORM 3930-COUNT-EXCEPT
               PERFORM 3940-PRORATE-COMMISSION
               PERFORM 3950-ACCUM-DENOM
               PERFORM 3960-SET-ISSUER
               PERFORM 3970-ACCRUE-SHORTAGE
               PERFORM 3980-CHECK-NEXT-DUE
               PERFORM 3990-ACCUM-STATUS
               PERFORM 4000-DESCRIBE-ISSUER
               PERFORM 4010-DESCRIBE-TRAN
               PERFORM 7010-CALL-TCDTE
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
      * SELECT THE CONSIGN GRID TIER FOR THE FACE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3000-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-GRID-RATE(1) TO TRCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TRCM-FACE-YTD NOT < WS-CONSIGN-GRID-LOW(WS-TX)
                   MOVE WS-CONSIGN-GRID-RATE(WS-TX) TO TRCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCT-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRCT-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRCT-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRCT-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCT-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-EXCEPT.
           IF TRCM-EXCEPT-CNT < 9999999
               ADD 1 TO TRCM-EXCEPT-CNT
           END-IF
           IF TRCM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO TRCM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-ITEM.
           IF TRCM-ITEM-CNT < 9999999
               ADD 1 TO TRCM-ITEM-CNT
           END-IF
           IF TRCM-ITEM-CNT NOT < 12
               MOVE 'Y' TO TRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FACE YTD.
      *----------------------------------------------------------------*
       3040-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-FACE-YTD ROUNDED =
                   TRCM-FACE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-FACE-YTD
           END-IF
           ADD TRCM-FACE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONSIGN AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3050-ACCRUE-CONSIGN.
           IF TRCT-CONSIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCT-CONSIGN-AMT * TRCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TRCM-CONSIGN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN CLOSE DATE IS PAST
      * HHK -> JCB WHEN CLOSE DATE IS PAST
      * JCB -> UCB WHEN CLOSE DATE IS PAST
      * UCB -> XTY WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3060-SET-CYCLE.
           EVALUATE TRCM-CYCLE-CD
               WHEN 'EPW'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HHK' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HHK'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JCB'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UCB'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XTY' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COMMISSION AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3070-FLAG-COMMISSION.
           IF TRCT-COMMISSION-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCT-COMMISSION-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * EG -> GH WHEN POST DATE IS PAST
      * GH -> LC WHEN POST DATE IS PAST
      * LC -> LU WHEN POST DATE IS PAST
      * LU -> YS WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3080-SET-SOURCE.
           EVALUATE TRCM-SOURCE-CD
               WHEN 'EG'
                   IF TRCT-POST-DATE > 0 AND TRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GH' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GH'
                   IF TRCT-POST-DATE > 0 AND TRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LC' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LC'
                   IF TRCT-POST-DATE > 0 AND TRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LU' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LU'
                   IF TRCT-POST-DATE > 0 AND TRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YS' TO TRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CONSIGN FEE ON THE CONSIGN AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3090-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRCM-CONSIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRCM-CONSIGN-AMT ROUNDED = TRCT-CONSIGN-AMT *
                       TRCM-FEE-PCT / 200
               WHEN TRCM-DENOM-CD = 'G'
                   MOVE 15.00 TO TRCM-CONSIGN-AMT
               WHEN TRCM-DENOM-CD = 'M'
                   MOVE 15.00 TO TRCM-CONSIGN-AMT
               WHEN OTHER
                   COMPUTE TRCM-CONSIGN-AMT ROUNDED = TRCT-CONSIGN-AMT *
                       TRCM-FEE-PCT / 100
           END-EVALUATE
           IF TRCM-CONSIGN-AMT > 0 AND TRCM-CONSIGN-AMT < 5.00
               MOVE 5.00 TO TRCM-CONSIGN-AMT
           END-IF
           IF TRCM-CONSIGN-AMT > 100.00
               MOVE 100.00 TO TRCM-CONSIGN-AMT
           END-IF
           ADD TRCM-CONSIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE REMIT YTD FEE ON THE FACE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3100-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRCM-REMIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TRCM-REMIT-YTD ROUNDED = TRCM-FACE-YTD *
                       TRCM-RATE / 200
               WHEN TRCM-CYCLE-CD = 'EPW'
                   MOVE 3.00 TO TRCM-REMIT-YTD
               WHEN TRCM-CYCLE-CD = 'HHK'
                   MOVE 12.00 TO TRCM-REMIT-YTD
               WHEN OTHER
                   COMPUTE TRCM-REMIT-YTD ROUNDED = TRCM-FACE-YTD *
                       TRCM-RATE / 100
           END-EVALUATE
           IF TRCM-REMIT-YTD > 0 AND TRCM-REMIT-YTD < 1.00
               MOVE 1.00 TO TRCM-REMIT-YTD
           END-IF
           IF TRCM-REMIT-YTD > 35.00
               MOVE 35.00 TO TRCM-REMIT-YTD
           END-IF
           ADD TRCM-REMIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E860 REJECT CD REQUIRED
      * E439 CONSIGN AMT NEGATIVE
      * E720 CONSIGN AMT OVER 50000
      * E858 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3110-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-REJECT-CD = SPACES
                   MOVE 'E860' TO WS-REJECT-CODE
               WHEN TRCM-CONSIGN-AMT < 0
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN TRCM-CONSIGN-AMT > 50000.00
                   MOVE 'E720' TO WS-REJECT-CODE
               WHEN TRCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E858' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3120-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TRCM-FACE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COMMISSION YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3130-ACCRUE-COMMISSION.
           IF TRCM-COMMISSION-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-COMMISSION-YTD * TRCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO TRCT-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN EFF DATE IS PAST
      * HHK -> JCB WHEN EFF DATE IS PAST
      * JCB -> UCB WHEN EFF DATE IS PAST
      * UCB -> XTY WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3140-SET-CYCLE.
           EVALUATE TRCM-CYCLE-CD
               WHEN 'EPW'
                   IF TRCT-EFF-DATE > 0 AND TRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HHK' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HHK'
                   IF TRCT-EFF-DATE > 0 AND TRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JCB'
                   IF TRCT-EFF-DATE > 0 AND TRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UCB' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UCB'
                   IF TRCT-EFF-DATE > 0 AND TRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XTY' TO TRCM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN GRIDX TIER FOR THE REMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-GRIDX-RATE(1) TO TRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRCM-REMIT-AMT NOT < WS-CONSIGN-GRIDX-LOW(WS-TX)
                   MOVE WS-CONSIGN-GRIDX-RATE(WS-TX) TO TRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3160-COUNT-SEQ.
           IF TRCT-SEQ < 9999999
               ADD 1 TO TRCT-SEQ
           END-IF
           IF TRCT-SEQ NOT < 3
               MOVE 'Y' TO TRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3170-FLAG-SHORTAGE.
           IF TRCM-SHORTAGE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-SHORTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REMIT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3180-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = TRCM-REMIT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TRCM-REMIT-AMT - (WS-WORK-AMT * 2)
           COMPUTE TRCM-FACE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHORTAGE AMT.
      *----------------------------------------------------------------*
       3190-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-SHORTAGE-AMT ROUNDED =
                   TRCM-REMIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-SHORTAGE-AMT
           END-IF
           ADD TRCM-SHORTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN GRID TIER FOR THE REMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-GRID-RATE(1) TO TRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TRCM-REMIT-AMT NOT < WS-CONSIGN-GRID-LOW(WS-TX)
                   MOVE WS-CONSIGN-GRID-RATE(WS-TX) TO TRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3210-FLAG-CONSIGN.
           IF TRCM-CONSIGN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRCM-CONSIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3220-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COMMISSION YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND YTD.
      *----------------------------------------------------------------*
       3230-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-REFUND-YTD ROUNDED =
                   TRCM-COMMISSION-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCM-REFUND-YTD
           END-IF
           ADD TRCM-REFUND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CONSIGN AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3240-FLAG-CONSIGN.
           IF TRCM-CONSIGN-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-CONSIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3250-CHECK-POST.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO TRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONSIGN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CONSIGN YTD.
      *----------------------------------------------------------------*
       3260-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-CONSIGN-YTD ROUNDED =
                   TRCT-CONSIGN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCM-CONSIGN-YTD
           END-IF
           ADD TRCM-CONSIGN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3270-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCT-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TRCT-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRCT-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCT-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCT-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COMMISSION AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3280-FLAG-COMMISSION.
           IF TRCT-COMMISSION-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRCT-COMMISSION-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * EG -> HZ WHEN LAST ACT DATE IS PAST
      * HZ -> JF WHEN LAST ACT DATE IS PAST
      * JF -> NZ WHEN LAST ACT DATE IS PAST
      * NZ -> EG WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3290-SET-BRANCH.
           EVALUATE TRCM-BRANCH-CD
               WHEN 'EG'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HZ' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HZ'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JF' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EG' TO TRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REMIT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3300-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = TRCT-REMIT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TRCT-REMIT-AMT - (WS-WORK-AMT * 3)
           COMPUTE TRCT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN GRID TIER FOR THE REFUND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3310-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-GRID-RATE(1) TO TRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TRCM-REFUND-YTD NOT < WS-CONSIGN-GRID-LOW(WS-TX)
                   MOVE WS-CONSIGN-GRID-RATE(WS-TX) TO TRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       3320-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCT-REFUND-AMT ROUNDED =
                   TRCM-FACE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCT-REFUND-AMT
           END-IF
           ADD TRCT-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE COMMISSION AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3330-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRCT-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRCT-REFUND-AMT ROUNDED = TRCT-COMMISSION-AMT
                       * TRCM-RATE / 200
               WHEN TRCM-DENOM-CD = 'G'
                   MOVE 7.00 TO TRCT-REFUND-AMT
               WHEN TRCM-DENOM-CD = 'M'
                   MOVE 7.00 TO TRCT-REFUND-AMT
               WHEN OTHER
                   COMPUTE TRCT-REFUND-AMT ROUNDED = TRCT-COMMISSION-AMT
                       * TRCM-RATE / 100
           END-EVALUATE
           IF TRCT-REFUND-AMT > 0 AND TRCT-REFUND-AMT < 2.00
               MOVE 2.00 TO TRCT-REFUND-AMT
           END-IF
           IF TRCT-REFUND-AMT > 50.00
               MOVE 50.00 TO TRCT-REFUND-AMT
           END-IF
           ADD TRCT-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3340-LIMIT-REFUND.
           IF TRCM-REFUND-AMT < 50.00
               MOVE 50.00 TO TRCM-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-REFUND-AMT > 5000.00
               MOVE 5000.00 TO TRCM-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3350-COUNT-EXCEPT.
           IF TRCM-EXCEPT-CNT < 9999999
               ADD 1 TO TRCM-EXCEPT-CNT
           END-IF
           IF TRCM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO TRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3360-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCM-COMMISSION-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRCM-COMMISSION-AMT - (WS-WORK-AMT *
               4)
           COMPUTE TRCM-CONSIGN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FACE FEE ON THE COMMISSION YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3370-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN TRCM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO TRCM-FACE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRCM-FACE-AMT ROUNDED = TRCM-COMMISSION-YTD *
                       TRCM-FEE-PCT / 200
               WHEN TRCT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO TRCM-FACE-AMT
               WHEN TRCT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO TRCM-FACE-AMT
               WHEN OTHER
                   COMPUTE TRCM-FACE-AMT ROUNDED = TRCM-COMMISSION-YTD *
                       TRCM-FEE-PCT / 100
           END-EVALUATE
           IF TRCM-FACE-AMT > 0 AND TRCM-FACE-AMT < 5.00
               MOVE 5.00 TO TRCM-FACE-AMT
           END-IF
           IF TRCM-FACE-AMT > 250.00
               MOVE 250.00 TO TRCM-FACE-AMT
           END-IF
           ADD TRCM-FACE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * REFUND YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3380-LIMIT-REFUND.
           IF TRCM-REFUND-YTD < 25.00
               MOVE 25.00 TO TRCM-REFUND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-REFUND-YTD > 1000.00
               MOVE 1000.00 TO TRCM-REFUND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3390-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-GRID-RATE(1) TO TRCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-CONSIGN-GRID-LOW(WS-TX)
                   MOVE WS-CONSIGN-GRID-RATE(WS-TX) TO TRCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3400-LIMIT-CONSIGN.
           IF TRCT-CONSIGN-AMT < 25.00
               MOVE 25.00 TO TRCT-CONSIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCT-CONSIGN-AMT > 2500.00
               MOVE 2500.00 TO TRCT-CONSIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E440 MEMO REQUIRED
      * E864 COMMISSION AMT NEGATIVE
      * E969 COMMISSION AMT OVER 99999
      * E526 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-MEMO = SPACES
                   MOVE 'E440' TO WS-REJECT-CODE
               WHEN TRCT-COMMISSION-AMT < 0
                   MOVE 'E864' TO WS-REJECT-CODE
               WHEN TRCT-COMMISSION-AMT > 99999.00
                   MOVE 'E969' TO WS-REJECT-CODE
               WHEN TRCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E526' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-CYCLE.
           IF TRCM-CYCLE-CNT < 9999999
               ADD 1 TO TRCM-CYCLE-CNT
           END-IF
           IF TRCM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO TRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COMMISSION AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONSIGN AMT.
      *----------------------------------------------------------------*
       3430-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-CONSIGN-AMT ROUNDED =
                   TRCM-COMMISSION-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-CONSIGN-AMT
           END-IF
           ADD TRCM-CONSIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E606 OPERATOR REQUIRED
      * E380 FACE AMT NEGATIVE
      * E426 FACE AMT OVER 10000
      * E814 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3440-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-OPERATOR = SPACES
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN TRCM-FACE-AMT < 0
                   MOVE 'E380' TO WS-REJECT-CODE
               WHEN TRCM-FACE-AMT > 10000.00
                   MOVE 'E426' TO WS-REJECT-CODE
               WHEN TRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E814' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3450-DESCRIBE-CHANNEL.
           EVALUATE TRCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TRCM-NEXT-REVIEW-DATE > 0 AND TRCM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-REMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRCM-REMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRCM-REMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCM-REMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-REMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3470-ACCRUE-FACE.
           IF TRCM-FACE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-FACE-YTD * TRCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TRCT-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3480-DESCRIBE-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE COMMISSION AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHORTAGE AMT.
      *----------------------------------------------------------------*
       3490-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-SHORTAGE-AMT ROUNDED =
                   TRCT-COMMISSION-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCM-SHORTAGE-AMT
           END-IF
           ADD TRCM-SHORTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3500-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE YTD FEE ON THE SHORTAGE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3510-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TRCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TRCM-SHORTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-EXEMPT-FLAG = 'Y'
                   COMPUTE TRCM-SHORTAGE-YTD ROUNDED = TRCM-SHORTAGE-AMT
                       * TRCM-FEE-PCT / 200
               WHEN TRCM-SOURCE-CD = 'EG'
                   MOVE 3.00 TO TRCM-SHORTAGE-YTD
               WHEN TRCM-SOURCE-CD = 'GH'
                   MOVE 7.00 TO TRCM-SHORTAGE-YTD
               WHEN OTHER
                   COMPUTE TRCM-SHORTAGE-YTD ROUNDED = TRCM-SHORTAGE-AMT
                       * TRCM-FEE-PCT / 100
           END-EVALUATE
           IF TRCM-SHORTAGE-YTD > 0 AND TRCM-SHORTAGE-YTD < 2.00
               MOVE 2.00 TO TRCM-SHORTAGE-YTD
           END-IF
           IF TRCM-SHORTAGE-YTD > 50.00
               MOVE 50.00 TO TRCM-SHORTAGE-YTD
           END-IF
           ADD TRCM-SHORTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SL -> RF WHEN LAST STMT DATE IS PAST
      * RF -> RM WHEN LAST STMT DATE IS PAST
      * RM -> CN WHEN LAST STMT DATE IS PAST
      * CN -> VD WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3520-SET-TRAN.
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
      * FACE YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3530-LIMIT-FACE.
           IF TRCM-FACE-YTD < 100.00
               MOVE 100.00 TO TRCM-FACE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-FACE-YTD > 50000.00
               MOVE 50000.00 TO TRCM-FACE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONSIGN AMT.
      *----------------------------------------------------------------*
       3540-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCT-CONSIGN-AMT ROUNDED =
                   TRCM-SHORTAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCT-CONSIGN-AMT
           END-IF
           ADD TRCT-CONSIGN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT REFUND AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3550-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = TRCT-REFUND-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRCT-REFUND-AMT - (WS-WORK-AMT * 4)
           COMPUTE TRCM-CONSIGN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3560-FLAG-SHORTAGE.
           IF TRCM-SHORTAGE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-SHORTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REFUND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COMMISSION AMT.
      *----------------------------------------------------------------*
       3570-PRORATE-COMMISSION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCT-COMMISSION-AMT ROUNDED =
                   TRCT-REFUND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCT-COMMISSION-AMT
           END-IF
           ADD TRCT-COMMISSION-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3580-DESCRIBE-CYCLE.
           EVALUATE TRCM-CYCLE-CD
               WHEN 'EPW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HHK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JCB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UCB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XTY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XUV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XXU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN BAND TIER FOR THE COMMISSION AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-BAND-RATE(1) TO TRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TRCM-COMMISSION-AMT NOT < WS-CONSIGN-BAND-LOW(WS-TX)
                   MOVE WS-CONSIGN-BAND-RATE(WS-TX) TO TRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E695 CUST ID REQUIRED
      * E129 FACE YTD NEGATIVE
      * E868 FACE YTD OVER 1000000
      * E363 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3600-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-CUST-ID = SPACES
                   MOVE 'E695' TO WS-REJECT-CODE
               WHEN TRCM-FACE-YTD < 0
                   MOVE 'E129' TO WS-REJECT-CODE
               WHEN TRCM-FACE-YTD > 1000000.00
                   MOVE 'E868' TO WS-REJECT-CODE
               WHEN TRCM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E363' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN EXPIRY DATE IS PAST
      * HHK -> JCB WHEN EXPIRY DATE IS PAST
      * JCB -> UCB WHEN EXPIRY DATE IS PAST
      * UCB -> XTY WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-CYCLE.
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
      * SELECT THE CONSIGN GRIDX TIER FOR THE CONSIGN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-GRIDX-RATE(1) TO TRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRCM-CONSIGN-YTD NOT < WS-CONSIGN-GRIDX-LOW(WS-TX)
                   MOVE WS-CONSIGN-GRIDX-RATE(WS-TX) TO TRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONSIGN FEE ON THE SHORTAGE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3630-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRCM-CONSIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCT-REVERSAL-FLAG = 'Y'
                   COMPUTE TRCM-CONSIGN-AMT ROUNDED = TRCM-SHORTAGE-AMT
                       * TRCM-RATE / 200
               WHEN TRCT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO TRCM-CONSIGN-AMT
               WHEN TRCT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO TRCM-CONSIGN-AMT
               WHEN OTHER
                   COMPUTE TRCM-CONSIGN-AMT ROUNDED = TRCM-SHORTAGE-AMT
                       * TRCM-RATE / 100
           END-EVALUATE
           IF TRCM-CONSIGN-AMT > 0 AND TRCM-CONSIGN-AMT < 1.00
               MOVE 1.00 TO TRCM-CONSIGN-AMT
           END-IF
           IF TRCM-CONSIGN-AMT > 150.00
               MOVE 150.00 TO TRCM-CONSIGN-AMT
           END-IF
           ADD TRCM-CONSIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FACE YTD.
      *----------------------------------------------------------------*
       3640-PRORATE-FACE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-FACE-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCM-FACE-YTD
           END-IF
           ADD TRCM-FACE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E605 DESC REQUIRED
      * E322 REFUND AMT NEGATIVE
      * E275 REFUND AMT OVER 5000
      * E134 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-DESC = SPACES
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN TRCT-REFUND-AMT < 0
                   MOVE 'E322' TO WS-REJECT-CODE
               WHEN TRCT-REFUND-AMT > 5000.00
                   MOVE 'E275' TO WS-REJECT-CODE
               WHEN TRCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E134' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-LAST-STMT.
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
                   ADD TRCM-SHORTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRCM-SHORTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRCM-SHORTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD TRCM-SHORTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-SHORTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REFUND AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3670-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = TRCM-REFUND-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRCM-REFUND-AMT - (WS-WORK-AMT * 12)
           COMPUTE TRCT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN GRID TIER FOR THE CONSIGN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-GRID-RATE(1) TO TRCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TRCM-CONSIGN-YTD NOT < WS-CONSIGN-GRID-LOW(WS-TX)
                   MOVE WS-CONSIGN-GRID-RATE(WS-TX) TO TRCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COMMISSION AMT BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3690-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-DENOM-CD
                   ADD TRCM-COMMISSION-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-COMMISSION-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONSIGN YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3700-ACCRUE-CONSIGN.
           IF TRCM-CONSIGN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-CONSIGN-YTD * TRCM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRCM-REFUND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3710-FLAG-FACE.
           IF TRCM-FACE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCM-FACE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3720-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-NEXT-DUE-DATE > 0 AND TRCM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-SHORTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD TRCM-SHORTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRCM-SHORTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRCM-SHORTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-SHORTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE COMMISSION YTD FEE ON THE REFUND AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3730-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRCM-COMMISSION-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRCM-COMMISSION-YTD ROUNDED = TRCT-REFUND-AMT
                       * TRCM-FEE-PCT / 200
               WHEN TRCM-STATUS-CD = 'HK'
                   MOVE 5.00 TO TRCM-COMMISSION-YTD
               WHEN TRCM-STATUS-CD = 'RU'
                   MOVE 5.00 TO TRCM-COMMISSION-YTD
               WHEN OTHER
                   COMPUTE TRCM-COMMISSION-YTD ROUNDED = TRCT-REFUND-AMT
                       * TRCM-FEE-PCT / 100
           END-EVALUATE
           IF TRCM-COMMISSION-YTD > 0 AND TRCM-COMMISSION-YTD < 1.00
               MOVE 1.00 TO TRCM-COMMISSION-YTD
           END-IF
           IF TRCM-COMMISSION-YTD > 250.00
               MOVE 250.00 TO TRCM-COMMISSION-YTD
           END-IF
           ADD TRCM-COMMISSION-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN GRID TIER FOR THE CONSIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3740-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-GRID-RATE(1) TO TRCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TRCM-CONSIGN-AMT NOT < WS-CONSIGN-GRID-LOW(WS-TX)
                   MOVE WS-CONSIGN-GRID-RATE(WS-TX) TO TRCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF TRCM-MATURITY-DATE > 0 AND TRCM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REMIT YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3760-LIMIT-REMIT.
           IF TRCM-REMIT-YTD < 50.00
               MOVE 50.00 TO TRCM-REMIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-REMIT-YTD > 500.00
               MOVE 500.00 TO TRCM-REMIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3770-COUNT-ITEM.
           IF TRCM-ITEM-CNT < 9999999
               ADD 1 TO TRCM-ITEM-CNT
           END-IF
           IF TRCM-ITEM-CNT NOT < 10
               MOVE 'Y' TO TRCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHORTAGE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3780-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = TRCM-SHORTAGE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TRCM-SHORTAGE-YTD - (WS-WORK-AMT * 4)
           COMPUTE TRCM-REMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TRCM-REFUND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRCM-REFUND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRCM-REFUND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCM-REFUND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-REFUND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN BAND TIER FOR THE FACE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-BAND-RATE(1) TO TRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF TRCM-FACE-YTD NOT < WS-CONSIGN-BAND-LOW(WS-TX)
                   MOVE WS-CONSIGN-BAND-RATE(WS-TX) TO TRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST PAY DATE IS PAST
      * BAT -> MAL WHEN LAST PAY DATE IS PAST
      * MAL -> TEL WHEN LAST PAY DATE IS PAST
      * TEL -> ONL WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-CHANNEL.
           EVALUATE TRCT-CHANNEL-CD
               WHEN 'BRN'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REMIT AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3820-LIMIT-REMIT.
           IF TRCT-REMIT-AMT < 100.00
               MOVE 100.00 TO TRCT-REMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCT-REMIT-AMT > 2500.00
               MOVE 2500.00 TO TRCT-REMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CONSIGN GRID TIER FOR THE COMMISSION AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3830-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CONSIGN-GRID-RATE(1) TO TRCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TRCM-COMMISSION-AMT NOT < WS-CONSIGN-GRID-LOW(WS-TX)
                   MOVE WS-CONSIGN-GRID-RATE(WS-TX) TO TRCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3840-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCT-CHANNEL-CD
                   ADD TRCM-FACE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-FACE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-POST.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO TRCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3860-DESCRIBE-SOURCE.
           EVALUATE TRCM-SOURCE-CD
               WHEN 'EG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3870-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF TRCM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REMIT YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3880-LIMIT-REMIT.
           IF TRCM-REMIT-YTD < 100.00
               MOVE 100.00 TO TRCM-REMIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-REMIT-YTD > 500.00
               MOVE 500.00 TO TRCM-REMIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3890-COUNT-ITEM.
           IF TRCM-ITEM-CNT < 9999999
               ADD 1 TO TRCM-ITEM-CNT
           END-IF
           IF TRCM-ITEM-CNT NOT < 6
               MOVE 'Y' TO TRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3900-DESCRIBE-TRAN.
           EVALUATE TRCT-TRAN-CD
               WHEN 'SL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3910-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCM-COMMISSION-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TRCM-COMMISSION-YTD - (WS-WORK-AMT *
               2)
           COMPUTE TRCM-REMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3920-COUNT-RETRY.
           IF TRCM-RETRY-CNT < 9999999
               ADD 1 TO TRCM-RETRY-CNT
           END-IF
           IF TRCM-RETRY-CNT NOT < 6
               MOVE 'Y' TO TRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3930-COUNT-EXCEPT.
           IF TRCM-EXCEPT-CNT < 9999999
               ADD 1 TO TRCM-EXCEPT-CNT
           END-IF
           IF TRCM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO TRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COMMISSION AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COMMISSION AMT.
      *----------------------------------------------------------------*
       3940-PRORATE-COMMISSION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCT-COMMISSION-AMT ROUNDED =
                   TRCM-COMMISSION-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCT-COMMISSION-AMT
           END-IF
           ADD TRCT-COMMISSION-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CONSIGN YTD BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3950-ACCUM-DENOM.
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
      * STATUS TRANSITIONS FOR ISSUER CD:
      * AF -> CJ WHEN LAST PAY DATE IS PAST
      * CJ -> DU WHEN LAST PAY DATE IS PAST
      * DU -> JY WHEN LAST PAY DATE IS PAST
      * JY -> RR WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJ' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJ'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DU'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JY' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RR' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3970-ACCRUE-SHORTAGE.
           IF TRCM-SHORTAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-SHORTAGE-YTD * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRCT-REMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3980-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO TRCM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-STATUS-CD
                   ADD TRCM-REMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-REMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4010-DESCRIBE-TRAN.
           EVALUATE TRCT-TRAN-CD
               WHEN 'SL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

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
                   MOVE TCDTE-REFUND-YTD
                     TO TRCM-REFUND-YTD
                   MOVE TCDTE-SHORTAGE-YTD
                     TO TRCM-SHORTAGE-YTD
                   MOVE TCDTE-STOP-FLAG
                     TO TRCM-STOP-FLAG
                   MOVE TCDTE-OVERRIDE-FLAG
                     TO TRCM-OVERRIDE-FLAG
                   MOVE TCDTE-REVIEW-FLAG
                     TO TRCM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-TCDTE' TO ABND-PARAGRAPH
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
           DISPLAY 'TCCYREN READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TCCYREN' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
