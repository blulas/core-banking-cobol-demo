      *================================================================*
      * PROGRAM  : TCCCLSE                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TRAVELERS CHEQUES - CLOSURE PROCESSING. READS THE   *
      *            TRC TRANSACTIONS, UPDATES THE TRC MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. TRAVELERS CHEQUE SALES,*
      *            CONSIGNMENT INVENTORY AND REMITTANCE                *
      * INPUT    : TRCMST (KSDS), TRCTRN, BANKCTL                      *
      * OUTPUT   : TRCOUT, TCCCLRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TCCAL, TCPRC            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-11-22 JLM  ORIGINAL                                       *
      * 1987-04-20 TWH  COMMISSION FEE SCHEDULE UPDATE REQ6933         *
      * 1998-08-13 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-09-09 PQR  REFUND FEE SCHEDULE UPDATE REQ1555             *
      * 2004-08-08 SKN  COMMISSION CUTOFF MOVED INC4680                *
      * 2005-09-06 MKL  PERFORMANCE - CONSIGN READ AHEAD REQ9262       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCCCLSE.
       AUTHOR. C R PATEL.

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
           SELECT REPORT-FILE ASSIGN TO "TCCCLRPT"
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
      * WS-FACE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FACE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000173000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000228000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000074000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000709000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006350000000414000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016350000000677500'.
       01  WS-FACE-SCHED-TBL REDEFINES WS-FACE-SCHED-VALUES.
           05  WS-FACE-SCHED-ROW OCCURS 6 TIMES.
               10  WS-FACE-SCHED-LOW       PIC 9(09)V99.
               10  WS-FACE-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FACE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FACE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000701000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000196000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000109000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000450500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000816500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004600000000611500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007100000000757000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009600000000445000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019600000000398500'.
       01  WS-FACE-GRID-TBL REDEFINES WS-FACE-GRID-VALUES.
           05  WS-FACE-GRID-ROW OCCURS 9 TIMES.
               10  WS-FACE-GRID-LOW        PIC 9(09)V99.
               10  WS-FACE-GRID-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SHORTAGE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHORTAGE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000872000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000764500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000874000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000527000'.
       01  WS-SHORTAGE-TIER-TBL REDEFINES WS-SHORTAGE-TIER-VALUES.
           05  WS-SHORTAGE-TIER-ROW OCCURS 4 TIMES.
               10  WS-SHORTAGE-TIER-LOW    PIC 9(09)V99.
               10  WS-SHORTAGE-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-REMIT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REMIT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000425500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000164000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000119500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010300000000431500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010400000000495500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000456000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000685500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015750000000817000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000750500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000492000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022000000000688500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022500000000083000'.
       01  WS-REMIT-GRID-TBL REDEFINES WS-REMIT-GRID-VALUES.
           05  WS-REMIT-GRID-ROW OCCURS 12 TIMES.
               10  WS-REMIT-GRID-LOW       PIC 9(09)V99.
               10  WS-REMIT-GRID-RATE      PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TCCCLSE  TRAVELERS CHEQUES'.
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
       COPY TCCALP.
       COPY TCPRCP.

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
               PERFORM 3000-SET-ISSUER
               PERFORM 3010-SPLIT-REFUND
               PERFORM 3020-ASSESS-SUSPEND
               PERFORM 3030-LIMIT-COMMISSION
               PERFORM 3040-FLAG-COMMISSION
               PERFORM 3050-SET-SOURCE
               PERFORM 3060-SPLIT-REMIT
               PERFORM 3070-LIMIT-SHORTAGE
               PERFORM 3080-SPLIT-CONSIGN
               PERFORM 3090-PRORATE-REMIT
               PERFORM 3100-COUNT-ITEM
               PERFORM 3110-EDIT-CONTACT
               PERFORM 3120-ACCRUE-CONSIGN
               PERFORM 3130-FLAG-REMIT
               PERFORM 3140-ACCUM-ISSUER
               PERFORM 3150-SELECT-MARGIN
               PERFORM 3160-FLAG-FACE
               PERFORM 3170-CHECK-MATURITY
               PERFORM 3180-EDIT-OPERATOR
               PERFORM 3190-ASSESS-REVERSAL
               PERFORM 3200-ACCRUE-REFUND
               PERFORM 3210-SELECT-FEE-PCT
               PERFORM 3220-SET-DENOM
               PERFORM 3230-EDIT-CONTACT
               PERFORM 3240-PRORATE-COMMISSION
               PERFORM 3250-COUNT-SEQ
               PERFORM 3260-CHECK-POST
               PERFORM 3270-PRORATE-REFUND
               PERFORM 3280-SPLIT-ITEM
               PERFORM 3290-FLAG-REFUND
               PERFORM 3300-ASSESS-STOP
               PERFORM 3310-DESCRIBE-ISSUER
               PERFORM 3320-SET-TRAN
               PERFORM 3330-FLAG-SHORTAGE
               PERFORM 3340-AGE-EFF
               PERFORM 3350-EDIT-REJECT
               PERFORM 3360-ACCUM-DENOM
               PERFORM 3370-ACCRUE-COMMISSION
               PERFORM 3380-ACCUM-BRANCH
               PERFORM 3390-ACCUM-CYCLE
               PERFORM 3400-SPLIT-CONSIGN
               PERFORM 3410-ASSESS-EXEMPT
               PERFORM 3420-COUNT-CYCLE
               PERFORM 3430-SPLIT-COMMISSION
               PERFORM 3440-AGE-CLOSE
               PERFORM 3450-ACCRUE-REMIT
               PERFORM 3460-CHECK-NEXT-REVIEW
               PERFORM 3470-DESCRIBE-BRANCH
               PERFORM 3480-CHECK-EFF
               PERFORM 3490-AGE-EFFECTIVE
               PERFORM 3500-ASSESS-REVERSAL
               PERFORM 3510-SPLIT-COMMISSION
               PERFORM 3520-LIMIT-CONSIGN
               PERFORM 3530-ACCUM-DENOM
               PERFORM 3540-FLAG-CONSIGN
               PERFORM 3550-ACCUM-TRAN
               PERFORM 3560-PRORATE-REMIT
               PERFORM 3570-ACCUM-ISSUER
               PERFORM 3580-LIMIT-FACE
               PERFORM 3590-SELECT-FEE-PCT
               PERFORM 3600-SPLIT-COMMISSION
               PERFORM 3610-ACCRUE-FACE
               PERFORM 3620-DESCRIBE-CYCLE
               PERFORM 3630-ASSESS-OVERRIDE
               PERFORM 3640-DESCRIBE-CHANNEL
               PERFORM 3650-FLAG-COMMISSION
               PERFORM 3660-EDIT-CONTACT
               PERFORM 3670-SPLIT-COMMISSION
               PERFORM 3680-ACCUM-CYCLE
               PERFORM 3690-ACCRUE-REFUND
               PERFORM 3700-FLAG-FACE
               PERFORM 3710-PRORATE-REMIT
               PERFORM 3720-SELECT-RATE
               PERFORM 3730-SELECT-RATE
               PERFORM 3740-COUNT-RETRY
               PERFORM 3750-LIMIT-FACE
               PERFORM 3760-LIMIT-ITEM
               PERFORM 3770-ASSESS-EXEMPT
               PERFORM 3780-LIMIT-SHORTAGE
               PERFORM 3790-ACCRUE-REFUND
               PERFORM 3800-EDIT-CONTACT
               PERFORM 3810-CHECK-EFFECTIVE
               PERFORM 3820-EDIT-REF-NO
               PERFORM 3830-AGE-POST
               PERFORM 3840-ACCUM-CYCLE
               PERFORM 3850-AGE-LAST-STMT
               PERFORM 3860-ACCUM-DENOM
               PERFORM 3870-SPLIT-FACE
               PERFORM 3880-EDIT-REJECT
               PERFORM 3890-PRORATE-SHORTAGE
               PERFORM 3900-PRORATE-CONSIGN
               PERFORM 3910-ACCUM-TRAN
               PERFORM 3920-ASSESS-REVERSAL
               PERFORM 3930-ACCRUE-REMIT
               PERFORM 3940-SELECT-RATE
               PERFORM 3950-AGE-POST
               PERFORM 3960-SET-CYCLE
               PERFORM 3970-SET-DENOM
               PERFORM 3980-ACCUM-STATUS
               PERFORM 3990-SET-STATUS
               PERFORM 4000-COUNT-RETRY
               PERFORM 4010-DESCRIBE-STATUS
               PERFORM 4020-ACCRUE-SHORTAGE
               PERFORM 4030-SPLIT-COMMISSION
               PERFORM 4040-EDIT-OPERATOR
               PERFORM 4050-ASSESS-REVERSAL
               PERFORM 4060-FLAG-ITEM
               PERFORM 4070-COUNT-RETRY
               PERFORM 4080-AGE-EXPIRY
               PERFORM 4090-ASSESS-SUSPEND
               PERFORM 4100-LIMIT-REMIT
               PERFORM 4110-SET-TRAN
               PERFORM 4120-CHECK-EXPIRY
               PERFORM 4130-DESCRIBE-ISSUER
               PERFORM 4140-FLAG-CONSIGN
               PERFORM 4150-LIMIT-SHORTAGE
               PERFORM 4160-SET-DENOM
               PERFORM 4170-FLAG-REFUND
               PERFORM 4180-ACCRUE-REFUND
               PERFORM 4190-ACCUM-CYCLE
               PERFORM 4200-AGE-LAST-ACT
               PERFORM 4210-COUNT-CYCLE
               PERFORM 4220-COUNT-SEQ
               PERFORM 4230-DESCRIBE-TRAN
               PERFORM 4240-EDIT-DESC
               PERFORM 4250-COUNT-RETRY
               PERFORM 4260-EDIT-MEMO
               PERFORM 4270-ACCUM-ISSUER
               PERFORM 4280-ACCRUE-REMIT
               PERFORM 4290-DESCRIBE-STATUS
               PERFORM 4300-ASSESS-REVERSAL
               PERFORM 4310-LIMIT-REFUND
               PERFORM 7000-CALL-TCCAL
               PERFORM 7020-CALL-TCPRC
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
      * STATUS TRANSITIONS FOR ISSUER CD:
      * AF -> CJ WHEN LAST ACT DATE IS PAST
      * CJ -> DU WHEN LAST ACT DATE IS PAST
      * DU -> JY WHEN LAST ACT DATE IS PAST
      * JY -> RR WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3000-SET-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CJ' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJ'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DU' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DU'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JY' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JY'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RR' TO TRCM-ISSUER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REFUND AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3010-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = TRCT-REFUND-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TRCT-REFUND-AMT - (WS-WORK-AMT * 3)
           COMPUTE TRCM-REFUND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3020-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRCM-SHORTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE TRCM-SHORTAGE-YTD ROUNDED = WS-WORK-AMT *
                       TRCM-MARGIN / 200
               WHEN TRCM-SOURCE-CD = 'EG'
                   MOVE 7.00 TO TRCM-SHORTAGE-YTD
               WHEN TRCM-SOURCE-CD = 'GH'
                   MOVE 5.00 TO TRCM-SHORTAGE-YTD
               WHEN OTHER
                   COMPUTE TRCM-SHORTAGE-YTD ROUNDED = WS-WORK-AMT *
                       TRCM-MARGIN / 100
           END-EVALUATE
           IF TRCM-SHORTAGE-YTD > 0 AND TRCM-SHORTAGE-YTD < 10.00
               MOVE 10.00 TO TRCM-SHORTAGE-YTD
           END-IF
           IF TRCM-SHORTAGE-YTD > 250.00
               MOVE 250.00 TO TRCM-SHORTAGE-YTD
           END-IF
           ADD TRCM-SHORTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * COMMISSION YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3030-LIMIT-COMMISSION.
           IF TRCM-COMMISSION-YTD < 25.00
               MOVE 25.00 TO TRCM-COMMISSION-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-COMMISSION-YTD > 2500.00
               MOVE 2500.00 TO TRCM-COMMISSION-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COMMISSION AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3040-FLAG-COMMISSION.
           IF TRCT-COMMISSION-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRCT-COMMISSION-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * EG -> GH WHEN NEXT DUE DATE IS PAST
      * GH -> LC WHEN NEXT DUE DATE IS PAST
      * LC -> LU WHEN NEXT DUE DATE IS PAST
      * LU -> YS WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-SOURCE.
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
      * SPLIT REMIT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3060-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = TRCT-REMIT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRCT-REMIT-AMT - (WS-WORK-AMT * 4)
           COMPUTE TRCT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3070-LIMIT-SHORTAGE.
           IF TRCM-SHORTAGE-AMT < 0.00
               MOVE 0.00 TO TRCM-SHORTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-SHORTAGE-AMT > 10000.00
               MOVE 10000.00 TO TRCM-SHORTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONSIGN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3080-SPLIT-CONSIGN.
           COMPUTE WS-WORK-AMT = TRCM-CONSIGN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRCM-CONSIGN-AMT - (WS-WORK-AMT * 4)
           COMPUTE TRCM-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FACE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REMIT AMT.
      *----------------------------------------------------------------*
       3090-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-REMIT-AMT ROUNDED =
                   TRCM-FACE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-REMIT-AMT
           END-IF
           ADD TRCM-REMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-ITEM.
           IF TRCM-ITEM-CNT < 9999999
               ADD 1 TO TRCM-ITEM-CNT
           END-IF
           IF TRCM-ITEM-CNT NOT < 6
               MOVE 'Y' TO TRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E366 CONTACT REQUIRED
      * E577 REFUND YTD NEGATIVE
      * E211 REFUND YTD OVER 10000
      * E911 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3110-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-CONTACT = SPACES
                   MOVE 'E366' TO WS-REJECT-CODE
               WHEN TRCM-REFUND-YTD < 0
                   MOVE 'E577' TO WS-REJECT-CODE
               WHEN TRCM-REFUND-YTD > 10000.00
                   MOVE 'E211' TO WS-REJECT-CODE
               WHEN TRCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E911' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONSIGN AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3120-ACCRUE-CONSIGN.
           IF TRCT-CONSIGN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCT-CONSIGN-AMT * TRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRCM-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REMIT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3130-FLAG-REMIT.
           IF TRCT-REMIT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCT-REMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FACE YTD BY ISSUER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3140-ACCUM-ISSUER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-ISSUER-CD
                   ADD TRCM-FACE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-ISSUER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-FACE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE TIER TIER FOR THE CONSIGN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-TIER-RATE(1) TO TRCM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF TRCT-CONSIGN-AMT NOT < WS-SHORTAGE-TIER-LOW(WS-TX)
                   MOVE WS-SHORTAGE-TIER-RATE(WS-TX) TO TRCM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3160-FLAG-FACE.
           IF TRCM-FACE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3170-CHECK-MATURITY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO TRCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E971 OPERATOR REQUIRED
      * E782 REFUND AMT NEGATIVE
      * E673 REFUND AMT OVER 25000
      * E583 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3180-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-OPERATOR = SPACES
                   MOVE 'E971' TO WS-REJECT-CODE
               WHEN TRCM-REFUND-AMT < 0
                   MOVE 'E782' TO WS-REJECT-CODE
               WHEN TRCM-REFUND-AMT > 25000.00
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN TRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E583' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REMIT YTD FEE ON THE WORK AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3190-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRCM-REMIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRCM-REMIT-YTD ROUNDED = WS-WORK-AMT *
                       TRCM-RATE / 200
               WHEN TRCM-DENOM-CD = 'G'
                   MOVE 12.00 TO TRCM-REMIT-YTD
               WHEN TRCM-DENOM-CD = 'M'
                   MOVE 3.00 TO TRCM-REMIT-YTD
               WHEN OTHER
                   COMPUTE TRCM-REMIT-YTD ROUNDED = WS-WORK-AMT *
                       TRCM-RATE / 100
           END-EVALUATE
           IF TRCM-REMIT-YTD > 0 AND TRCM-REMIT-YTD < 5.00
               MOVE 5.00 TO TRCM-REMIT-YTD
           END-IF
           IF TRCM-REMIT-YTD > 100.00
               MOVE 100.00 TO TRCM-REMIT-YTD
           END-IF
           ADD TRCM-REMIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-REFUND.
           IF TRCM-REFUND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-REFUND-YTD * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRCM-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FACE GRID TIER FOR THE REMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-GRID-RATE(1) TO TRCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRCT-REMIT-AMT NOT < WS-FACE-GRID-LOW(WS-TX)
                   MOVE WS-FACE-GRID-RATE(WS-TX) TO TRCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DENOM CD:
      * G -> M WHEN LAST ACT DATE IS PAST
      * M -> N WHEN LAST ACT DATE IS PAST
      * N -> P WHEN LAST ACT DATE IS PAST
      * P -> V WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3220-SET-DENOM.
           EVALUATE TRCM-DENOM-CD
               WHEN 'G'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TRCM-LAST-ACT-DATE > 0 AND TRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E997 CONTACT REQUIRED
      * E541 REFUND AMT NEGATIVE
      * E969 REFUND AMT OVER 50000
      * E771 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3230-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-CONTACT = SPACES
                   MOVE 'E997' TO WS-REJECT-CODE
               WHEN TRCM-REFUND-AMT < 0
                   MOVE 'E541' TO WS-REJECT-CODE
               WHEN TRCM-REFUND-AMT > 50000.00
                   MOVE 'E969' TO WS-REJECT-CODE
               WHEN TRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E771' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO COMMISSION AMT.
      *----------------------------------------------------------------*
       3240-PRORATE-COMMISSION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-COMMISSION-AMT ROUNDED =
                   TRCM-REMIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCM-COMMISSION-AMT
           END-IF
           ADD TRCM-COMMISSION-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3250-COUNT-SEQ.
           IF TRCT-SEQ < 9999999
               ADD 1 TO TRCT-SEQ
           END-IF
           IF TRCT-SEQ NOT < 12
               MOVE 'Y' TO TRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3260-CHECK-POST.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO TRCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       3270-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCT-REFUND-AMT ROUNDED =
                   TRCM-REMIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCT-REFUND-AMT
           END-IF
           ADD TRCT-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3280-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3290-FLAG-REFUND.
           IF TRCM-REFUND-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCM-REFUND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REMIT FEE ON THE CONSIGN AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3300-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TRCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TRCM-REMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCT-REVERSAL-FLAG = 'Y'
                   COMPUTE TRCM-REMIT-AMT ROUNDED = TRCM-CONSIGN-AMT *
                       TRCM-RATE / 200
               WHEN TRCM-CYCLE-CD = 'EPW'
                   MOVE 10.00 TO TRCM-REMIT-AMT
               WHEN TRCM-CYCLE-CD = 'HHK'
                   MOVE 7.00 TO TRCM-REMIT-AMT
               WHEN OTHER
                   COMPUTE TRCM-REMIT-AMT ROUNDED = TRCM-CONSIGN-AMT *
                       TRCM-RATE / 100
           END-EVALUATE
           IF TRCM-REMIT-AMT > 0 AND TRCM-REMIT-AMT < 10.00
               MOVE 10.00 TO TRCM-REMIT-AMT
           END-IF
           IF TRCM-REMIT-AMT > 75.00
               MOVE 75.00 TO TRCM-REMIT-AMT
           END-IF
           ADD TRCM-REMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SL -> RF WHEN MATURITY DATE IS PAST
      * RF -> RM WHEN MATURITY DATE IS PAST
      * RM -> CN WHEN MATURITY DATE IS PAST
      * CN -> VD WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-TRAN.
           EVALUATE TRCT-TRAN-CD
               WHEN 'SL'
                   IF TRCM-MATURITY-DATE > 0 AND TRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RF'
                   IF TRCM-MATURITY-DATE > 0 AND TRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RM'
                   IF TRCM-MATURITY-DATE > 0 AND TRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF TRCM-MATURITY-DATE > 0 AND TRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHORTAGE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3330-FLAG-SHORTAGE.
           IF TRCM-SHORTAGE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRCM-SHORTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3340-AGE-EFF.
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
                   ADD TRCM-SHORTAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TRCM-SHORTAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TRCM-SHORTAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCM-SHORTAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-SHORTAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E773 REJECT CD REQUIRED
      * E146 REMIT AMT NEGATIVE
      * E999 REMIT AMT OVER 25000
      * E676 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-REJECT-CD = SPACES
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN TRCM-REMIT-AMT < 0
                   MOVE 'E146' TO WS-REJECT-CODE
               WHEN TRCM-REMIT-AMT > 25000.00
                   MOVE 'E999' TO WS-REJECT-CODE
               WHEN TRCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E676' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COMMISSION AMT BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3360-ACCUM-DENOM.
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
      * DAILY INTEREST ON COMMISSION AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-COMMISSION.
           IF TRCT-COMMISSION-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCT-COMMISSION-AMT * TRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRCM-REMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3380-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-BRANCH-CD
                   ADD TRCT-REMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCT-REMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHORTAGE YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3390-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-CYCLE-CD
                   ADD TRCM-SHORTAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-SHORTAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONSIGN AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-CONSIGN.
           COMPUTE WS-WORK-AMT = TRCM-CONSIGN-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRCM-CONSIGN-AMT - (WS-WORK-AMT * 12)
           COMPUTE TRCM-COMMISSION-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONSIGN YTD FEE ON THE REFUND YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3410-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRCM-CONSIGN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TRCM-CONSIGN-YTD ROUNDED = TRCM-REFUND-YTD *
                       TRCM-MARGIN / 200
               WHEN TRCM-CYCLE-CD = 'EPW'
                   MOVE 20.00 TO TRCM-CONSIGN-YTD
               WHEN TRCM-CYCLE-CD = 'HHK'
                   MOVE 12.00 TO TRCM-CONSIGN-YTD
               WHEN OTHER
                   COMPUTE TRCM-CONSIGN-YTD ROUNDED = TRCM-REFUND-YTD *
                       TRCM-MARGIN / 100
           END-EVALUATE
           IF TRCM-CONSIGN-YTD > 0 AND TRCM-CONSIGN-YTD < 1.00
               MOVE 1.00 TO TRCM-CONSIGN-YTD
           END-IF
           IF TRCM-CONSIGN-YTD > 25.00
               MOVE 25.00 TO TRCM-CONSIGN-YTD
           END-IF
           ADD TRCM-CONSIGN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-CYCLE.
           IF TRCM-CYCLE-CNT < 9999999
               ADD 1 TO TRCM-CYCLE-CNT
           END-IF
           IF TRCM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO TRCM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCM-COMMISSION-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TRCM-COMMISSION-AMT - (WS-WORK-AMT *
               12)
           COMPUTE TRCM-REMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-CLOSE.
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
                   ADD TRCM-SHORTAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TRCM-SHORTAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TRCM-SHORTAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD TRCM-SHORTAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-SHORTAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3450-ACCRUE-REMIT.
           IF TRCM-REMIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-REMIT-YTD * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRCT-REMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3460-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO TRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3470-DESCRIBE-BRANCH.
           EVALUATE TRCM-BRANCH-CD
               WHEN 'EG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3480-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF TRCT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TRCT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TRCM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-EFFECTIVE.
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
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE COMMISSION FEE ON THE REFUND AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3500-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRCM-COMMISSION-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-REVIEW-FLAG = 'Y'
                   COMPUTE TRCM-COMMISSION-AMT ROUNDED = TRCT-REFUND-AMT
                       * TRCM-MARGIN / 200
               WHEN TRCM-BRANCH-CD = 'EG'
                   MOVE 20.00 TO TRCM-COMMISSION-AMT
               WHEN TRCM-BRANCH-CD = 'HZ'
                   MOVE 10.00 TO TRCM-COMMISSION-AMT
               WHEN OTHER
                   COMPUTE TRCM-COMMISSION-AMT ROUNDED = TRCT-REFUND-AMT
                       * TRCM-MARGIN / 100
           END-EVALUATE
           IF TRCM-COMMISSION-AMT > 0 AND TRCM-COMMISSION-AMT < 5.00
               MOVE 5.00 TO TRCM-COMMISSION-AMT
           END-IF
           IF TRCM-COMMISSION-AMT > 50.00
               MOVE 50.00 TO TRCM-COMMISSION-AMT
           END-IF
           ADD TRCM-COMMISSION-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3510-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCM-COMMISSION-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TRCM-COMMISSION-AMT - (WS-WORK-AMT *
               4)
           COMPUTE TRCM-COMMISSION-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3520-LIMIT-CONSIGN.
           IF TRCT-CONSIGN-AMT < 50.00
               MOVE 50.00 TO TRCT-CONSIGN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCT-CONSIGN-AMT > 500.00
               MOVE 500.00 TO TRCT-CONSIGN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT AMT BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3530-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-DENOM-CD
                   ADD TRCT-REMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCT-REMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CONSIGN AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3540-FLAG-CONSIGN.
           IF TRCT-CONSIGN-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TRCT-CONSIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHORTAGE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3550-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCT-TRAN-CD
                   ADD TRCM-SHORTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-SHORTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COMMISSION AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REMIT YTD.
      *----------------------------------------------------------------*
       3560-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-REMIT-YTD ROUNDED =
                   TRCT-COMMISSION-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCM-REMIT-YTD
           END-IF
           ADD TRCM-REMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY ISSUER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-ISSUER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-ISSUER-CD
                   ADD TRCT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-ISSUER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FACE YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3580-LIMIT-FACE.
           IF TRCM-FACE-YTD < 100.00
               MOVE 100.00 TO TRCM-FACE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-FACE-YTD > 10000.00
               MOVE 10000.00 TO TRCM-FACE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT GRID TIER FOR THE REMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-GRID-RATE(1) TO TRCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TRCM-REMIT-YTD NOT < WS-REMIT-GRID-LOW(WS-TX)
                   MOVE WS-REMIT-GRID-RATE(WS-TX) TO TRCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3600-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCT-COMMISSION-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TRCT-COMMISSION-AMT - (WS-WORK-AMT *
               2)
           COMPUTE TRCT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FACE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3610-ACCRUE-FACE.
           IF TRCM-FACE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-FACE-YTD * TRCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3620-DESCRIBE-CYCLE.
           EVALUATE TRCM-CYCLE-CD
               WHEN 'EPW'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HHK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JCB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UCB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XTY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XUV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XXU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FACE FEE ON THE FACE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3630-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TRCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TRCM-FACE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-REVIEW-FLAG = 'Y'
                   COMPUTE TRCM-FACE-AMT ROUNDED = TRCM-FACE-YTD *
                       TRCM-MARGIN / 200
               WHEN TRCM-BRANCH-CD = 'EG'
                   MOVE 15.00 TO TRCM-FACE-AMT
               WHEN TRCM-BRANCH-CD = 'HZ'
                   MOVE 20.00 TO TRCM-FACE-AMT
               WHEN OTHER
                   COMPUTE TRCM-FACE-AMT ROUNDED = TRCM-FACE-YTD *
                       TRCM-MARGIN / 100
           END-EVALUATE
           IF TRCM-FACE-AMT > 0 AND TRCM-FACE-AMT < 1.00
               MOVE 1.00 TO TRCM-FACE-AMT
           END-IF
           IF TRCM-FACE-AMT > 35.00
               MOVE 35.00 TO TRCM-FACE-AMT
           END-IF
           ADD TRCM-FACE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3640-DESCRIBE-CHANNEL.
           EVALUATE TRCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COMMISSION AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3650-FLAG-COMMISSION.
           IF TRCT-COMMISSION-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCT-COMMISSION-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E263 CONTACT REQUIRED
      * E423 COMMISSION YTD NEGATIVE
      * E223 COMMISSION YTD OVER 50000
      * E771 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-CONTACT = SPACES
                   MOVE 'E263' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-YTD < 0
                   MOVE 'E423' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-YTD > 50000.00
                   MOVE 'E223' TO WS-REJECT-CODE
               WHEN TRCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E771' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3670-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE TRCM-COMMISSION-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3680-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-CYCLE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3690-ACCRUE-REFUND.
           IF TRCT-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCT-REFUND-AMT * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRCM-COMMISSION-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3700-FLAG-FACE.
           IF TRCM-FACE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRCM-FACE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REMIT AMT.
      *----------------------------------------------------------------*
       3710-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-REMIT-AMT ROUNDED =
                   TRCM-REMIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCM-REMIT-AMT
           END-IF
           ADD TRCM-REMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FACE GRID TIER FOR THE CONSIGN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3720-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FACE-GRID-RATE(1) TO TRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TRCM-CONSIGN-YTD NOT < WS-FACE-GRID-LOW(WS-TX)
                   MOVE WS-FACE-GRID-RATE(WS-TX) TO TRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3730-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-GRID-RATE(1) TO TRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-REMIT-GRID-LOW(WS-TX)
                   MOVE WS-REMIT-GRID-RATE(WS-TX) TO TRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-RETRY.
           IF TRCM-RETRY-CNT < 9999999
               ADD 1 TO TRCM-RETRY-CNT
           END-IF
           IF TRCM-RETRY-CNT NOT < 10
               MOVE 'Y' TO TRCM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FACE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3750-LIMIT-FACE.
           IF TRCM-FACE-AMT < 0.00
               MOVE 0.00 TO TRCM-FACE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-FACE-AMT > 10000.00
               MOVE 10000.00 TO TRCM-FACE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3760-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CONSIGN FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3770-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TRCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TRCM-CONSIGN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-REVIEW-FLAG = 'Y'
                   COMPUTE TRCM-CONSIGN-AMT ROUNDED = TRCM-SHORTAGE-YTD
                       * TRCM-FEE-PCT / 200
               WHEN TRCT-TRAN-CD = 'SL'
                   MOVE 15.00 TO TRCM-CONSIGN-AMT
               WHEN TRCT-TRAN-CD = 'RF'
                   MOVE 3.00 TO TRCM-CONSIGN-AMT
               WHEN OTHER
                   COMPUTE TRCM-CONSIGN-AMT ROUNDED = TRCM-SHORTAGE-YTD
                       * TRCM-FEE-PCT / 100
           END-EVALUATE
           IF TRCM-CONSIGN-AMT > 0 AND TRCM-CONSIGN-AMT < 1.00
               MOVE 1.00 TO TRCM-CONSIGN-AMT
           END-IF
           IF TRCM-CONSIGN-AMT > 35.00
               MOVE 35.00 TO TRCM-CONSIGN-AMT
           END-IF
           ADD TRCM-CONSIGN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SHORTAGE YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-SHORTAGE.
           IF TRCM-SHORTAGE-YTD < 50.00
               MOVE 50.00 TO TRCM-SHORTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-SHORTAGE-YTD > 50000.00
               MOVE 50000.00 TO TRCM-SHORTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3790-ACCRUE-REFUND.
           IF TRCT-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCT-REFUND-AMT * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E461 CONTACT REQUIRED
      * E452 COMMISSION YTD NEGATIVE
      * E472 COMMISSION YTD OVER 50000
      * E176 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-CONTACT = SPACES
                   MOVE 'E461' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-YTD < 0
                   MOVE 'E452' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-YTD > 50000.00
                   MOVE 'E472' TO WS-REJECT-CODE
               WHEN TRCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3810-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO TRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E794 REF NO REQUIRED
      * E718 COMMISSION YTD NEGATIVE
      * E942 COMMISSION YTD OVER 1000000
      * E585 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-REF-NO = SPACES
                   MOVE 'E794' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-YTD < 0
                   MOVE 'E718' TO WS-REJECT-CODE
               WHEN TRCM-COMMISSION-YTD > 1000000.00
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN TRCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E585' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-POST.
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
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3840-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-CYCLE-CD
                   ADD TRCM-REMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-REMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-LAST-STMT.
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
                   ADD TRCT-COMMISSION-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD TRCT-COMMISSION-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TRCT-COMMISSION-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCT-COMMISSION-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCT-COMMISSION-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE COMMISSION YTD BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-DENOM-CD
                   ADD TRCM-COMMISSION-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-COMMISSION-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FACE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3870-SPLIT-FACE.
           COMPUTE WS-WORK-AMT = TRCM-FACE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TRCM-FACE-YTD - (WS-WORK-AMT * 2)
           COMPUTE TRCT-COMMISSION-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E281 REJECT CD REQUIRED
      * E645 REMIT AMT NEGATIVE
      * E044 REMIT AMT OVER 1000000
      * E030 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3880-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-REJECT-CD = SPACES
                   MOVE 'E281' TO WS-REJECT-CODE
               WHEN TRCT-REMIT-AMT < 0
                   MOVE 'E645' TO WS-REJECT-CODE
               WHEN TRCT-REMIT-AMT > 1000000.00
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN TRCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E030' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COMMISSION YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHORTAGE AMT.
      *----------------------------------------------------------------*
       3890-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-SHORTAGE-AMT ROUNDED =
                   TRCM-COMMISSION-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TRCM-SHORTAGE-AMT
           END-IF
           ADD TRCM-SHORTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE REMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONSIGN YTD.
      *----------------------------------------------------------------*
       3900-PRORATE-CONSIGN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TRCM-CONSIGN-YTD ROUNDED =
                   TRCM-REMIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TRCM-CONSIGN-YTD
           END-IF
           ADD TRCM-CONSIGN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3910-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCT-TRAN-CD
                   ADD TRCT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REMIT FEE ON THE CONSIGN AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3920-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRCM-REMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   COMPUTE TRCM-REMIT-AMT ROUNDED = TRCT-CONSIGN-AMT *
                       TRCM-MARGIN / 200
               WHEN TRCM-SOURCE-CD = 'EG'
                   MOVE 10.00 TO TRCM-REMIT-AMT
               WHEN TRCM-SOURCE-CD = 'GH'
                   MOVE 5.00 TO TRCM-REMIT-AMT
               WHEN OTHER
                   COMPUTE TRCM-REMIT-AMT ROUNDED = TRCT-CONSIGN-AMT *
                       TRCM-MARGIN / 100
           END-EVALUATE
           IF TRCM-REMIT-AMT > 0 AND TRCM-REMIT-AMT < 2.00
               MOVE 2.00 TO TRCM-REMIT-AMT
           END-IF
           IF TRCM-REMIT-AMT > 50.00
               MOVE 50.00 TO TRCM-REMIT-AMT
           END-IF
           ADD TRCM-REMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3930-ACCRUE-REMIT.
           IF TRCM-REMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-REMIT-AMT * TRCM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO TRCT-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REMIT GRID TIER FOR THE REMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REMIT-GRID-RATE(1) TO TRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF TRCT-REMIT-AMT NOT < WS-REMIT-GRID-LOW(WS-TX)
                   MOVE WS-REMIT-GRID-RATE(WS-TX) TO TRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3950-AGE-POST.
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
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-CONSIGN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EPW -> HHK WHEN EFF DATE IS PAST
      * HHK -> JCB WHEN EFF DATE IS PAST
      * JCB -> UCB WHEN EFF DATE IS PAST
      * UCB -> XTY WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-CYCLE.
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
      * STATUS TRANSITIONS FOR DENOM CD:
      * G -> M WHEN CLOSE DATE IS PAST
      * M -> N WHEN CLOSE DATE IS PAST
      * N -> P WHEN CLOSE DATE IS PAST
      * P -> V WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-DENOM.
           EVALUATE TRCM-DENOM-CD
               WHEN 'G'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3980-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-STATUS-CD
                   ADD TRCT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * HK -> RU WHEN NEXT REVIEW DATE IS PAST
      * RU -> WA WHEN NEXT REVIEW DATE IS PAST
      * WA -> HK WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3990-SET-STATUS.
           EVALUATE TRCM-STATUS-CD
               WHEN 'HK'
                   IF TRCM-NEXT-REVIEW-DATE > 0 AND
                       TRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RU' TO TRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RU'
                   IF TRCM-NEXT-REVIEW-DATE > 0 AND
                       TRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WA' TO TRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WA'
                   IF TRCM-NEXT-REVIEW-DATE > 0 AND
                       TRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'HK' TO TRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4000-COUNT-RETRY.
           IF TRCM-RETRY-CNT < 9999999
               ADD 1 TO TRCM-RETRY-CNT
           END-IF
           IF TRCM-RETRY-CNT NOT < 10
               MOVE 'Y' TO TRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4010-DESCRIBE-STATUS.
           EVALUATE TRCM-STATUS-CD
               WHEN 'HK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4020-ACCRUE-SHORTAGE.
           IF TRCM-SHORTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-SHORTAGE-AMT * TRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRCM-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COMMISSION AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-COMMISSION.
           COMPUTE WS-WORK-AMT = TRCM-COMMISSION-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TRCM-COMMISSION-AMT - (WS-WORK-AMT *
               3)
           COMPUTE TRCM-REMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E673 OPERATOR REQUIRED
      * E370 FACE YTD NEGATIVE
      * E973 FACE YTD OVER 25000
      * E111 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4040-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-OPERATOR = SPACES
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN TRCM-FACE-YTD < 0
                   MOVE 'E370' TO WS-REJECT-CODE
               WHEN TRCM-FACE-YTD > 25000.00
                   MOVE 'E973' TO WS-REJECT-CODE
               WHEN TRCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E111' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REMIT FEE ON THE SHORTAGE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4050-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRCM-REMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-REVIEW-FLAG = 'Y'
                   COMPUTE TRCM-REMIT-AMT ROUNDED = TRCM-SHORTAGE-AMT *
                       TRCM-MARGIN / 200
               WHEN TRCT-TRAN-CD = 'SL'
                   MOVE 5.00 TO TRCM-REMIT-AMT
               WHEN TRCT-TRAN-CD = 'RF'
                   MOVE 5.00 TO TRCM-REMIT-AMT
               WHEN OTHER
                   COMPUTE TRCM-REMIT-AMT ROUNDED = TRCM-SHORTAGE-AMT *
                       TRCM-MARGIN / 100
           END-EVALUATE
           IF TRCM-REMIT-AMT > 0 AND TRCM-REMIT-AMT < 2.00
               MOVE 2.00 TO TRCM-REMIT-AMT
           END-IF
           IF TRCM-REMIT-AMT > 75.00
               MOVE 75.00 TO TRCM-REMIT-AMT
           END-IF
           ADD TRCM-REMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4060-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4070-COUNT-RETRY.
           IF TRCM-RETRY-CNT < 9999999
               ADD 1 TO TRCM-RETRY-CNT
           END-IF
           IF TRCM-RETRY-CNT NOT < 3
               MOVE 'Y' TO TRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4080-AGE-EXPIRY.
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
                   ADD TRCM-REFUND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD TRCM-REFUND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TRCM-REFUND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TRCM-REFUND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-REFUND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE COMMISSION FEE ON THE SHORTAGE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4090-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TRCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TRCM-COMMISSION-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-REVIEW-FLAG = 'Y'
                   COMPUTE TRCM-COMMISSION-AMT ROUNDED =
                       TRCM-SHORTAGE-AMT * TRCM-FEE-PCT / 200
               WHEN TRCM-CYCLE-CD = 'EPW'
                   MOVE 12.00 TO TRCM-COMMISSION-AMT
               WHEN TRCM-CYCLE-CD = 'HHK'
                   MOVE 3.00 TO TRCM-COMMISSION-AMT
               WHEN OTHER
                   COMPUTE TRCM-COMMISSION-AMT ROUNDED =
                       TRCM-SHORTAGE-AMT * TRCM-FEE-PCT / 100
           END-EVALUATE
           IF TRCM-COMMISSION-AMT > 0 AND TRCM-COMMISSION-AMT < 1.00
               MOVE 1.00 TO TRCM-COMMISSION-AMT
           END-IF
           IF TRCM-COMMISSION-AMT > 75.00
               MOVE 75.00 TO TRCM-COMMISSION-AMT
           END-IF
           ADD TRCM-COMMISSION-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * REMIT YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4100-LIMIT-REMIT.
           IF TRCM-REMIT-YTD < 100.00
               MOVE 100.00 TO TRCM-REMIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-REMIT-YTD > 50000.00
               MOVE 50000.00 TO TRCM-REMIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SL -> RF WHEN LAST PAY DATE IS PAST
      * RF -> RM WHEN LAST PAY DATE IS PAST
      * RM -> CN WHEN LAST PAY DATE IS PAST
      * CN -> VD WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4110-SET-TRAN.
           EVALUATE TRCT-TRAN-CD
               WHEN 'SL'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RF'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RM'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF TRCM-LAST-PAY-DATE > 0 AND TRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VD' TO TRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4120-CHECK-EXPIRY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO TRCM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ISSUER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4130-DESCRIBE-ISSUER.
           EVALUATE TRCM-ISSUER-CD
               WHEN 'AF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TK'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CONSIGN AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4140-FLAG-CONSIGN.
           IF TRCT-CONSIGN-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TRCT-CONSIGN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4150-LIMIT-SHORTAGE.
           IF TRCM-SHORTAGE-YTD < 25.00
               MOVE 25.00 TO TRCM-SHORTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCM-SHORTAGE-YTD > 10000.00
               MOVE 10000.00 TO TRCM-SHORTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DENOM CD:
      * G -> M WHEN CLOSE DATE IS PAST
      * M -> N WHEN CLOSE DATE IS PAST
      * N -> P WHEN CLOSE DATE IS PAST
      * P -> V WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4160-SET-DENOM.
           EVALUATE TRCM-DENOM-CD
               WHEN 'G'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF TRCM-CLOSE-DATE > 0 AND TRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO TRCM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REFUND AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4170-FLAG-REFUND.
           IF TRCM-REFUND-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TRCM-REFUND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4180-ACCRUE-REFUND.
           IF TRCM-REFUND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-REFUND-YTD * TRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO TRCM-COMMISSION-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4190-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-CYCLE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4200-AGE-LAST-ACT.
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
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TRCM-CONSIGN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4210-COUNT-CYCLE.
           IF TRCM-CYCLE-CNT < 9999999
               ADD 1 TO TRCM-CYCLE-CNT
           END-IF
           IF TRCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO TRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4220-COUNT-SEQ.
           IF TRCT-SEQ < 9999999
               ADD 1 TO TRCT-SEQ
           END-IF
           IF TRCT-SEQ NOT < 12
               MOVE 'Y' TO TRCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4230-DESCRIBE-TRAN.
           EVALUATE TRCT-TRAN-CD
               WHEN 'SL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E969 DESC REQUIRED
      * E144 REMIT YTD NEGATIVE
      * E678 REMIT YTD OVER 10000
      * E582 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4240-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCM-DESC = SPACES
                   MOVE 'E969' TO WS-REJECT-CODE
               WHEN TRCM-REMIT-YTD < 0
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN TRCM-REMIT-YTD > 10000.00
                   MOVE 'E678' TO WS-REJECT-CODE
               WHEN TRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E582' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4250-COUNT-RETRY.
           IF TRCM-RETRY-CNT < 9999999
               ADD 1 TO TRCM-RETRY-CNT
           END-IF
           IF TRCM-RETRY-CNT NOT < 5
               MOVE 'Y' TO TRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E319 MEMO REQUIRED
      * E450 SHORTAGE AMT NEGATIVE
      * E537 SHORTAGE AMT OVER 99999
      * E671 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4260-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TRCT-MEMO = SPACES
                   MOVE 'E319' TO WS-REJECT-CODE
               WHEN TRCM-SHORTAGE-AMT < 0
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN TRCM-SHORTAGE-AMT > 99999.00
                   MOVE 'E537' TO WS-REJECT-CODE
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
      * ACCUMULATE COMMISSION AMT BY ISSUER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4270-ACCUM-ISSUER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TRCM-ISSUER-CD
                   ADD TRCM-COMMISSION-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TRCM-ISSUER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TRCM-COMMISSION-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4280-ACCRUE-REMIT.
           IF TRCM-REMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TRCM-REMIT-AMT * TRCM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO TRCM-FACE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4290-DESCRIBE-STATUS.
           EVALUATE TRCM-STATUS-CD
               WHEN 'HK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE REMIT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4300-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TRCT-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TRCM-REVIEW-FLAG = 'Y'
                   COMPUTE TRCT-REFUND-AMT ROUNDED = TRCM-REMIT-AMT *
                       TRCM-MARGIN / 200
               WHEN TRCM-ISSUER-CD = 'AF'
                   MOVE 10.00 TO TRCT-REFUND-AMT
               WHEN TRCM-ISSUER-CD = 'CJ'
                   MOVE 5.00 TO TRCT-REFUND-AMT
               WHEN OTHER
                   COMPUTE TRCT-REFUND-AMT ROUNDED = TRCM-REMIT-AMT *
                       TRCM-MARGIN / 100
           END-EVALUATE
           IF TRCT-REFUND-AMT > 0 AND TRCT-REFUND-AMT < 5.00
               MOVE 5.00 TO TRCT-REFUND-AMT
           END-IF
           IF TRCT-REFUND-AMT > 75.00
               MOVE 75.00 TO TRCT-REFUND-AMT
           END-IF
           ADD TRCT-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4310-LIMIT-REFUND.
           IF TRCT-REFUND-AMT < 0.00
               MOVE 0.00 TO TRCT-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TRCT-REFUND-AMT > 50000.00
               MOVE 50000.00 TO TRCT-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

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
                   MOVE TCCAL-COMMISSION-AMT
                     TO TRCM-COMMISSION-AMT
                   MOVE TCCAL-SHORTAGE-AMT
                     TO TRCM-SHORTAGE-AMT
                   MOVE TCCAL-CONSIGN-AMT
                     TO TRCM-CONSIGN-AMT
                   MOVE TCCAL-STOP-FLAG
                     TO TRCM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-TCCAL' TO ABND-PARAGRAPH
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
                   MOVE TCPRC-SHORTAGE-YTD
                     TO TRCM-SHORTAGE-YTD
                   MOVE TCPRC-FACE-YTD
                     TO TRCM-FACE-YTD
                   MOVE TCPRC-CONSIGN-AMT
                     TO TRCM-CONSIGN-AMT
                   MOVE TCPRC-REVIEW-FLAG
                     TO TRCM-REVIEW-FLAG
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
           DISPLAY 'TCCCLSE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TCCCLSE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
