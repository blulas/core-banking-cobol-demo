      *================================================================*
      * PROGRAM  : TLLSETL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : TELLER BALANCING - SETTLEMENT. READS THE TBL        *
      *            TRANSACTIONS, UPDATES THE TBL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. TELLER CASH DRAWER BALANCING,   *
      *            BATCH PROOF AND OVER/SHORT                          *
      * INPUT    : TBLMST (KSDS), TBLTRN, BANKCTL                      *
      * OUTPUT   : TBLOUT, TLLSERPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, TLPEN, TLDTE, TLFEE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-12-15 EWS  ORIGINAL                                       *
      * 1992-12-15 TWH  VAULT-BUY CALCULATION CORRECTED CHG9838        *
      * 1995-05-09 LMB  VAULT-SELL CALCULATION CORRECTED INC6749       *
      * 1996-12-06 LMB  CASH-IN CALCULATION CORRECTED INC1077          *
      * 1997-06-13 RJT  CASH-IN FEE SCHEDULE UPDATE INC5417            *
      * 1997-07-22 SKN  VAULT-BUY CALCULATION CORRECTED PRB7516        *
      * 2005-08-09 JLM  AUDIT FINDING - BATCH CONTROL TOTALS CHG5462   *
      * 2008-08-20 SKN  OVERSHORT ROUNDING - HALF UP INC7327           *
      * 2009-08-05 GAF  CASH-OUT TABLE EXPANDED CHG3212                *
      * 2011-03-21 SKN  VAULT-SELL CALCULATION CORRECTED INC4193       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLLSETL.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "TBLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TBLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "TBLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "TBLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "TLLSERPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY TBLMST.
       FD  TRAN-FILE.
       COPY TBLTRN.
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
                                           VALUE 19.000000.
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
      * WS-VAULT-SE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VAULT-SE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000897500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000147000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000077500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007750000000670500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000725500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000134500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000102500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010800000000308500'.
       01  WS-VAULT-SE-BAND-TBL REDEFINES WS-VAULT-SE-BAND-VALUES.
           05  WS-VAULT-SE-BAND-ROW OCCURS 8 TIMES.
               10  WS-VAULT-SE-BAND-LOW    PIC 9(09)V99.
               10  WS-VAULT-SE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-OUT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-OUT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000590500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000244000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000900000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000634500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015150000000874500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020150000000806000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030150000000575500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00040150000000389500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00041150000000471000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00051150000000318000'.
       01  WS-CASH-OUT-BAND-TBL REDEFINES WS-CASH-OUT-BAND-VALUES.
           05  WS-CASH-OUT-BAND-ROW OCCURS 10 TIMES.
               10  WS-CASH-OUT-BAND-LOW    PIC 9(09)V99.
               10  WS-CASH-OUT-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VAULT-SE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VAULT-SE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000577000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000138500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000186500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000029000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000073500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020600000000686000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020650000000681000'.
       01  WS-VAULT-SE-TIER-TBL REDEFINES WS-VAULT-SE-TIER-VALUES.
           05  WS-VAULT-SE-TIER-ROW OCCURS 7 TIMES.
               10  WS-VAULT-SE-TIER-LOW    PIC 9(09)V99.
               10  WS-VAULT-SE-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-IN-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-IN-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000882000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000156000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000147000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000852500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000345000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014600000000479500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019600000000092000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019650000000768000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019900000000389000'.
       01  WS-CASH-IN-SCHED-TBL REDEFINES WS-CASH-IN-SCHED-VALUES.
           05  WS-CASH-IN-SCHED-ROW OCCURS 9 TIMES.
               10  WS-CASH-IN-SCHED-LOW    PIC 9(09)V99.
               10  WS-CASH-IN-SCHED-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  TLLSETL  TELLER BALANCING'.
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
       COPY TLPENP.
       COPY TLDTEP.
       COPY TLFEEP.

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
               MOVE 'TBLMST' TO ABND-FILE-NAME
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
           MOVE TBLT-KEY TO TBLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ASSESS-EXEMPT
               PERFORM 3010-ASSESS-EXEMPT
               PERFORM 3020-SET-CHANNEL
               PERFORM 3030-FLAG-BATCH
               PERFORM 3040-FLAG-CASH-IN
               PERFORM 3050-SELECT-DISCOUNT
               PERFORM 3060-PRORATE-VAULT-BUY
               PERFORM 3070-SET-SOURCE
               PERFORM 3080-ASSESS-HOLD
               PERFORM 3090-LIMIT-BATCH
               PERFORM 3100-FLAG-CASH-OUT
               PERFORM 3110-SET-CYCLE
               PERFORM 3120-SPLIT-BATCH
               PERFORM 3130-CHECK-LAST-PAY
               PERFORM 3140-AGE-LAST-MAINT
               PERFORM 3150-DESCRIBE-CHANNEL
               PERFORM 3160-ASSESS-HOLD
               PERFORM 3170-FLAG-CASH-IN
               PERFORM 3180-ASSESS-REVERSAL
               PERFORM 3190-AGE-EFFECTIVE
               PERFORM 3200-DESCRIBE-CHANNEL
               PERFORM 3210-AGE-LAST-STMT
               PERFORM 3220-FLAG-CASH-IN
               PERFORM 3230-ASSESS-SUSPEND
               PERFORM 3240-LIMIT-BATCH
               PERFORM 3250-SELECT-RATE
               PERFORM 3260-AGE-LAST-PAY
               PERFORM 3270-ASSESS-OVERRIDE
               PERFORM 3280-SET-CHANNEL
               PERFORM 3290-ASSESS-SUSPEND
               PERFORM 3300-COUNT-RETRY
               PERFORM 3310-LIMIT-CASH-OUT
               PERFORM 3320-SELECT-DISCOUNT
               PERFORM 3330-CHECK-LAST-PAY
               PERFORM 3340-COUNT-SEQ
               PERFORM 3350-COUNT-ITEM
               PERFORM 3360-PRORATE-VAULT-SELL
               PERFORM 3370-AGE-EFFECTIVE
               PERFORM 3380-SPLIT-CASH-IN
               PERFORM 3390-CHECK-EFFECTIVE
               PERFORM 3400-AGE-EXPIRY
               PERFORM 3410-AGE-EFF
               PERFORM 3420-SPLIT-OVERSHORT
               PERFORM 3430-COUNT-RETRY
               PERFORM 3440-FLAG-VAULT-BUY
               PERFORM 3450-ASSESS-HOLD
               PERFORM 3460-EDIT-CONTACT
               PERFORM 3470-EDIT-REJECT
               PERFORM 3480-LIMIT-OVERSHORT
               PERFORM 3490-ACCRUE-OVERSHORT
               PERFORM 3500-ACCUM-SOURCE
               PERFORM 3510-PRORATE-VAULT-SELL
               PERFORM 3520-PRORATE-BATCH
               PERFORM 3530-SET-TRAN
               PERFORM 3540-COUNT-NOTICE
               PERFORM 3550-SET-TRAN
               PERFORM 3560-AGE-EFF
               PERFORM 3570-FLAG-CASH-OUT
               PERFORM 3580-EDIT-OPERATOR
               PERFORM 3590-DESCRIBE-REASON
               PERFORM 3600-PRORATE-CASH-OUT
               PERFORM 3610-ACCRUE-VAULT-SELL
               PERFORM 3620-ACCUM-CHANNEL
               PERFORM 3630-FLAG-CASH-OUT
               PERFORM 3640-EDIT-NAME
               PERFORM 3650-EDIT-CUST-ID
               PERFORM 3660-DESCRIBE-DRAWER
               PERFORM 3670-ACCUM-REASON
               PERFORM 3680-SET-DRAWER
               PERFORM 3690-LIMIT-VAULT-BUY
               PERFORM 3700-LIMIT-ITEM
               PERFORM 3710-SELECT-FEE-PCT
               PERFORM 3720-AGE-EFF
               PERFORM 3730-CHECK-NOTICE
               PERFORM 3740-EDIT-NAME
               PERFORM 3750-PRORATE-BATCH
               PERFORM 3760-AGE-NOTICE
               PERFORM 3770-AGE-LAST-PAY
               PERFORM 3780-ASSESS-STOP
               PERFORM 3790-SET-PROOF-TYPE
               PERFORM 3800-LIMIT-CASH-IN
               PERFORM 3810-SELECT-RATE
               PERFORM 3820-DESCRIBE-CHANNEL
               PERFORM 3830-PRORATE-BATCH
               PERFORM 3840-SET-TRAN
               PERFORM 3850-SPLIT-OVERSHORT
               PERFORM 3860-EDIT-NAME
               PERFORM 3870-SPLIT-VAULT-SELL
               PERFORM 3880-SPLIT-BATCH
               PERFORM 3890-ASSESS-OVERRIDE
               PERFORM 3900-CHECK-LAST-STMT
               PERFORM 3910-CHECK-LAST-PAY
               PERFORM 3920-SPLIT-BATCH
               PERFORM 3930-SET-SOURCE
               PERFORM 3940-PRORATE-OVERSHORT
               PERFORM 3950-ACCRUE-VAULT-BUY
               PERFORM 3960-EDIT-MEMO
               PERFORM 3970-SPLIT-CASH-IN
               PERFORM 3980-LIMIT-ITEM
               PERFORM 3990-ACCUM-REASON
               PERFORM 4000-SPLIT-VAULT-SELL
               PERFORM 4010-COUNT-NOTICE
               PERFORM 4020-LIMIT-BATCH
               PERFORM 4030-ASSESS-REVERSAL
               PERFORM 4040-SET-TRAN
               PERFORM 4050-SELECT-RATE
               PERFORM 4060-SPLIT-CASH-OUT
               PERFORM 4070-SET-PROOF-TYPE
               PERFORM 4080-CHECK-LAST-STMT
               PERFORM 4090-SET-TRAN
               PERFORM 4100-COUNT-SEQ
               PERFORM 4110-ASSESS-EXEMPT
               PERFORM 4120-CHECK-NOTICE
               PERFORM 4130-PRORATE-VAULT-SELL
               PERFORM 4140-ACCRUE-VAULT-SELL
               PERFORM 4150-PRORATE-CASH-IN
               PERFORM 4160-AGE-NEXT-REVIEW
               PERFORM 4170-SET-PROOF-TYPE
               PERFORM 4180-LIMIT-VAULT-SELL
               PERFORM 4190-FLAG-CASH-OUT
               PERFORM 4200-EDIT-ADDR
               PERFORM 4210-ACCUM-TRAN
               PERFORM 4220-EDIT-OPERATOR
               PERFORM 4230-SPLIT-CASH-IN
               PERFORM 4240-ACCUM-REASON
               PERFORM 4250-SPLIT-OVERSHORT
               PERFORM 4260-CHECK-OPEN
               PERFORM 4270-PRORATE-VAULT-SELL
               PERFORM 4280-FLAG-CASH-OUT
               PERFORM 4290-EDIT-CUST-ID
               PERFORM 4300-ACCRUE-VAULT-SELL
               PERFORM 4310-CHECK-LAST-PAY
               PERFORM 4320-ACCRUE-OVERSHORT
               PERFORM 4330-SET-PROOF-TYPE
               PERFORM 4340-SPLIT-VAULT-SELL
               PERFORM 7040-CALL-TLPEN
               PERFORM 7010-CALL-TLDTE
               PERFORM 7000-CALL-TLFEE
               REWRITE TBL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'TBLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM TBL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH FEE ON THE VAULT SELL AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3000-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLT-BATCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLT-BATCH-AMT ROUNDED = TBLM-VAULT-SELL-AMT
                       * TBLM-RATE / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 3.00 TO TBLT-BATCH-AMT
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 10.00 TO TBLT-BATCH-AMT
               WHEN OTHER
                   COMPUTE TBLT-BATCH-AMT ROUNDED = TBLM-VAULT-SELL-AMT
                       * TBLM-RATE / 100
           END-EVALUATE
           IF TBLT-BATCH-AMT > 0 AND TBLT-BATCH-AMT < 5.00
               MOVE 5.00 TO TBLT-BATCH-AMT
           END-IF
           IF TBLT-BATCH-AMT > 75.00
               MOVE 75.00 TO TBLT-BATCH-AMT
           END-IF
           ADD TBLT-BATCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT SELL FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3010-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLM-VAULT-SELL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-STOP-FLAG = 'Y'
                   COMPUTE TBLM-VAULT-SELL-AMT ROUNDED = WS-WORK-AMT *
                       TBLM-DISCOUNT / 200
               WHEN TBLM-PROOF-TYPE-CD = 'JG'
                   MOVE 10.00 TO TBLM-VAULT-SELL-AMT
               WHEN TBLM-PROOF-TYPE-CD = 'RX'
                   MOVE 7.00 TO TBLM-VAULT-SELL-AMT
               WHEN OTHER
                   COMPUTE TBLM-VAULT-SELL-AMT ROUNDED = WS-WORK-AMT *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-VAULT-SELL-AMT > 0 AND TBLM-VAULT-SELL-AMT < 2.00
               MOVE 2.00 TO TBLM-VAULT-SELL-AMT
           END-IF
           IF TBLM-VAULT-SELL-AMT > 250.00
               MOVE 250.00 TO TBLM-VAULT-SELL-AMT
           END-IF
           ADD TBLM-VAULT-SELL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * D -> F WHEN LAST PAY DATE IS PAST
      * F -> M WHEN LAST PAY DATE IS PAST
      * M -> N WHEN LAST PAY DATE IS PAST
      * N -> R WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3020-SET-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'N' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'N'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TBLM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * BATCH AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3030-FLAG-BATCH.
           IF TBLT-BATCH-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLT-BATCH-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CASH IN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3040-FLAG-CASH-IN.
           IF TBLM-CASH-IN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-IN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE TIER TIER FOR THE VAULT SELL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3050-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-TIER-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TBLM-VAULT-SELL-AMT NOT < WS-VAULT-SE-TIER-LOW(WS-TX)
                   MOVE WS-VAULT-SE-TIER-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BATCH YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VAULT BUY YTD.
      *----------------------------------------------------------------*
       3060-PRORATE-VAULT-BUY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-BUY-YTD ROUNDED =
                   TBLM-BATCH-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-VAULT-BUY-YTD
           END-IF
           ADD TBLM-VAULT-BUY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN EFF DATE IS PAST
      * PT -> SZ WHEN EFF DATE IS PAST
      * SZ -> VK WHEN EFF DATE IS PAST
      * VK -> VW WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-SOURCE.
           EVALUATE TBLM-SOURCE-CD
               WHEN 'AF'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VW' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT BUY FEE ON THE CASH IN YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3080-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TBLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO TBLM-VAULT-BUY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   COMPUTE TBLM-VAULT-BUY-AMT ROUNDED = TBLM-CASH-IN-YTD
                       * TBLM-DISCOUNT / 200
               WHEN TBLM-CHANNEL-CD = 'D'
                   MOVE 12.00 TO TBLM-VAULT-BUY-AMT
               WHEN TBLM-CHANNEL-CD = 'F'
                   MOVE 5.00 TO TBLM-VAULT-BUY-AMT
               WHEN OTHER
                   COMPUTE TBLM-VAULT-BUY-AMT ROUNDED = TBLM-CASH-IN-YTD
                       * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-VAULT-BUY-AMT > 0 AND TBLM-VAULT-BUY-AMT < 10.00
               MOVE 10.00 TO TBLM-VAULT-BUY-AMT
           END-IF
           IF TBLM-VAULT-BUY-AMT > 150.00
               MOVE 150.00 TO TBLM-VAULT-BUY-AMT
           END-IF
           ADD TBLM-VAULT-BUY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BATCH AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3090-LIMIT-BATCH.
           IF TBLT-BATCH-AMT < 50.00
               MOVE 50.00 TO TBLT-BATCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLT-BATCH-AMT > 500.00
               MOVE 500.00 TO TBLT-BATCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3100-FLAG-CASH-OUT.
           IF TBLM-CASH-OUT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-OUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * KX -> TA WHEN LAST PAY DATE IS PAST
      * TA -> TG WHEN LAST PAY DATE IS PAST
      * TG -> WG WHEN LAST PAY DATE IS PAST
      * WG -> KX WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3110-SET-CYCLE.
           EVALUATE TBLM-CYCLE-CD
               WHEN 'KX'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TA' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TA'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TG'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WG' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WG'
                   IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KX' TO TBLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT BATCH AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3120-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLM-BATCH-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TBLM-BATCH-AMT - (WS-WORK-AMT * 4)
           COMPUTE TBLT-CASH-OUT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO TBLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-OVERSHORT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3150-DESCRIBE-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT SELL FEE ON THE OVERSHORT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3160-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TBLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO TBLM-VAULT-SELL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   COMPUTE TBLM-VAULT-SELL-AMT ROUNDED =
                       TBLT-OVERSHORT-AMT * TBLM-FEE-PCT / 200
               WHEN TBLT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO TBLM-VAULT-SELL-AMT
               WHEN TBLT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO TBLM-VAULT-SELL-AMT
               WHEN OTHER
                   COMPUTE TBLM-VAULT-SELL-AMT ROUNDED =
                       TBLT-OVERSHORT-AMT * TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLM-VAULT-SELL-AMT > 0 AND TBLM-VAULT-SELL-AMT < 10.00
               MOVE 10.00 TO TBLM-VAULT-SELL-AMT
           END-IF
           IF TBLM-VAULT-SELL-AMT > 150.00
               MOVE 150.00 TO TBLM-VAULT-SELL-AMT
           END-IF
           ADD TBLM-VAULT-SELL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASH IN AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3170-FLAG-CASH-IN.
           IF TBLM-CASH-IN-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-IN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT FEE ON THE WORK AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3180-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TBLT-OVERSHORT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLT-OVERSHORT-AMT ROUNDED = WS-WORK-AMT *
                       TBLM-RATE / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 20.00 TO TBLT-OVERSHORT-AMT
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 20.00 TO TBLT-OVERSHORT-AMT
               WHEN OTHER
                   COMPUTE TBLT-OVERSHORT-AMT ROUNDED = WS-WORK-AMT *
                       TBLM-RATE / 100
           END-EVALUATE
           IF TBLT-OVERSHORT-AMT > 0 AND TBLT-OVERSHORT-AMT < 2.00
               MOVE 2.00 TO TBLT-OVERSHORT-AMT
           END-IF
           IF TBLT-OVERSHORT-AMT > 250.00
               MOVE 250.00 TO TBLT-OVERSHORT-AMT
           END-IF
           ADD TBLT-OVERSHORT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3190-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3200-DESCRIBE-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'N'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3210-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH IN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3220-FLAG-CASH-IN.
           IF TBLM-CASH-IN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-IN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH FEE ON THE BATCH AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3230-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TBLM-BATCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   COMPUTE TBLM-BATCH-AMT ROUNDED = TBLT-BATCH-AMT *
                       TBLM-FEE-PCT / 200
               WHEN TBLT-TRAN-CD = 'BI'
                   MOVE 7.00 TO TBLM-BATCH-AMT
               WHEN TBLT-TRAN-CD = 'BO'
                   MOVE 5.00 TO TBLM-BATCH-AMT
               WHEN OTHER
                   COMPUTE TBLM-BATCH-AMT ROUNDED = TBLT-BATCH-AMT *
                       TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLM-BATCH-AMT > 0 AND TBLM-BATCH-AMT < 1.00
               MOVE 1.00 TO TBLM-BATCH-AMT
           END-IF
           IF TBLM-BATCH-AMT > 75.00
               MOVE 75.00 TO TBLM-BATCH-AMT
           END-IF
           ADD TBLM-BATCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * BATCH AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3240-LIMIT-BATCH.
           IF TBLM-BATCH-AMT < 50.00
               MOVE 50.00 TO TBLM-BATCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-BATCH-AMT > 2500.00
               MOVE 2500.00 TO TBLM-BATCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN SCHED TIER FOR THE VAULT BUY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3250-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-SCHED-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TBLM-VAULT-BUY-YTD NOT < WS-CASH-IN-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-IN-SCHED-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN YTD FEE ON THE CASH IN AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3270-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLM-CASH-IN-AMT *
                       TBLM-DISCOUNT / 200
               WHEN TBLM-REASON-CD = 'GS'
                   MOVE 10.00 TO TBLM-CASH-IN-YTD
               WHEN TBLM-REASON-CD = 'GV'
                   MOVE 5.00 TO TBLM-CASH-IN-YTD
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLM-CASH-IN-AMT *
                       TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-CASH-IN-YTD > 0 AND TBLM-CASH-IN-YTD < 2.00
               MOVE 2.00 TO TBLM-CASH-IN-YTD
           END-IF
           IF TBLM-CASH-IN-YTD > 50.00
               MOVE 50.00 TO TBLM-CASH-IN-YTD
           END-IF
           ADD TBLM-CASH-IN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST STMT DATE IS PAST
      * BAT -> MAL WHEN LAST STMT DATE IS PAST
      * MAL -> TEL WHEN LAST STMT DATE IS PAST
      * TEL -> ONL WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3280-SET-CHANNEL.
           EVALUATE TBLT-CHANNEL-CD
               WHEN 'BRN'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF TBLM-LAST-STMT-DATE > 0 AND TBLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO TBLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE BATCH FEE ON THE CASH OUT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3290-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO TBLT-BATCH-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE TBLT-BATCH-AMT ROUNDED = TBLT-CASH-OUT-AMT *
                       TBLM-FEE-PCT / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 7.00 TO TBLT-BATCH-AMT
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 10.00 TO TBLT-BATCH-AMT
               WHEN OTHER
                   COMPUTE TBLT-BATCH-AMT ROUNDED = TBLT-CASH-OUT-AMT *
                       TBLM-FEE-PCT / 100
           END-EVALUATE
           IF TBLT-BATCH-AMT > 0 AND TBLT-BATCH-AMT < 10.00
               MOVE 10.00 TO TBLT-BATCH-AMT
           END-IF
           IF TBLT-BATCH-AMT > 75.00
               MOVE 75.00 TO TBLT-BATCH-AMT
           END-IF
           ADD TBLT-BATCH-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 10
               MOVE 'Y' TO TBLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3310-LIMIT-CASH-OUT.
           IF TBLM-CASH-OUT-YTD < 25.00
               MOVE 25.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-OUT-YTD > 500.00
               MOVE 500.00 TO TBLM-CASH-OUT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN SCHED TIER FOR THE VAULT SELL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-SCHED-RATE(1) TO TBLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TBLT-VAULT-SELL-AMT NOT < WS-CASH-IN-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-IN-SCHED-RATE(WS-TX) TO TBLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3340-COUNT-SEQ.
           IF TBLT-SEQ < 9999999
               ADD 1 TO TBLT-SEQ
           END-IF
           IF TBLT-SEQ NOT < 6
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3350-COUNT-ITEM.
           IF TBLM-ITEM-CNT < 9999999
               ADD 1 TO TBLM-ITEM-CNT
           END-IF
           IF TBLM-ITEM-CNT NOT < 12
               MOVE 'Y' TO TBLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT SELL YTD.
      *----------------------------------------------------------------*
       3360-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-SELL-YTD ROUNDED =
                   TBLM-VAULT-SELL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-VAULT-SELL-YTD
           END-IF
           ADD TBLM-VAULT-SELL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EFFECTIVE-DATE > 0 AND TBLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CASH IN YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3380-SPLIT-CASH-IN.
           COMPUTE WS-WORK-AMT = TBLM-CASH-IN-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TBLM-CASH-IN-YTD - (WS-WORK-AMT * 3)
           COMPUTE TBLM-BATCH-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3390-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TBLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-EXPIRY-DATE > 0 AND TBLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLT-OVERSHORT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLT-OVERSHORT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLT-OVERSHORT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TBLT-OVERSHORT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLT-OVERSHORT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-OVERSHORT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TBLM-OVERSHORT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TBLM-OVERSHORT-AMT - (WS-WORK-AMT *
               3)
           COMPUTE TBLM-VAULT-BUY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3430-COUNT-RETRY.
           IF TBLM-RETRY-CNT < 9999999
               ADD 1 TO TBLM-RETRY-CNT
           END-IF
           IF TBLM-RETRY-CNT NOT < 10
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VAULT BUY YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3440-FLAG-VAULT-BUY.
           IF TBLM-VAULT-BUY-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-VAULT-BUY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN FEE ON THE OVERSHORT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3450-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN TBLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED = TBLM-OVERSHORT-AMT
                       * TBLM-RATE / 200
               WHEN TBLT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO TBLM-CASH-IN-AMT
               WHEN TBLT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO TBLM-CASH-IN-AMT
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-AMT ROUNDED = TBLM-OVERSHORT-AMT
                       * TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-CASH-IN-AMT > 0 AND TBLM-CASH-IN-AMT < 10.00
               MOVE 10.00 TO TBLM-CASH-IN-AMT
           END-IF
           IF TBLM-CASH-IN-AMT > 150.00
               MOVE 150.00 TO TBLM-CASH-IN-AMT
           END-IF
           ADD TBLM-CASH-IN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E144 CONTACT REQUIRED
      * E504 CASH OUT AMT NEGATIVE
      * E442 CASH OUT AMT OVER 5000
      * E888 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3460-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CONTACT = SPACES
                   MOVE 'E144' TO WS-REJECT-CODE
               WHEN TBLT-CASH-OUT-AMT < 0
                   MOVE 'E504' TO WS-REJECT-CODE
               WHEN TBLT-CASH-OUT-AMT > 5000.00
                   MOVE 'E442' TO WS-REJECT-CODE
               WHEN TBLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E888' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E457 REJECT CD REQUIRED
      * E396 CASH IN YTD NEGATIVE
      * E151 CASH IN YTD OVER 99999
      * E695 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3470-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-REJECT-CD = SPACES
                   MOVE 'E457' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-YTD < 0
                   MOVE 'E396' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-YTD > 99999.00
                   MOVE 'E151' TO WS-REJECT-CODE
               WHEN TBLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E695' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * OVERSHORT YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3480-LIMIT-OVERSHORT.
           IF TBLM-OVERSHORT-YTD < 50.00
               MOVE 50.00 TO TBLM-OVERSHORT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-OVERSHORT-YTD > 2500.00
               MOVE 2500.00 TO TBLM-OVERSHORT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-OVERSHORT.
           IF TBLT-OVERSHORT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-OVERSHORT-AMT * TBLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO TBLT-BATCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3500-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-SOURCE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VAULT SELL YTD.
      *----------------------------------------------------------------*
       3510-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-SELL-YTD ROUNDED =
                   TBLT-VAULT-SELL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-VAULT-SELL-YTD
           END-IF
           ADD TBLM-VAULT-SELL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE CASH IN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO BATCH AMT.
      *----------------------------------------------------------------*
       3520-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-BATCH-AMT ROUNDED =
                   TBLM-CASH-IN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-BATCH-AMT
           END-IF
           ADD TBLM-BATCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BI -> BO WHEN LAST ACT DATE IS PAST
      * BO -> VB WHEN LAST ACT DATE IS PAST
      * VB -> VS WHEN LAST ACT DATE IS PAST
      * VS -> OS WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3530-SET-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BO' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BO'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VB'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VS'
                   IF TBLM-LAST-ACT-DATE > 0 AND TBLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3540-COUNT-NOTICE.
           IF TBLM-NOTICE-CNT < 9999999
               ADD 1 TO TBLM-NOTICE-CNT
           END-IF
           IF TBLM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO TBLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BI -> BO WHEN NOTICE DATE IS PAST
      * BO -> VB WHEN NOTICE DATE IS PAST
      * VB -> VS WHEN NOTICE DATE IS PAST
      * VS -> OS WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3550-SET-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BO' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BO'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VB'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VS'
                   IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3560-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLT-EFF-DATE TO DTU-DATE-1
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
      * CASH OUT YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3570-FLAG-CASH-OUT.
           IF TBLM-CASH-OUT-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-OUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E292 OPERATOR REQUIRED
      * E499 BATCH AMT NEGATIVE
      * E007 BATCH AMT OVER 250000
      * E782 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3580-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-OPERATOR = SPACES
                   MOVE 'E292' TO WS-REJECT-CODE
               WHEN TBLT-BATCH-AMT < 0
                   MOVE 'E499' TO WS-REJECT-CODE
               WHEN TBLT-BATCH-AMT > 250000.00
                   MOVE 'E007' TO WS-REJECT-CODE
               WHEN TBLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E782' TO WS-REJECT-CODE
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
       3590-DESCRIBE-REASON.
           EVALUATE TBLM-REASON-CD
               WHEN 'GS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE BATCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH OUT AMT.
      *----------------------------------------------------------------*
       3600-PRORATE-CASH-OUT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLT-CASH-OUT-AMT ROUNDED =
                   TBLT-BATCH-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLT-CASH-OUT-AMT
           END-IF
           ADD TBLT-CASH-OUT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3610-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-AMT * TBLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO TBLM-BATCH-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE BATCH AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3620-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-CHANNEL-CD
                   ADD TBLM-BATCH-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-BATCH-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3630-FLAG-CASH-OUT.
           IF TBLT-CASH-OUT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE TBLT-CASH-OUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E572 NAME REQUIRED
      * E290 VAULT SELL AMT NEGATIVE
      * E877 VAULT SELL AMT OVER 99999
      * E378 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-NAME = SPACES
                   MOVE 'E572' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-AMT < 0
                   MOVE 'E290' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-AMT > 99999.00
                   MOVE 'E877' TO WS-REJECT-CODE
               WHEN TBLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E378' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E821 CUST ID REQUIRED
      * E204 BATCH YTD NEGATIVE
      * E644 BATCH YTD OVER 250000
      * E368 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CUST-ID = SPACES
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN TBLM-BATCH-YTD < 0
                   MOVE 'E204' TO WS-REJECT-CODE
               WHEN TBLM-BATCH-YTD > 250000.00
                   MOVE 'E644' TO WS-REJECT-CODE
               WHEN TBLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E368' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DRAWER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3660-DESCRIBE-DRAWER.
           EVALUATE TBLM-DRAWER-CD
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3670-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-REASON-CD
                   ADD TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-OVERSHORT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DRAWER CD:
      * D -> F WHEN OPEN DATE IS PAST
      * F -> R WHEN OPEN DATE IS PAST
      * R -> D WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3680-SET-DRAWER.
           EVALUATE TBLM-DRAWER-CD
               WHEN 'D'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO TBLM-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO TBLM-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO TBLM-DRAWER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT BUY AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3690-LIMIT-VAULT-BUY.
           IF TBLM-VAULT-BUY-AMT < 0.00
               MOVE 0.00 TO TBLM-VAULT-BUY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-BUY-AMT > 50000.00
               MOVE 50000.00 TO TBLM-VAULT-BUY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3700-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE BAND TIER FOR THE BATCH AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3710-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-BAND-RATE(1) TO TBLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF TBLT-BATCH-AMT NOT < WS-VAULT-SE-BAND-LOW(WS-TX)
                   MOVE WS-VAULT-SE-BAND-RATE(WS-TX) TO TBLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3720-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-IN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3730-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO TBLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E861 NAME REQUIRED
      * E639 VAULT SELL YTD NEGATIVE
      * E676 VAULT SELL YTD OVER 10000
      * E683 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3740-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-NAME = SPACES
                   MOVE 'E861' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-YTD < 0
                   MOVE 'E639' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-YTD > 10000.00
                   MOVE 'E676' TO WS-REJECT-CODE
               WHEN TBLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E683' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE BATCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3750-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   TBLM-BATCH-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3760-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NOTICE-DATE > 0 AND TBLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLT-CASH-OUT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD TBLT-CASH-OUT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD TBLT-CASH-OUT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD TBLT-CASH-OUT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLT-CASH-OUT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3770-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0 AND TBLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-BATCH-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VAULT SELL FEE ON THE CASH OUT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3780-ASSESS-STOP.
           EVALUATE TRUE
               WHEN TBLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO TBLM-VAULT-SELL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLM-VAULT-SELL-AMT ROUNDED =
                       TBLT-CASH-OUT-AMT * TBLM-DISCOUNT / 200
               WHEN TBLM-DRAWER-CD = 'D'
                   MOVE 10.00 TO TBLM-VAULT-SELL-AMT
               WHEN TBLM-DRAWER-CD = 'F'
                   MOVE 3.00 TO TBLM-VAULT-SELL-AMT
               WHEN OTHER
                   COMPUTE TBLM-VAULT-SELL-AMT ROUNDED =
                       TBLT-CASH-OUT-AMT * TBLM-DISCOUNT / 100
           END-EVALUATE
           IF TBLM-VAULT-SELL-AMT > 0 AND TBLM-VAULT-SELL-AMT < 10.00
               MOVE 10.00 TO TBLM-VAULT-SELL-AMT
           END-IF
           IF TBLM-VAULT-SELL-AMT > 35.00
               MOVE 35.00 TO TBLM-VAULT-SELL-AMT
           END-IF
           ADD TBLM-VAULT-SELL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PROOF TYPE CD:
      * JG -> RX WHEN NEXT REVIEW DATE IS PAST
      * RX -> TW WHEN NEXT REVIEW DATE IS PAST
      * TW -> UG WHEN NEXT REVIEW DATE IS PAST
      * UG -> XY WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-PROOF-TYPE.
           EVALUATE TBLM-PROOF-TYPE-CD
               WHEN 'JG'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RX' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RX'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TW' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TW'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UG' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'XY' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH IN YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3800-LIMIT-CASH-IN.
           IF TBLM-CASH-IN-YTD < 25.00
               MOVE 25.00 TO TBLM-CASH-IN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-CASH-IN-YTD > 5000.00
               MOVE 5000.00 TO TBLM-CASH-IN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VAULT SE TIER TIER FOR THE VAULT SELL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3810-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VAULT-SE-TIER-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF TBLM-VAULT-SELL-AMT NOT < WS-VAULT-SE-TIER-LOW(WS-TX)
                   MOVE WS-VAULT-SE-TIER-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-CHANNEL.
           EVALUATE TBLM-CHANNEL-CD
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'F'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'N'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO BATCH AMT.
      *----------------------------------------------------------------*
       3830-PRORATE-BATCH.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-BATCH-AMT ROUNDED =
                   TBLM-OVERSHORT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-BATCH-AMT
           END-IF
           ADD TBLM-BATCH-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BI -> BO WHEN EFF DATE IS PAST
      * BO -> VB WHEN EFF DATE IS PAST
      * VB -> VS WHEN EFF DATE IS PAST
      * VS -> OS WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BO' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BO'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VB' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VB'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VS'
                   IF TBLT-EFF-DATE > 0 AND TBLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'OS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3850-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TBLT-OVERSHORT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = TBLT-OVERSHORT-AMT - (WS-WORK-AMT *
               4)
           COMPUTE TBLM-OVERSHORT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E894 NAME REQUIRED
      * E317 VAULT SELL YTD NEGATIVE
      * E580 VAULT SELL YTD OVER 250000
      * E284 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3860-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-NAME = SPACES
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-YTD < 0
                   MOVE 'E317' TO WS-REJECT-CODE
               WHEN TBLM-VAULT-SELL-YTD > 250000.00
                   MOVE 'E580' TO WS-REJECT-CODE
               WHEN TBLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E284' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3870-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLM-VAULT-SELL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = TBLM-VAULT-SELL-AMT - (WS-WORK-AMT *
               3)
           COMPUTE TBLM-OVERSHORT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BATCH YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3880-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLM-BATCH-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TBLM-BATCH-YTD - (WS-WORK-AMT * 4)
           COMPUTE TBLM-OVERSHORT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT FEE ON THE CASH OUT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3890-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN TBLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO TBLT-OVERSHORT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-REVIEW-FLAG = 'Y'
                   COMPUTE TBLT-OVERSHORT-AMT ROUNDED =
                       TBLM-CASH-OUT-AMT * TBLM-RATE / 200
               WHEN TBLM-REASON-CD = 'GS'
                   MOVE 7.00 TO TBLT-OVERSHORT-AMT
               WHEN TBLM-REASON-CD = 'GV'
                   MOVE 15.00 TO TBLT-OVERSHORT-AMT
               WHEN OTHER
                   COMPUTE TBLT-OVERSHORT-AMT ROUNDED =
                       TBLM-CASH-OUT-AMT * TBLM-RATE / 100
           END-EVALUATE
           IF TBLT-OVERSHORT-AMT > 0 AND TBLT-OVERSHORT-AMT < 5.00
               MOVE 5.00 TO TBLT-OVERSHORT-AMT
           END-IF
           IF TBLT-OVERSHORT-AMT > 250.00
               MOVE 250.00 TO TBLT-OVERSHORT-AMT
           END-IF
           ADD TBLT-OVERSHORT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3900-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3910-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT BATCH YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3920-SPLIT-BATCH.
           COMPUTE WS-WORK-AMT = TBLM-BATCH-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TBLM-BATCH-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AF -> PT WHEN OPEN DATE IS PAST
      * PT -> SZ WHEN OPEN DATE IS PAST
      * SZ -> VK WHEN OPEN DATE IS PAST
      * VK -> VW WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3930-SET-SOURCE.
           EVALUATE TBLM-SOURCE-CD
               WHEN 'AF'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PT' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PT'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SZ' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SZ'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VK' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VK'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VW' TO TBLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE BATCH AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO OVERSHORT AMT.
      *----------------------------------------------------------------*
       3940-PRORATE-OVERSHORT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-OVERSHORT-AMT ROUNDED =
                   TBLT-BATCH-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO TBLM-OVERSHORT-AMT
           END-IF
           ADD TBLM-OVERSHORT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT BUY YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3950-ACCRUE-VAULT-BUY.
           IF TBLM-VAULT-BUY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-BUY-YTD * TBLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TBLM-OVERSHORT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E159 MEMO REQUIRED
      * E515 CASH IN YTD NEGATIVE
      * E199 CASH IN YTD OVER 250000
      * E295 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-MEMO = SPACES
                   MOVE 'E159' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-YTD < 0
                   MOVE 'E515' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-YTD > 250000.00
                   MOVE 'E199' TO WS-REJECT-CODE
               WHEN TBLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E295' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH IN YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3970-SPLIT-CASH-IN.
           COMPUTE WS-WORK-AMT = TBLM-CASH-IN-YTD / 4
           COMPUTE WS-WORK-AMT-2 = TBLM-CASH-IN-YTD - (WS-WORK-AMT * 4)
           COMPUTE TBLM-VAULT-BUY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3980-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLT-VAULT-SELL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TBLT-VAULT-SELL-AMT - (WS-WORK-AMT *
               2)
           COMPUTE TBLM-CASH-IN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4010-COUNT-NOTICE.
           IF TBLM-NOTICE-CNT < 9999999
               ADD 1 TO TBLM-NOTICE-CNT
           END-IF
           IF TBLM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO TBLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * BATCH AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4020-LIMIT-BATCH.
           IF TBLM-BATCH-AMT < 100.00
               MOVE 100.00 TO TBLM-BATCH-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-BATCH-AMT > 2500.00
               MOVE 2500.00 TO TBLM-BATCH-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH IN YTD FEE ON THE OVERSHORT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4030-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO TBLM-CASH-IN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLM-SUSPEND-FLAG = 'Y'
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLM-OVERSHORT-YTD
                       * TBLM-RATE / 200
               WHEN TBLM-REASON-CD = 'GS'
                   MOVE 15.00 TO TBLM-CASH-IN-YTD
               WHEN TBLM-REASON-CD = 'GV'
                   MOVE 20.00 TO TBLM-CASH-IN-YTD
               WHEN OTHER
                   COMPUTE TBLM-CASH-IN-YTD ROUNDED = TBLM-OVERSHORT-YTD
                       * TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-CASH-IN-YTD > 0 AND TBLM-CASH-IN-YTD < 5.00
               MOVE 5.00 TO TBLM-CASH-IN-YTD
           END-IF
           IF TBLM-CASH-IN-YTD > 150.00
               MOVE 150.00 TO TBLM-CASH-IN-YTD
           END-IF
           ADD TBLM-CASH-IN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BI -> BO WHEN NEXT REVIEW DATE IS PAST
      * BO -> VB WHEN NEXT REVIEW DATE IS PAST
      * VB -> VS WHEN NEXT REVIEW DATE IS PAST
      * VS -> OS WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4040-SET-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BO' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BO'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VB' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VB'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VS'
                   IF TBLM-NEXT-REVIEW-DATE > 0 AND
                       TBLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'OS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASH IN SCHED TIER FOR THE VAULT SELL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4050-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-IN-SCHED-RATE(1) TO TBLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF TBLT-VAULT-SELL-AMT NOT < WS-CASH-IN-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-IN-SCHED-RATE(WS-TX) TO TBLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF TBLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO TBLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH OUT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4060-SPLIT-CASH-OUT.
           COMPUTE WS-WORK-AMT = TBLM-CASH-OUT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = TBLM-CASH-OUT-YTD - (WS-WORK-AMT * 3)
           COMPUTE TBLT-OVERSHORT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PROOF TYPE CD:
      * JG -> RX WHEN OPEN DATE IS PAST
      * RX -> TW WHEN OPEN DATE IS PAST
      * TW -> UG WHEN OPEN DATE IS PAST
      * UG -> XY WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4070-SET-PROOF-TYPE.
           EVALUATE TBLM-PROOF-TYPE-CD
               WHEN 'JG'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RX' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RX'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TW' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TW'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF TBLM-OPEN-DATE > 0 AND TBLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XY' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4080-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BI -> BO WHEN LAST MAINT DATE IS PAST
      * BO -> VB WHEN LAST MAINT DATE IS PAST
      * VB -> VS WHEN LAST MAINT DATE IS PAST
      * VS -> OS WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4090-SET-TRAN.
           EVALUATE TBLT-TRAN-CD
               WHEN 'BI'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BO' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BO'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VB' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VB'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'VS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VS'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'OS' TO TBLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-SEQ.
           IF TBLT-SEQ < 9999999
               ADD 1 TO TBLT-SEQ
           END-IF
           IF TBLT-SEQ NOT < 5
               MOVE 'Y' TO TBLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE OVERSHORT FEE ON THE VAULT SELL AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4110-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN TBLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO TBLM-OVERSHORT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN TBLT-REVERSAL-FLAG = 'Y'
                   COMPUTE TBLM-OVERSHORT-AMT ROUNDED =
                       TBLM-VAULT-SELL-AMT * TBLM-RATE / 200
               WHEN TBLT-TRAN-CD = 'BI'
                   MOVE 10.00 TO TBLM-OVERSHORT-AMT
               WHEN TBLT-TRAN-CD = 'BO'
                   MOVE 5.00 TO TBLM-OVERSHORT-AMT
               WHEN OTHER
                   COMPUTE TBLM-OVERSHORT-AMT ROUNDED =
                       TBLM-VAULT-SELL-AMT * TBLM-RATE / 100
           END-EVALUATE
           IF TBLM-OVERSHORT-AMT > 0 AND TBLM-OVERSHORT-AMT < 1.00
               MOVE 1.00 TO TBLM-OVERSHORT-AMT
           END-IF
           IF TBLM-OVERSHORT-AMT > 250.00
               MOVE 250.00 TO TBLM-OVERSHORT-AMT
           END-IF
           ADD TBLM-OVERSHORT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4120-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TBLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VAULT SELL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT SELL YTD.
      *----------------------------------------------------------------*
       4130-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-SELL-YTD ROUNDED =
                   TBLT-VAULT-SELL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-VAULT-SELL-YTD
           END-IF
           ADD TBLM-VAULT-SELL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4140-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-AMT * TBLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE OVERSHORT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH IN AMT.
      *----------------------------------------------------------------*
       4150-PRORATE-CASH-IN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-CASH-IN-AMT ROUNDED =
                   TBLT-OVERSHORT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-CASH-IN-AMT
           END-IF
           ADD TBLM-CASH-IN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4160-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF TBLM-NEXT-REVIEW-DATE > 0 AND TBLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD TBLM-CASH-IN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PROOF TYPE CD:
      * JG -> RX WHEN POST DATE IS PAST
      * RX -> TW WHEN POST DATE IS PAST
      * TW -> UG WHEN POST DATE IS PAST
      * UG -> XY WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4170-SET-PROOF-TYPE.
           EVALUATE TBLM-PROOF-TYPE-CD
               WHEN 'JG'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RX' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RX'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TW' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TW'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF TBLT-POST-DATE > 0 AND TBLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XY' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VAULT SELL AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4180-LIMIT-VAULT-SELL.
           IF TBLM-VAULT-SELL-AMT < 25.00
               MOVE 25.00 TO TBLM-VAULT-SELL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF TBLM-VAULT-SELL-AMT > 1000.00
               MOVE 1000.00 TO TBLM-VAULT-SELL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH OUT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4190-FLAG-CASH-OUT.
           IF TBLM-CASH-OUT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-OUT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E868 ADDR REQUIRED
      * E050 CASH IN AMT NEGATIVE
      * E911 CASH IN AMT OVER 5000
      * E259 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-ADDR = SPACES
                   MOVE 'E868' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT < 0
                   MOVE 'E050' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-AMT > 5000.00
                   MOVE 'E911' TO WS-REJECT-CODE
               WHEN TBLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E259' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE OVERSHORT AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4210-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLT-TRAN-CD
                   ADD TBLM-OVERSHORT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLM-OVERSHORT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E303 OPERATOR REQUIRED
      * E186 BATCH AMT NEGATIVE
      * E616 BATCH AMT OVER 250000
      * E667 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4220-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLT-OPERATOR = SPACES
                   MOVE 'E303' TO WS-REJECT-CODE
               WHEN TBLT-BATCH-AMT < 0
                   MOVE 'E186' TO WS-REJECT-CODE
               WHEN TBLT-BATCH-AMT > 250000.00
                   MOVE 'E616' TO WS-REJECT-CODE
               WHEN TBLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E667' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH IN AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4230-SPLIT-CASH-IN.
           COMPUTE WS-WORK-AMT = TBLM-CASH-IN-AMT / 2
           COMPUTE WS-WORK-AMT-2 = TBLM-CASH-IN-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH OUT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4240-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = TBLM-REASON-CD
                   ADD TBLT-CASH-OUT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE TBLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE TBLT-CASH-OUT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT OVERSHORT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4250-SPLIT-OVERSHORT.
           COMPUTE WS-WORK-AMT = TBLM-OVERSHORT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = TBLM-OVERSHORT-YTD - (WS-WORK-AMT *
               2)
           COMPUTE TBLM-VAULT-BUY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4260-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF TBLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH OUT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VAULT SELL AMT.
      *----------------------------------------------------------------*
       4270-PRORATE-VAULT-SELL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE TBLM-VAULT-SELL-AMT ROUNDED =
                   TBLT-CASH-OUT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO TBLM-VAULT-SELL-AMT
           END-IF
           ADD TBLM-VAULT-SELL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CASH OUT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4280-FLAG-CASH-OUT.
           IF TBLM-CASH-OUT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE TBLM-CASH-OUT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E655 CUST ID REQUIRED
      * E948 CASH IN YTD NEGATIVE
      * E558 CASH IN YTD OVER 10000
      * E649 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4290-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN TBLM-CUST-ID = SPACES
                   MOVE 'E655' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-YTD < 0
                   MOVE 'E948' TO WS-REJECT-CODE
               WHEN TBLM-CASH-IN-YTD > 10000.00
                   MOVE 'E558' TO WS-REJECT-CODE
               WHEN TBLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E649' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VAULT SELL YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4300-ACCRUE-VAULT-SELL.
           IF TBLM-VAULT-SELL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLM-VAULT-SELL-YTD * TBLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4310-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF TBLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE TBLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO TBLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON OVERSHORT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4320-ACCRUE-OVERSHORT.
           IF TBLT-OVERSHORT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   TBLT-OVERSHORT-AMT * TBLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO TBLM-VAULT-SELL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PROOF TYPE CD:
      * JG -> RX WHEN LAST MAINT DATE IS PAST
      * RX -> TW WHEN LAST MAINT DATE IS PAST
      * TW -> UG WHEN LAST MAINT DATE IS PAST
      * UG -> XY WHEN LAST MAINT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4330-SET-PROOF-TYPE.
           EVALUATE TBLM-PROOF-TYPE-CD
               WHEN 'JG'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RX' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RX'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'TW' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TW'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UG' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF TBLM-LAST-MAINT-DATE > 0 AND TBLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XY' TO TBLM-PROOF-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VAULT SELL AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4340-SPLIT-VAULT-SELL.
           COMPUTE WS-WORK-AMT = TBLT-VAULT-SELL-AMT / 12
           COMPUTE WS-WORK-AMT-2 = TBLT-VAULT-SELL-AMT - (WS-WORK-AMT *
               12)
           COMPUTE TBLM-OVERSHORT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-TLPEN.
           INITIALIZE TLPEN-PARMS
           MOVE TBLM-OVERSHORT-AMT
             TO TLPEN-OVERSHORT-AMT
           MOVE TBLM-VAULT-BUY-AMT
             TO TLPEN-VAULT-BUY-AMT
           MOVE TBLM-CASH-IN-AMT
             TO TLPEN-CASH-IN-AMT
           MOVE TBLM-CASH-OUT-YTD
             TO TLPEN-CASH-OUT-YTD
           MOVE TBLM-BATCH-AMT
             TO TLPEN-BATCH-AMT
           MOVE TBLM-CASH-OUT-AMT
             TO TLPEN-CASH-OUT-AMT
           MOVE TBLM-OPEN-DATE
             TO TLPEN-OPEN-DATE
           MOVE TBLM-EFFECTIVE-DATE
             TO TLPEN-EFFECTIVE-DATE
           MOVE TBLM-NOTICE-DATE
             TO TLPEN-NOTICE-DATE
           MOVE TBLM-NEXT-REVIEW-DATE
             TO TLPEN-NEXT-REVIEW-DATE
           MOVE TBLM-DRAWER-CD
             TO TLPEN-DRAWER-CD
           MOVE TBLM-REASON-CD
             TO TLPEN-REASON-CD
           MOVE TBLM-CHANNEL-CD
             TO TLPEN-CHANNEL-CD
           MOVE TBLM-SOURCE-CD
             TO TLPEN-SOURCE-CD
           MOVE TBLM-SUSPEND-FLAG
             TO TLPEN-SUSPEND-FLAG
           MOVE TBLM-REVIEW-FLAG
             TO TLPEN-REVIEW-FLAG
           MOVE TBLM-HOLD-FLAG
             TO TLPEN-HOLD-FLAG
           MOVE TBLM-ITEM-CNT
             TO TLPEN-ITEM-CNT
           MOVE TBLM-RETRY-CNT
             TO TLPEN-RETRY-CNT
           MOVE TBLM-RATE
             TO TLPEN-RATE
           MOVE TBLM-FEE-PCT
             TO TLPEN-FEE-PCT
           MOVE TBLM-REF-NO
             TO TLPEN-REF-NO
           MOVE TBLM-DESC
             TO TLPEN-DESC
           CALL 'TLPEN' USING TLPEN-PARMS
           EVALUATE TLPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLPEN-OVERSHORT-AMT
                     TO TBLM-OVERSHORT-AMT
                   MOVE TLPEN-VAULT-BUY-AMT
                     TO TBLM-VAULT-BUY-AMT
                   MOVE TLPEN-CASH-OUT-YTD
                     TO TBLM-CASH-OUT-YTD
                   MOVE TLPEN-BATCH-AMT
                     TO TBLM-BATCH-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-TLPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-TLDTE.
           INITIALIZE TLDTE-PARMS
           MOVE TBLM-CASH-IN-YTD
             TO TLDTE-CASH-IN-YTD
           MOVE TBLM-CASH-OUT-YTD
             TO TLDTE-CASH-OUT-YTD
           MOVE TBLM-CASH-OUT-AMT
             TO TLDTE-CASH-OUT-AMT
           MOVE TBLM-VAULT-BUY-YTD
             TO TLDTE-VAULT-BUY-YTD
           MOVE TBLM-BATCH-AMT
             TO TLDTE-BATCH-AMT
           MOVE TBLM-CASH-IN-AMT
             TO TLDTE-CASH-IN-AMT
           MOVE TBLM-LAST-STMT-DATE
             TO TLDTE-LAST-STMT-DATE
           MOVE TBLM-LAST-ACT-DATE
             TO TLDTE-LAST-ACT-DATE
           MOVE TBLM-NOTICE-DATE
             TO TLDTE-NOTICE-DATE
           MOVE TBLM-EFFECTIVE-DATE
             TO TLDTE-EFFECTIVE-DATE
           MOVE TBLM-CYCLE-CD
             TO TLDTE-CYCLE-CD
           MOVE TBLM-CHANNEL-CD
             TO TLDTE-CHANNEL-CD
           MOVE TBLM-SOURCE-CD
             TO TLDTE-SOURCE-CD
           MOVE TBLM-DRAWER-CD
             TO TLDTE-DRAWER-CD
           MOVE TBLM-SUSPEND-FLAG
             TO TLDTE-SUSPEND-FLAG
           MOVE TBLM-HOLD-FLAG
             TO TLDTE-HOLD-FLAG
           MOVE TBLM-STOP-FLAG
             TO TLDTE-STOP-FLAG
           MOVE TBLM-NOTICE-CNT
             TO TLDTE-NOTICE-CNT
           MOVE TBLM-RETRY-CNT
             TO TLDTE-RETRY-CNT
           MOVE TBLM-DISCOUNT
             TO TLDTE-DISCOUNT
           MOVE TBLM-FEE-PCT
             TO TLDTE-FEE-PCT
           MOVE TBLM-DESC
             TO TLDTE-DESC
           MOVE TBLM-CONTACT
             TO TLDTE-CONTACT
           CALL 'TLDTE' USING TLDTE-PARMS
           EVALUATE TLDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLDTE-CASH-IN-YTD
                     TO TBLM-CASH-IN-YTD
                   MOVE TLDTE-CASH-OUT-AMT
                     TO TBLM-CASH-OUT-AMT
                   MOVE TLDTE-VAULT-BUY-YTD
                     TO TBLM-VAULT-BUY-YTD
                   MOVE TLDTE-BATCH-AMT
                     TO TBLM-BATCH-AMT
                   MOVE TLDTE-SUSPEND-FLAG
                     TO TBLM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-TLDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE TBL FIELDS TO TLFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-TLFEE.
           INITIALIZE TLFEE-PARMS
           MOVE TBLM-VAULT-BUY-AMT
             TO TLFEE-VAULT-BUY-AMT
           MOVE TBLM-VAULT-SELL-YTD
             TO TLFEE-VAULT-SELL-YTD
           MOVE TBLM-CASH-IN-YTD
             TO TLFEE-CASH-IN-YTD
           MOVE TBLM-VAULT-BUY-YTD
             TO TLFEE-VAULT-BUY-YTD
           MOVE TBLM-VAULT-SELL-AMT
             TO TLFEE-VAULT-SELL-AMT
           MOVE TBLM-CASH-OUT-YTD
             TO TLFEE-CASH-OUT-YTD
           MOVE TBLM-OPEN-DATE
             TO TLFEE-OPEN-DATE
           MOVE TBLM-NEXT-REVIEW-DATE
             TO TLFEE-NEXT-REVIEW-DATE
           MOVE TBLM-LAST-PAY-DATE
             TO TLFEE-LAST-PAY-DATE
           MOVE TBLM-LAST-ACT-DATE
             TO TLFEE-LAST-ACT-DATE
           MOVE TBLM-CHANNEL-CD
             TO TLFEE-CHANNEL-CD
           MOVE TBLM-PROOF-TYPE-CD
             TO TLFEE-PROOF-TYPE-CD
           MOVE TBLM-DRAWER-CD
             TO TLFEE-DRAWER-CD
           MOVE TBLM-CYCLE-CD
             TO TLFEE-CYCLE-CD
           MOVE TBLM-EXEMPT-FLAG
             TO TLFEE-EXEMPT-FLAG
           MOVE TBLM-STOP-FLAG
             TO TLFEE-STOP-FLAG
           MOVE TBLM-OVERRIDE-FLAG
             TO TLFEE-OVERRIDE-FLAG
           MOVE TBLM-EXCEPT-CNT
             TO TLFEE-EXCEPT-CNT
           MOVE TBLM-NOTICE-CNT
             TO TLFEE-NOTICE-CNT
           MOVE TBLM-DISCOUNT
             TO TLFEE-DISCOUNT
           MOVE TBLM-RATE
             TO TLFEE-RATE
           MOVE TBLM-DESC
             TO TLFEE-DESC
           MOVE TBLM-NAME
             TO TLFEE-NAME
           CALL 'TLFEE' USING TLFEE-PARMS
           EVALUATE TLFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE TLFEE-VAULT-BUY-AMT
                     TO TBLM-VAULT-BUY-AMT
                   MOVE TLFEE-VAULT-SELL-YTD
                     TO TBLM-VAULT-SELL-YTD
                   MOVE TLFEE-VAULT-BUY-YTD
                     TO TBLM-VAULT-BUY-YTD
                   MOVE TLFEE-VAULT-SELL-AMT
                     TO TBLM-VAULT-SELL-AMT
                   MOVE TLFEE-EXEMPT-FLAG
                     TO TBLM-EXEMPT-FLAG
                   MOVE TLFEE-STOP-FLAG
                     TO TBLM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-TLFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE TBLT-KEY TO RD-KEY
           MOVE TBLT-TRAN-CD TO RD-CODE
           MOVE TBLT-CASH-OUT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE TBLT-KEY TO RD-KEY
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
           DISPLAY 'TLLSETL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'TLLSETL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
