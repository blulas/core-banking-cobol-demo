      *================================================================*
      * PROGRAM  : BPYCLSE                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : BILL PAYMENT SERVICE - CLOSURE PROCESSING. READS THE*
      *            BPY TRANSACTIONS, UPDATES THE BPY MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. CONSUMER BILL PAYMENT  *
      *            PAYEE MAINTENANCE AND CHECK OR EFT DISBURSEMENT     *
      * INPUT    : BPYMST (KSDS), BPYTRN, BANKCTL                      *
      * OUTPUT   : BPYOUT, BPYCLRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, BPSTAT, BPELG, BPPRC    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-09-20 JLM  ORIGINAL                                       *
      * 1996-11-18 RJT  AUDIT FINDING - REFUND CONTROL TOTALS CHG1658  *
      * 1997-08-18 EWS  FEE CALCULATION CORRECTED PRB8031              *
      * 1997-10-01 MKL  RETURNED LIMIT REVISED INC8828                 *
      * 1997-07-18 EWS  NEW REFUND REPORT COLUMNS INC1931              *
      * 1999-01-26 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-04-04 LMB  RETURNED NOTICE TEXT PER LEGAL PRB9986         *
      * 2001-02-28 PQR  PAYMENT EXCEPTIONS TO OPS QUEUE INC7696        *
      * 2001-02-05 EWS  RETURNED LIMIT REVISED CHG3089                 *
      * 2003-01-06 TWH  RETURNED STATUS CODES ADDED INC0610            *
      * 2006-10-02 LMB  REFUND LIMIT REVISED CHG5276                   *
      * 2008-06-08 JLM  REFUND CUTOFF MOVED PRB7875                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BPYCLSE.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "BPYMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS BPYM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "BPYTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "BPYOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "BPYCLRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY BPYMST.
       FD  TRAN-FILE.
       COPY BPYTRN.
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
                                           VALUE 9.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 31.
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
      * WS-REFUND-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REFUND-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000154000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000552000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000153000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000565500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000819000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012050000000770000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022050000000536500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022150000000756000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027150000000172000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00037150000000145000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00037200000000128000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00047200000000403000'.
       01  WS-REFUND-SCHED-TBL REDEFINES WS-REFUND-SCHED-VALUES.
           05  WS-REFUND-SCHED-ROW OCCURS 12 TIMES.
               10  WS-REFUND-SCHED-LOW     PIC 9(09)V99.
               10  WS-REFUND-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYMENT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000753000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000122500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000483500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010550000000690500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020550000000343000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020800000000096000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021800000000132000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021900000000863000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022000000000517000'.
       01  WS-PAYMENT-BAND-TBL REDEFINES WS-PAYMENT-BAND-VALUES.
           05  WS-PAYMENT-BAND-ROW OCCURS 9 TIMES.
               10  WS-PAYMENT-BAND-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000060000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000439500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000093000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000140000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000658500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000250000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000479000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000546500'.
       01  WS-FEE-SCHED-TBL REDEFINES WS-FEE-SCHED-VALUES.
           05  WS-FEE-SCHED-ROW OCCURS 8 TIMES.
               10  WS-FEE-SCHED-LOW        PIC 9(09)V99.
               10  WS-FEE-SCHED-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  BPYCLSE  BILL PAYMENT SERVICE'.
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
       COPY BPSTATP.
       COPY BPELGP.
       COPY BPPRCP.

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
               MOVE 'BPYMST' TO ABND-FILE-NAME
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
           MOVE BPYT-KEY TO BPYM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-FLAG-FEE
               PERFORM 3010-LIMIT-PAYMENT
               PERFORM 3020-FLAG-ITEM
               PERFORM 3030-PRORATE-PAYMENT
               PERFORM 3040-SET-CYCLE
               PERFORM 3050-ACCUM-TRAN
               PERFORM 3060-EDIT-MEMO
               PERFORM 3070-ACCUM-CYCLE
               PERFORM 3080-SET-CHANNEL
               PERFORM 3090-ASSESS-PREFERRED
               PERFORM 3100-ACCUM-REASON
               PERFORM 3110-ASSESS-OVERRIDE
               PERFORM 3120-COUNT-SEQ
               PERFORM 3130-SPLIT-REFUND
               PERFORM 3140-SELECT-MARGIN
               PERFORM 3150-ASSESS-PREFERRED
               PERFORM 3160-COUNT-ITEM
               PERFORM 3170-ACCUM-CYCLE
               PERFORM 3180-DESCRIBE-CYCLE
               PERFORM 3190-AGE-NOTICE
               PERFORM 3200-FLAG-RETURNED
               PERFORM 3210-DESCRIBE-STATUS
               PERFORM 3220-DESCRIBE-PAYEE-TYPE
               PERFORM 3230-SET-TRAN
               PERFORM 3240-SELECT-PENALTY-PCT
               PERFORM 3250-ASSESS-OVERRIDE
               PERFORM 3260-ASSESS-SUSPEND
               PERFORM 3270-DESCRIBE-REMIT-METHOD
               PERFORM 3280-ACCRUE-FEE
               PERFORM 3290-ASSESS-HOLD
               PERFORM 3300-SET-CYCLE
               PERFORM 3310-PRORATE-FEE
               PERFORM 3320-SET-TRAN
               PERFORM 3330-FLAG-REFUND
               PERFORM 3340-SET-STATUS
               PERFORM 3350-SPLIT-PAYMENT
               PERFORM 3360-SELECT-RATE
               PERFORM 3370-SPLIT-PAYMENT
               PERFORM 3380-COUNT-PAYMENT
               PERFORM 3390-EDIT-REJECT
               PERFORM 3400-ACCUM-CYCLE
               PERFORM 3410-FLAG-ITEM
               PERFORM 3420-SET-PAYEE-TYPE
               PERFORM 3430-DESCRIBE-CYCLE
               PERFORM 3440-DESCRIBE-SOURCE
               PERFORM 3450-SPLIT-REFUND
               PERFORM 3460-COUNT-CYCLE
               PERFORM 3470-SELECT-PENALTY-PCT
               PERFORM 3480-FLAG-REFUND
               PERFORM 3490-EDIT-REF-NO
               PERFORM 3500-LIMIT-FEE
               PERFORM 3510-ACCRUE-FEE
               PERFORM 3520-SELECT-RATE
               PERFORM 3530-FLAG-PAYMENT
               PERFORM 3540-ASSESS-AUTO-RENEW
               PERFORM 3550-SELECT-RATE
               PERFORM 3560-ACCUM-CYCLE
               PERFORM 3570-DESCRIBE-CHANNEL
               PERFORM 3580-PRORATE-FEE
               PERFORM 3590-ACCRUE-FEE
               PERFORM 3600-SELECT-PENALTY-PCT
               PERFORM 3610-SET-TRAN
               PERFORM 3620-SELECT-PENALTY-PCT
               PERFORM 3630-DESCRIBE-SOURCE
               PERFORM 3640-SELECT-RATE
               PERFORM 3650-SELECT-PENALTY-PCT
               PERFORM 3660-AGE-EFF
               PERFORM 3670-SET-PAYEE-TYPE
               PERFORM 3680-ASSESS-PREFERRED
               PERFORM 3690-SET-CHANNEL
               PERFORM 3700-COUNT-ITEM
               PERFORM 3710-PRORATE-PAYMENT
               PERFORM 3720-DESCRIBE-REMIT-METHOD
               PERFORM 3730-SPLIT-RETURNED
               PERFORM 3740-CHECK-OPEN
               PERFORM 3750-SELECT-MARGIN
               PERFORM 3760-LIMIT-RETURNED
               PERFORM 3770-LIMIT-RETURNED
               PERFORM 3780-LIMIT-REFUND
               PERFORM 3790-LIMIT-REFUND
               PERFORM 3800-SET-PAYEE-TYPE
               PERFORM 3810-AGE-POST
               PERFORM 3820-CHECK-LAST-STMT
               PERFORM 3830-SPLIT-PAYMENT
               PERFORM 3840-SPLIT-REFUND
               PERFORM 3850-SPLIT-PAYMENT
               PERFORM 3860-CHECK-POST
               PERFORM 3870-ACCUM-CHANNEL
               PERFORM 3880-DESCRIBE-PAYEE-TYPE
               PERFORM 3890-ASSESS-OVERRIDE
               PERFORM 3900-LIMIT-REFUND
               PERFORM 3910-AGE-EFF
               PERFORM 3920-LIMIT-PAYMENT
               PERFORM 3930-SET-CYCLE
               PERFORM 3940-SELECT-MARGIN
               PERFORM 3950-SPLIT-FEE
               PERFORM 3960-LIMIT-REFUND
               PERFORM 3970-SPLIT-PAYMENT
               PERFORM 3980-PRORATE-RETURNED
               PERFORM 3990-COUNT-CYCLE
               PERFORM 4000-COUNT-PAYMENT
               PERFORM 4010-SELECT-MARGIN
               PERFORM 4020-SPLIT-RETURNED
               PERFORM 4030-EDIT-CONTACT
               PERFORM 4040-ASSESS-AUTO-RENEW
               PERFORM 4050-PRORATE-RETURNED
               PERFORM 4060-COUNT-ITEM
               PERFORM 4070-PRORATE-ITEM
               PERFORM 4080-ASSESS-REVERSAL
               PERFORM 4090-SPLIT-FEE
               PERFORM 4100-EDIT-OPERATOR
               PERFORM 4110-SPLIT-FEE
               PERFORM 4120-COUNT-EXCEPT
               PERFORM 4130-EDIT-CUST-ID
               PERFORM 4140-COUNT-CYCLE
               PERFORM 4150-ACCUM-REMIT-METHOD
               PERFORM 4160-AGE-EFFECTIVE
               PERFORM 4170-COUNT-PAYMENT
               PERFORM 4180-AGE-LAST-STMT
               PERFORM 4190-SET-STATUS
               PERFORM 4200-ACCRUE-REFUND
               PERFORM 4210-PRORATE-RETURNED
               PERFORM 4220-ASSESS-EXEMPT
               PERFORM 4230-CHECK-POST
               PERFORM 4240-SELECT-RATE
               PERFORM 4250-COUNT-CYCLE
               PERFORM 4260-SET-CHANNEL
               PERFORM 4270-EDIT-OPERATOR
               PERFORM 4280-LIMIT-ITEM
               PERFORM 4290-COUNT-SEQ
               PERFORM 4300-SET-SOURCE
               PERFORM 4310-ACCRUE-PAYMENT
               PERFORM 4320-CHECK-NEXT-DUE
               PERFORM 4330-PRORATE-REFUND
               PERFORM 4340-CHECK-LAST-STMT
               PERFORM 4350-ACCUM-REASON
               PERFORM 4360-ACCUM-TRAN
               PERFORM 4370-AGE-POST
               PERFORM 4380-SET-STATUS
               PERFORM 4390-ASSESS-REVERSAL
               PERFORM 4400-SPLIT-RETURNED
               PERFORM 4410-AGE-OPEN
               PERFORM 4420-ACCRUE-FEE
               PERFORM 4430-SPLIT-REFUND
               PERFORM 4440-CHECK-NEXT-DUE
               PERFORM 4450-PRORATE-FEE
               PERFORM 4460-SELECT-MARGIN
               PERFORM 4470-ASSESS-HOLD
               PERFORM 4480-FLAG-REFUND
               PERFORM 4490-SPLIT-PAYMENT
               PERFORM 4500-CHECK-EFF
               PERFORM 4510-FLAG-PAYMENT
               PERFORM 4520-FLAG-FEE
               PERFORM 4530-ACCRUE-REFUND
               PERFORM 4540-SET-CHANNEL
               PERFORM 4550-COUNT-ITEM
               PERFORM 4560-LIMIT-ITEM
               PERFORM 4570-ACCUM-REMIT-METHOD
               PERFORM 4580-PRORATE-PAYMENT
               PERFORM 4590-LIMIT-RETURNED
               PERFORM 4600-LIMIT-PAYMENT
               PERFORM 4610-ACCRUE-PAYMENT
               PERFORM 4620-COUNT-EXCEPT
               PERFORM 4630-ACCUM-STATUS
               PERFORM 4640-FLAG-FEE
               PERFORM 4650-ASSESS-HOLD
               PERFORM 7020-CALL-BPSTAT
               PERFORM 7000-CALL-BPELG
               PERFORM 7010-CALL-BPPRC
               REWRITE BPY-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'BPYMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM BPY-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * FEE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3000-FLAG-FEE.
           IF BPYM-FEE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-PAYMENT.
           IF BPYM-PAYMENT-YTD < 0.00
               MOVE 0.00 TO BPYM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-PAYMENT-YTD > 10000.00
               MOVE 10000.00 TO BPYM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3020-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REFUND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       3030-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-PAYMENT-YTD ROUNDED =
                   BPYM-REFUND-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-PAYMENT-YTD
           END-IF
           ADD BPYM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN NOTICE DATE IS PAST
      * FP -> JH WHEN NOTICE DATE IS PAST
      * JH -> JV WHEN NOTICE DATE IS PAST
      * JV -> RM WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3040-SET-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FP' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FP'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JH' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JH'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JV' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JV'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3050-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-TRAN-CD
                   ADD BPYM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E691 MEMO REQUIRED
      * E388 REFUND AMT NEGATIVE
      * E219 REFUND AMT OVER 25000
      * E676 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3060-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-MEMO = SPACES
                   MOVE 'E691' TO WS-REJECT-CODE
               WHEN BPYT-REFUND-AMT < 0
                   MOVE 'E388' TO WS-REJECT-CODE
               WHEN BPYT-REFUND-AMT > 25000.00
                   MOVE 'E219' TO WS-REJECT-CODE
               WHEN BPYM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E676' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-CYCLE-CD
                   ADD BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3080-SET-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE RETURNED YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3090-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO BPYM-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE BPYM-REFUND-AMT ROUNDED = BPYM-RETURNED-YTD *
                       BPYM-RATE / 200
               WHEN BPYM-REASON-CD = 'AX'
                   MOVE 5.00 TO BPYM-REFUND-AMT
               WHEN BPYM-REASON-CD = 'BX'
                   MOVE 12.00 TO BPYM-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYM-REFUND-AMT ROUNDED = BPYM-RETURNED-YTD *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-REFUND-AMT > 0 AND BPYM-REFUND-AMT < 1.00
               MOVE 1.00 TO BPYM-REFUND-AMT
           END-IF
           IF BPYM-REFUND-AMT > 25.00
               MOVE 25.00 TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-REASON-CD
                   ADD BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED FEE ON THE REFUND AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3110-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BPYT-RETURNED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = BPYM-REFUND-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-REASON-CD = 'AX'
                   MOVE 3.00 TO BPYT-RETURNED-AMT
               WHEN BPYM-REASON-CD = 'BX'
                   MOVE 20.00 TO BPYT-RETURNED-AMT
               WHEN OTHER
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = BPYM-REFUND-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYT-RETURNED-AMT > 0 AND BPYT-RETURNED-AMT < 10.00
               MOVE 10.00 TO BPYT-RETURNED-AMT
           END-IF
           IF BPYT-RETURNED-AMT > 25.00
               MOVE 25.00 TO BPYT-RETURNED-AMT
           END-IF
           ADD BPYT-RETURNED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3120-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 10
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3130-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPYT-REFUND-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BPYT-REFUND-AMT - (WS-WORK-AMT * 2)
           COMPUTE BPYM-RETURNED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE REFUND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3140-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF BPYM-REFUND-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED FEE ON THE PAYMENT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3150-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO BPYT-RETURNED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = BPYT-PAYMENT-AMT
                       * BPYM-MARGIN / 200
               WHEN BPYM-REMIT-METHOD-CD = 'CGV'
                   MOVE 15.00 TO BPYT-RETURNED-AMT
               WHEN BPYM-REMIT-METHOD-CD = 'FNS'
                   MOVE 3.00 TO BPYT-RETURNED-AMT
               WHEN OTHER
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = BPYT-PAYMENT-AMT
                       * BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYT-RETURNED-AMT > 0 AND BPYT-RETURNED-AMT < 2.00
               MOVE 2.00 TO BPYT-RETURNED-AMT
           END-IF
           IF BPYT-RETURNED-AMT > 100.00
               MOVE 100.00 TO BPYT-RETURNED-AMT
           END-IF
           ADD BPYT-RETURNED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3160-COUNT-ITEM.
           IF BPYM-ITEM-CNT < 9999999
               ADD 1 TO BPYM-ITEM-CNT
           END-IF
           IF BPYM-ITEM-CNT NOT < 3
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-CYCLE-CD
                   ADD BPYT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3190-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETURNED AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3200-FLAG-RETURNED.
           IF BPYT-RETURNED-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYT-RETURNED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3210-DESCRIBE-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3220-DESCRIBE-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'US'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SC -> PD WHEN LAST STMT DATE IS PAST
      * PD -> RT WHEN LAST STMT DATE IS PAST
      * RT -> CN WHEN LAST STMT DATE IS PAST
      * CN -> RF WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RT' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RT'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO BPYM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO BPYM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND YTD FEE ON THE RETURNED YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3250-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BPYM-REFUND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE BPYM-REFUND-YTD ROUNDED = BPYM-RETURNED-YTD *
                       BPYM-MARGIN / 200
               WHEN BPYM-REMIT-METHOD-CD = 'CGV'
                   MOVE 3.00 TO BPYM-REFUND-YTD
               WHEN BPYM-REMIT-METHOD-CD = 'FNS'
                   MOVE 5.00 TO BPYM-REFUND-YTD
               WHEN OTHER
                   COMPUTE BPYM-REFUND-YTD ROUNDED = BPYM-RETURNED-YTD *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYM-REFUND-YTD > 0 AND BPYM-REFUND-YTD < 1.00
               MOVE 1.00 TO BPYM-REFUND-YTD
           END-IF
           IF BPYM-REFUND-YTD > 35.00
               MOVE 35.00 TO BPYM-REFUND-YTD
           END-IF
           ADD BPYM-REFUND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE FEE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3260-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BPYM-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE BPYM-REFUND-AMT ROUNDED = BPYM-FEE-YTD *
                       BPYM-RATE / 200
               WHEN BPYM-REMIT-METHOD-CD = 'CGV'
                   MOVE 5.00 TO BPYM-REFUND-AMT
               WHEN BPYM-REMIT-METHOD-CD = 'FNS'
                   MOVE 20.00 TO BPYM-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYM-REFUND-AMT ROUNDED = BPYM-FEE-YTD *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-REFUND-AMT > 0 AND BPYM-REFUND-AMT < 5.00
               MOVE 5.00 TO BPYM-REFUND-AMT
           END-IF
           IF BPYM-REFUND-AMT > 75.00
               MOVE 75.00 TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REMIT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3270-DESCRIBE-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FNS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NMS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TAF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VNK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WUA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XCT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-FEE.
           IF BPYM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-FEE-YTD * BPYM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BPYM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND YTD FEE ON THE FEE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3290-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPYM-REFUND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE BPYM-REFUND-YTD ROUNDED = BPYM-FEE-YTD *
                       BPYM-RATE / 200
               WHEN BPYM-STATUS-CD = 'JX'
                   MOVE 10.00 TO BPYM-REFUND-YTD
               WHEN BPYM-STATUS-CD = 'LR'
                   MOVE 12.00 TO BPYM-REFUND-YTD
               WHEN OTHER
                   COMPUTE BPYM-REFUND-YTD ROUNDED = BPYM-FEE-YTD *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-REFUND-YTD > 0 AND BPYM-REFUND-YTD < 10.00
               MOVE 10.00 TO BPYM-REFUND-YTD
           END-IF
           IF BPYM-REFUND-YTD > 100.00
               MOVE 100.00 TO BPYM-REFUND-YTD
           END-IF
           ADD BPYM-REFUND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN MATURITY DATE IS PAST
      * FP -> JH WHEN MATURITY DATE IS PAST
      * JH -> JV WHEN MATURITY DATE IS PAST
      * JV -> RM WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3300-SET-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FP' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FP'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JH' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JH'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JV' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JV'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REFUND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3310-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-FEE-YTD ROUNDED =
                   BPYT-REFUND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-FEE-YTD
           END-IF
           ADD BPYM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SC -> PD WHEN CLOSE DATE IS PAST
      * PD -> RT WHEN CLOSE DATE IS PAST
      * RT -> CN WHEN CLOSE DATE IS PAST
      * CN -> RF WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RT' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RT'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REFUND AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3330-FLAG-REFUND.
           IF BPYM-REFUND-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-REFUND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN OPEN DATE IS PAST
      * LR -> NZ WHEN OPEN DATE IS PAST
      * NZ -> RV WHEN OPEN DATE IS PAST
      * RV -> TM WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3340-SET-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LR' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LR'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPYM-PAYMENT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = BPYM-PAYMENT-YTD - (WS-WORK-AMT * 12)
           COMPUTE BPYT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE RETURNED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3360-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF BPYM-RETURNED-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3370-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPYM-PAYMENT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BPYM-PAYMENT-AMT - (WS-WORK-AMT * 4)
           COMPUTE BPYT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3380-COUNT-PAYMENT.
           IF BPYM-PAYMENT-CNT < 9999999
               ADD 1 TO BPYM-PAYMENT-CNT
           END-IF
           IF BPYM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E656 REJECT CD REQUIRED
      * E645 REFUND YTD NEGATIVE
      * E122 REFUND YTD OVER 10000
      * E170 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3390-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-REJECT-CD = SPACES
                   MOVE 'E656' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-YTD < 0
                   MOVE 'E645' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-YTD > 10000.00
                   MOVE 'E122' TO WS-REJECT-CODE
               WHEN BPYM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E170' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3400-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-CYCLE-CD
                   ADD BPYM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3410-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * BT -> FR WHEN EFF DATE IS PAST
      * FR -> LL WHEN EFF DATE IS PAST
      * LL -> US WHEN EFF DATE IS PAST
      * US -> XM WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FR' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FR'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LL' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LL'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3430-DESCRIBE-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JH'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3440-DESCRIBE-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'HD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'KD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE BPYM-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3460-COUNT-CYCLE.
           IF BPYM-CYCLE-CNT < 9999999
               ADD 1 TO BPYM-CYCLE-CNT
           END-IF
           IF BPYM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE RETURNED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3470-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO BPYM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF BPYM-RETURNED-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO BPYM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3480-FLAG-REFUND.
           IF BPYT-REFUND-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYT-REFUND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E311 REF NO REQUIRED
      * E431 WORK AMT NEGATIVE
      * E839 WORK AMT OVER 99999
      * E051 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3490-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-REF-NO = SPACES
                   MOVE 'E311' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E431' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 99999.00
                   MOVE 'E839' TO WS-REJECT-CODE
               WHEN BPYT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E051' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3500-LIMIT-FEE.
           IF BPYM-FEE-AMT < 50.00
               MOVE 50.00 TO BPYM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-FEE-AMT > 50000.00
               MOVE 50000.00 TO BPYM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3510-ACCRUE-FEE.
           IF BPYM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-FEE-AMT * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3520-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3530-FLAG-PAYMENT.
           IF BPYM-PAYMENT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE RETURNED AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3540-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO BPYM-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE BPYM-REFUND-AMT ROUNDED = BPYT-RETURNED-AMT *
                       BPYM-RATE / 200
               WHEN BPYM-SOURCE-CD = 'CT'
                   MOVE 12.00 TO BPYM-REFUND-AMT
               WHEN BPYM-SOURCE-CD = 'FV'
                   MOVE 7.00 TO BPYM-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYM-REFUND-AMT ROUNDED = BPYT-RETURNED-AMT *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-REFUND-AMT > 0 AND BPYM-REFUND-AMT < 5.00
               MOVE 5.00 TO BPYM-REFUND-AMT
           END-IF
           IF BPYM-REFUND-AMT > 25.00
               MOVE 25.00 TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE RETURNED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF BPYM-RETURNED-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-CYCLE-CD
                   ADD BPYM-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3570-DESCRIBE-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REFUND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3580-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-FEE-AMT ROUNDED =
                   BPYM-REFUND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYT-FEE-AMT
           END-IF
           ADD BPYT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3590-ACCRUE-FEE.
           IF BPYM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-FEE-YTD * BPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BPYM-REFUND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND SCHED TIER FOR THE RETURNED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3600-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-SCHED-RATE(1) TO BPYM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPYM-RETURNED-AMT NOT < WS-REFUND-SCHED-LOW(WS-TX)
                   MOVE WS-REFUND-SCHED-RATE(WS-TX) TO BPYM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SC -> PD WHEN EXPIRY DATE IS PAST
      * PD -> RT WHEN EXPIRY DATE IS PAST
      * RT -> CN WHEN EXPIRY DATE IS PAST
      * CN -> RF WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RT' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RT'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE RETURNED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO BPYM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF BPYM-RETURNED-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO BPYM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'HD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE REFUND SCHED TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3640-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-SCHED-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPYT-FEE-AMT NOT < WS-REFUND-SCHED-LOW(WS-TX)
                   MOVE WS-REFUND-SCHED-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND SCHED TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3650-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-SCHED-RATE(1) TO BPYM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPYM-FEE-YTD NOT < WS-REFUND-SCHED-LOW(WS-TX)
                   MOVE WS-REFUND-SCHED-RATE(WS-TX) TO BPYM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * BT -> FR WHEN NOTICE DATE IS PAST
      * FR -> LL WHEN NOTICE DATE IS PAST
      * LL -> US WHEN NOTICE DATE IS PAST
      * US -> XM WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3670-SET-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FR' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FR'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LL' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LL'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE PAYMENT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3680-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO BPYM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-HOLD-FLAG = 'Y'
                   COMPUTE BPYM-FEE-AMT ROUNDED = BPYM-PAYMENT-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO BPYM-FEE-AMT
               WHEN BPYT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO BPYM-FEE-AMT
               WHEN OTHER
                   COMPUTE BPYM-FEE-AMT ROUNDED = BPYM-PAYMENT-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYM-FEE-AMT > 0 AND BPYM-FEE-AMT < 10.00
               MOVE 10.00 TO BPYM-FEE-AMT
           END-IF
           IF BPYM-FEE-AMT > 25.00
               MOVE 25.00 TO BPYM-FEE-AMT
           END-IF
           ADD BPYM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-ITEM.
           IF BPYM-ITEM-CNT < 9999999
               ADD 1 TO BPYM-ITEM-CNT
           END-IF
           IF BPYM-ITEM-CNT NOT < 12
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       3710-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-PAYMENT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-PAYMENT-YTD
           END-IF
           ADD BPYM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REMIT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3720-DESCRIBE-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FNS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'NMS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TAF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VNK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WUA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XCT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RETURNED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3730-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPYM-RETURNED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = BPYM-RETURNED-YTD - (WS-WORK-AMT * 2)
           COMPUTE BPYM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3740-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF BPYM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3750-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3760-LIMIT-RETURNED.
           IF BPYM-RETURNED-YTD < 25.00
               MOVE 25.00 TO BPYM-RETURNED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-RETURNED-YTD > 1000.00
               MOVE 1000.00 TO BPYM-RETURNED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3770-LIMIT-RETURNED.
           IF BPYT-RETURNED-AMT < 100.00
               MOVE 100.00 TO BPYT-RETURNED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-RETURNED-AMT > 1000.00
               MOVE 1000.00 TO BPYT-RETURNED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-REFUND.
           IF BPYM-REFUND-AMT < 25.00
               MOVE 25.00 TO BPYM-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-REFUND-AMT > 1000.00
               MOVE 1000.00 TO BPYM-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3790-LIMIT-REFUND.
           IF BPYT-REFUND-AMT < 10.00
               MOVE 10.00 TO BPYT-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-REFUND-AMT > 5000.00
               MOVE 5000.00 TO BPYT-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * BT -> FR WHEN OPEN DATE IS PAST
      * FR -> LL WHEN OPEN DATE IS PAST
      * LL -> US WHEN OPEN DATE IS PAST
      * US -> XM WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3800-SET-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FR' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FR'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LL' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LL'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3810-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3820-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPYM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPYM-PAYMENT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BPYM-PAYMENT-YTD - (WS-WORK-AMT * 3)
           COMPUTE BPYM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3840-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPYT-REFUND-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPYT-REFUND-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3850-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPYM-PAYMENT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = BPYM-PAYMENT-YTD - (WS-WORK-AMT * 2)
           COMPUTE BPYT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3860-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF BPYT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3870-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-CHANNEL-CD
                   ADD BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'US'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE FEE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3890-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BPYM-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-HOLD-FLAG = 'Y'
                   COMPUTE BPYM-REFUND-AMT ROUNDED = BPYM-FEE-YTD *
                       BPYM-RATE / 200
               WHEN BPYM-PAYEE-TYPE-CD = 'BT'
                   MOVE 20.00 TO BPYM-REFUND-AMT
               WHEN BPYM-PAYEE-TYPE-CD = 'FR'
                   MOVE 20.00 TO BPYM-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYM-REFUND-AMT ROUNDED = BPYM-FEE-YTD *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-REFUND-AMT > 0 AND BPYM-REFUND-AMT < 5.00
               MOVE 5.00 TO BPYM-REFUND-AMT
           END-IF
           IF BPYM-REFUND-AMT > 150.00
               MOVE 150.00 TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3900-LIMIT-REFUND.
           IF BPYM-REFUND-AMT < 0.00
               MOVE 0.00 TO BPYM-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-REFUND-AMT > 50000.00
               MOVE 50000.00 TO BPYM-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3920-LIMIT-PAYMENT.
           IF BPYT-PAYMENT-AMT < 0.00
               MOVE 0.00 TO BPYT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-PAYMENT-AMT > 5000.00
               MOVE 5000.00 TO BPYT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN LAST STMT DATE IS PAST
      * FP -> JH WHEN LAST STMT DATE IS PAST
      * JH -> JV WHEN LAST STMT DATE IS PAST
      * JV -> RM WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3930-SET-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FP' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FP'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JH' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JH'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JV' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JV'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE REFUND SCHED TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-SCHED-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPYM-PAYMENT-YTD NOT < WS-REFUND-SCHED-LOW(WS-TX)
                   MOVE WS-REFUND-SCHED-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3950-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYT-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPYT-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE BPYM-RETURNED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3960-LIMIT-REFUND.
           IF BPYM-REFUND-AMT < 50.00
               MOVE 50.00 TO BPYM-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-REFUND-AMT > 1000.00
               MOVE 1000.00 TO BPYM-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3970-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPYT-PAYMENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPYT-PAYMENT-AMT - (WS-WORK-AMT * 3)
           COMPUTE BPYM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       3980-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-AMT ROUNDED =
                   BPYM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-RETURNED-AMT
           END-IF
           ADD BPYM-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3990-COUNT-CYCLE.
           IF BPYM-CYCLE-CNT < 9999999
               ADD 1 TO BPYM-CYCLE-CNT
           END-IF
           IF BPYM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4000-COUNT-PAYMENT.
           IF BPYM-PAYMENT-CNT < 9999999
               ADD 1 TO BPYM-PAYMENT-CNT
           END-IF
           IF BPYM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4010-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF BPYM-PAYMENT-AMT NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETURNED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4020-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPYM-RETURNED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BPYM-RETURNED-AMT - (WS-WORK-AMT * 4)
           COMPUTE BPYM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E026 CONTACT REQUIRED
      * E912 PAYMENT AMT NEGATIVE
      * E338 PAYMENT AMT OVER 10000
      * E205 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4030-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-CONTACT = SPACES
                   MOVE 'E026' TO WS-REJECT-CODE
               WHEN BPYM-PAYMENT-AMT < 0
                   MOVE 'E912' TO WS-REJECT-CODE
               WHEN BPYM-PAYMENT-AMT > 10000.00
                   MOVE 'E338' TO WS-REJECT-CODE
               WHEN BPYM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E205' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE RETURNED AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4040-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO BPYT-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   COMPUTE BPYT-REFUND-AMT ROUNDED = BPYT-RETURNED-AMT *
                       BPYM-RATE / 200
               WHEN BPYT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO BPYT-REFUND-AMT
               WHEN BPYT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO BPYT-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYT-REFUND-AMT ROUNDED = BPYT-RETURNED-AMT *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYT-REFUND-AMT > 0 AND BPYT-REFUND-AMT < 10.00
               MOVE 10.00 TO BPYT-REFUND-AMT
           END-IF
           IF BPYT-REFUND-AMT > 25.00
               MOVE 25.00 TO BPYT-REFUND-AMT
           END-IF
           ADD BPYT-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       4050-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-RETURNED-AMT ROUNDED =
                   BPYM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYT-RETURNED-AMT
           END-IF
           ADD BPYT-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4060-COUNT-ITEM.
           IF BPYM-ITEM-CNT < 9999999
               ADD 1 TO BPYM-ITEM-CNT
           END-IF
           IF BPYM-ITEM-CNT NOT < 6
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4070-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED FEE ON THE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4080-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BPYM-RETURNED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   COMPUTE BPYM-RETURNED-AMT ROUNDED = BPYM-FEE-AMT *
                       BPYM-MARGIN / 200
               WHEN BPYM-PAYEE-TYPE-CD = 'BT'
                   MOVE 7.00 TO BPYM-RETURNED-AMT
               WHEN BPYM-PAYEE-TYPE-CD = 'FR'
                   MOVE 15.00 TO BPYM-RETURNED-AMT
               WHEN OTHER
                   COMPUTE BPYM-RETURNED-AMT ROUNDED = BPYM-FEE-AMT *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYM-RETURNED-AMT > 0 AND BPYM-RETURNED-AMT < 2.00
               MOVE 2.00 TO BPYM-RETURNED-AMT
           END-IF
           IF BPYM-RETURNED-AMT > 25.00
               MOVE 25.00 TO BPYM-RETURNED-AMT
           END-IF
           ADD BPYM-RETURNED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4090-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYT-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPYT-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E587 OPERATOR REQUIRED
      * E794 RETURNED YTD NEGATIVE
      * E869 RETURNED YTD OVER 1000000
      * E591 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4100-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-OPERATOR = SPACES
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-YTD < 0
                   MOVE 'E794' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-YTD > 1000000.00
                   MOVE 'E869' TO WS-REJECT-CODE
               WHEN BPYM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E591' TO WS-REJECT-CODE
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
       4110-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE BPYM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4120-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E200 CUST ID REQUIRED
      * E977 REFUND AMT NEGATIVE
      * E365 REFUND AMT OVER 250000
      * E283 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4130-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-CUST-ID = SPACES
                   MOVE 'E200' TO WS-REJECT-CODE
               WHEN BPYT-REFUND-AMT < 0
                   MOVE 'E977' TO WS-REJECT-CODE
               WHEN BPYT-REFUND-AMT > 250000.00
                   MOVE 'E365' TO WS-REJECT-CODE
               WHEN BPYM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E283' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4140-COUNT-CYCLE.
           IF BPYM-CYCLE-CNT < 9999999
               ADD 1 TO BPYM-CYCLE-CNT
           END-IF
           IF BPYM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY REMIT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4150-ACCUM-REMIT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-REMIT-METHOD-CD
                   ADD BPYT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-REMIT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4160-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-PAYMENT.
           IF BPYM-PAYMENT-CNT < 9999999
               ADD 1 TO BPYM-PAYMENT-CNT
           END-IF
           IF BPYM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4180-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN MATURITY DATE IS PAST
      * LR -> NZ WHEN MATURITY DATE IS PAST
      * NZ -> RV WHEN MATURITY DATE IS PAST
      * RV -> TM WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4190-SET-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LR' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LR'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4200-ACCRUE-REFUND.
           IF BPYT-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-REFUND-AMT * BPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BPYT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETURNED YTD.
      *----------------------------------------------------------------*
       4210-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-YTD ROUNDED =
                   BPYM-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-RETURNED-YTD
           END-IF
           ADD BPYM-RETURNED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4220-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BPYM-RETURNED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = WS-WORK-AMT-2 *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-PAYEE-TYPE-CD = 'BT'
                   MOVE 10.00 TO BPYM-RETURNED-YTD
               WHEN BPYM-PAYEE-TYPE-CD = 'FR'
                   MOVE 12.00 TO BPYM-RETURNED-YTD
               WHEN OTHER
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = WS-WORK-AMT-2 *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYM-RETURNED-YTD > 0 AND BPYM-RETURNED-YTD < 5.00
               MOVE 5.00 TO BPYM-RETURNED-YTD
           END-IF
           IF BPYM-RETURNED-YTD > 250.00
               MOVE 250.00 TO BPYM-RETURNED-YTD
           END-IF
           ADD BPYM-RETURNED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4230-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF BPYT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND SCHED TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4240-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-SCHED-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPYM-PAYMENT-AMT NOT < WS-REFUND-SCHED-LOW(WS-TX)
                   MOVE WS-REFUND-SCHED-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4250-COUNT-CYCLE.
           IF BPYM-CYCLE-CNT < 9999999
               ADD 1 TO BPYM-CYCLE-CNT
           END-IF
           IF BPYM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST STMT DATE IS PAST
      * BAT -> MAL WHEN LAST STMT DATE IS PAST
      * MAL -> TEL WHEN LAST STMT DATE IS PAST
      * TEL -> ONL WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4260-SET-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E073 OPERATOR REQUIRED
      * E083 WORK AMT NEGATIVE
      * E842 WORK AMT OVER 10000
      * E353 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4270-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-OPERATOR = SPACES
                   MOVE 'E073' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E083' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E842' TO WS-REJECT-CODE
               WHEN BPYT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E353' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4280-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4290-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 5
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CT -> FV WHEN NEXT DUE DATE IS PAST
      * FV -> HD WHEN NEXT DUE DATE IS PAST
      * HD -> KD WHEN NEXT DUE DATE IS PAST
      * KD -> NE WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4300-SET-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FV' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FV'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HD'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4310-ACCRUE-PAYMENT.
           IF BPYT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-PAYMENT-AMT * BPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BPYM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4320-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO BPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETURNED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND YTD.
      *----------------------------------------------------------------*
       4330-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-REFUND-YTD ROUNDED =
                   BPYT-RETURNED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-REFUND-YTD
           END-IF
           ADD BPYM-REFUND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4340-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPYM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4350-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-REASON-CD
                   ADD BPYM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4360-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-TRAN-CD
                   ADD BPYM-REFUND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-REFUND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4370-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN NEXT DUE DATE IS PAST
      * LR -> NZ WHEN NEXT DUE DATE IS PAST
      * NZ -> RV WHEN NEXT DUE DATE IS PAST
      * RV -> TM WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4380-SET-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LR' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LR'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE RETURNED AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4390-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BPYT-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-HOLD-FLAG = 'Y'
                   COMPUTE BPYT-REFUND-AMT ROUNDED = BPYM-RETURNED-AMT *
                       BPYM-MARGIN / 200
               WHEN BPYM-SOURCE-CD = 'CT'
                   MOVE 12.00 TO BPYT-REFUND-AMT
               WHEN BPYM-SOURCE-CD = 'FV'
                   MOVE 12.00 TO BPYT-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYT-REFUND-AMT ROUNDED = BPYM-RETURNED-AMT *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYT-REFUND-AMT > 0 AND BPYT-REFUND-AMT < 1.00
               MOVE 1.00 TO BPYT-REFUND-AMT
           END-IF
           IF BPYT-REFUND-AMT > 50.00
               MOVE 50.00 TO BPYT-REFUND-AMT
           END-IF
           ADD BPYT-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT RETURNED AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4400-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPYT-RETURNED-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BPYT-RETURNED-AMT - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4410-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4420-ACCRUE-FEE.
           IF BPYM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-FEE-AMT * BPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BPYM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4430-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE BPYM-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4440-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       4450-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-FEE-AMT ROUNDED =
                   BPYM-PAYMENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYT-FEE-AMT
           END-IF
           ADD BPYT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE REFUND SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4460-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-SCHED-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-REFUND-SCHED-LOW(WS-TX)
                   MOVE WS-REFUND-SCHED-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE PAYMENT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4470-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPYM-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   COMPUTE BPYM-REFUND-AMT ROUNDED = BPYM-PAYMENT-AMT *
                       BPYM-MARGIN / 200
               WHEN BPYM-REASON-CD = 'AX'
                   MOVE 3.00 TO BPYM-REFUND-AMT
               WHEN BPYM-REASON-CD = 'BX'
                   MOVE 3.00 TO BPYM-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYM-REFUND-AMT ROUNDED = BPYM-PAYMENT-AMT *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYM-REFUND-AMT > 0 AND BPYM-REFUND-AMT < 2.00
               MOVE 2.00 TO BPYM-REFUND-AMT
           END-IF
           IF BPYM-REFUND-AMT > 150.00
               MOVE 150.00 TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * REFUND YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4480-FLAG-REFUND.
           IF BPYM-REFUND-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-REFUND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4490-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPYM-PAYMENT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = BPYM-PAYMENT-YTD - (WS-WORK-AMT * 12)
           COMPUTE BPYM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4500-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF BPYT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4510-FLAG-PAYMENT.
           IF BPYT-PAYMENT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4520-FLAG-FEE.
           IF BPYT-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4530-ACCRUE-REFUND.
           IF BPYM-REFUND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-REFUND-YTD * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYM-RETURNED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST ACT DATE IS PAST
      * BAT -> MAL WHEN LAST ACT DATE IS PAST
      * MAL -> TEL WHEN LAST ACT DATE IS PAST
      * TEL -> ONL WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4540-SET-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF BPYM-LAST-ACT-DATE > 0 AND BPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BPYM-LAST-ACT-DATE > 0 AND BPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BPYM-LAST-ACT-DATE > 0 AND BPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BPYM-LAST-ACT-DATE > 0 AND BPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4550-COUNT-ITEM.
           IF BPYM-ITEM-CNT < 9999999
               ADD 1 TO BPYM-ITEM-CNT
           END-IF
           IF BPYM-ITEM-CNT NOT < 3
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4560-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REMIT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4570-ACCUM-REMIT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-REMIT-METHOD-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-REMIT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       4580-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-PAYMENT-AMT ROUNDED =
                   BPYT-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-PAYMENT-AMT
           END-IF
           ADD BPYM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETURNED AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4590-LIMIT-RETURNED.
           IF BPYT-RETURNED-AMT < 0.00
               MOVE 0.00 TO BPYT-RETURNED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-RETURNED-AMT > 2500.00
               MOVE 2500.00 TO BPYT-RETURNED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4600-LIMIT-PAYMENT.
           IF BPYM-PAYMENT-YTD < 10.00
               MOVE 10.00 TO BPYM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-PAYMENT-YTD > 10000.00
               MOVE 10000.00 TO BPYM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4610-ACCRUE-PAYMENT.
           IF BPYT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-PAYMENT-AMT * BPYM-RATE / 100 / 365
               ADD WS-WORK-AMT TO BPYM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4620-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4630-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-STATUS-CD
                   ADD BPYM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4640-FLAG-FEE.
           IF BPYM-FEE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE FEE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4650-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPYM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BPYM-PAYMENT-AMT ROUNDED = BPYM-FEE-AMT *
                       BPYM-RATE / 200
               WHEN BPYM-CYCLE-CD = 'DA'
                   MOVE 20.00 TO BPYM-PAYMENT-AMT
               WHEN BPYM-CYCLE-CD = 'FP'
                   MOVE 7.00 TO BPYM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE BPYM-PAYMENT-AMT ROUNDED = BPYM-FEE-AMT *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-PAYMENT-AMT > 0 AND BPYM-PAYMENT-AMT < 10.00
               MOVE 10.00 TO BPYM-PAYMENT-AMT
           END-IF
           IF BPYM-PAYMENT-AMT > 50.00
               MOVE 50.00 TO BPYM-PAYMENT-AMT
           END-IF
           ADD BPYM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PASS THE BPY FIELDS TO BPSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-BPSTAT.
           INITIALIZE BPSTAT-PARMS
           MOVE BPYM-PAYMENT-AMT
             TO BPSTAT-PAYMENT-AMT
           MOVE BPYM-RETURNED-AMT
             TO BPSTAT-RETURNED-AMT
           MOVE BPYM-REFUND-AMT
             TO BPSTAT-REFUND-AMT
           MOVE BPYM-PAYMENT-YTD
             TO BPSTAT-PAYMENT-YTD
           MOVE BPYM-RETURNED-YTD
             TO BPSTAT-RETURNED-YTD
           MOVE BPYM-FEE-AMT
             TO BPSTAT-FEE-AMT
           MOVE BPYM-OPEN-DATE
             TO BPSTAT-OPEN-DATE
           MOVE BPYM-MATURITY-DATE
             TO BPSTAT-MATURITY-DATE
           MOVE BPYM-LAST-STMT-DATE
             TO BPSTAT-LAST-STMT-DATE
           MOVE BPYM-CLOSE-DATE
             TO BPSTAT-CLOSE-DATE
           MOVE BPYM-CYCLE-CD
             TO BPSTAT-CYCLE-CD
           MOVE BPYM-REMIT-METHOD-CD
             TO BPSTAT-REMIT-METHOD-CD
           MOVE BPYM-STATUS-CD
             TO BPSTAT-STATUS-CD
           MOVE BPYM-REASON-CD
             TO BPSTAT-REASON-CD
           MOVE BPYM-AUTO-RENEW-FLAG
             TO BPSTAT-AUTO-RENEW-FLAG
           MOVE BPYM-EXEMPT-FLAG
             TO BPSTAT-EXEMPT-FLAG
           MOVE BPYM-SUSPEND-FLAG
             TO BPSTAT-SUSPEND-FLAG
           MOVE BPYM-PAYMENT-CNT
             TO BPSTAT-PAYMENT-CNT
           MOVE BPYM-EXCEPT-CNT
             TO BPSTAT-EXCEPT-CNT
           MOVE BPYM-PENALTY-PCT
             TO BPSTAT-PENALTY-PCT
           MOVE BPYM-RATE
             TO BPSTAT-RATE
           MOVE BPYM-REF-NO
             TO BPSTAT-REF-NO
           MOVE BPYM-CUST-ID
             TO BPSTAT-CUST-ID
           CALL 'BPSTAT' USING BPSTAT-PARMS
           EVALUATE BPSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BPSTAT-PAYMENT-YTD
                     TO BPYM-PAYMENT-YTD
                   MOVE BPSTAT-AUTO-RENEW-FLAG
                     TO BPYM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-BPSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE BPY FIELDS TO BPELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-BPELG.
           INITIALIZE BPELG-PARMS
           MOVE BPYM-PAYMENT-YTD
             TO BPELG-PAYMENT-YTD
           MOVE BPYM-FEE-YTD
             TO BPELG-FEE-YTD
           MOVE BPYM-REFUND-YTD
             TO BPELG-REFUND-YTD
           MOVE BPYM-FEE-AMT
             TO BPELG-FEE-AMT
           MOVE BPYM-PAYMENT-AMT
             TO BPELG-PAYMENT-AMT
           MOVE BPYM-REFUND-AMT
             TO BPELG-REFUND-AMT
           MOVE BPYM-EXPIRY-DATE
             TO BPELG-EXPIRY-DATE
           MOVE BPYM-LAST-STMT-DATE
             TO BPELG-LAST-STMT-DATE
           MOVE BPYM-NEXT-DUE-DATE
             TO BPELG-NEXT-DUE-DATE
           MOVE BPYM-CLOSE-DATE
             TO BPELG-CLOSE-DATE
           MOVE BPYM-CYCLE-CD
             TO BPELG-CYCLE-CD
           MOVE BPYM-STATUS-CD
             TO BPELG-STATUS-CD
           MOVE BPYM-REMIT-METHOD-CD
             TO BPELG-REMIT-METHOD-CD
           MOVE BPYM-PAYEE-TYPE-CD
             TO BPELG-PAYEE-TYPE-CD
           MOVE BPYM-OVERRIDE-FLAG
             TO BPELG-OVERRIDE-FLAG
           MOVE BPYM-HOLD-FLAG
             TO BPELG-HOLD-FLAG
           MOVE BPYM-SUSPEND-FLAG
             TO BPELG-SUSPEND-FLAG
           MOVE BPYM-EXCEPT-CNT
             TO BPELG-EXCEPT-CNT
           MOVE BPYM-CYCLE-CNT
             TO BPELG-CYCLE-CNT
           MOVE BPYM-RATE
             TO BPELG-RATE
           MOVE BPYM-MARGIN
             TO BPELG-MARGIN
           MOVE BPYM-ADDR
             TO BPELG-ADDR
           MOVE BPYM-REF-NO
             TO BPELG-REF-NO
           CALL 'BPELG' USING BPELG-PARMS
           EVALUATE BPELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BPELG-PAYMENT-YTD
                     TO BPYM-PAYMENT-YTD
                   MOVE BPELG-FEE-YTD
                     TO BPYM-FEE-YTD
                   MOVE BPELG-FEE-AMT
                     TO BPYM-FEE-AMT
                   MOVE BPELG-OVERRIDE-FLAG
                     TO BPYM-OVERRIDE-FLAG
                   MOVE BPELG-HOLD-FLAG
                     TO BPYM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-BPELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE BPY FIELDS TO BPPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-BPPRC.
           INITIALIZE BPPRC-PARMS
           MOVE BPYM-FEE-AMT
             TO BPPRC-FEE-AMT
           MOVE BPYM-PAYMENT-AMT
             TO BPPRC-PAYMENT-AMT
           MOVE BPYM-PAYMENT-YTD
             TO BPPRC-PAYMENT-YTD
           MOVE BPYM-REFUND-YTD
             TO BPPRC-REFUND-YTD
           MOVE BPYM-RETURNED-YTD
             TO BPPRC-RETURNED-YTD
           MOVE BPYM-FEE-YTD
             TO BPPRC-FEE-YTD
           MOVE BPYM-EXPIRY-DATE
             TO BPPRC-EXPIRY-DATE
           MOVE BPYM-LAST-ACT-DATE
             TO BPPRC-LAST-ACT-DATE
           MOVE BPYM-EFFECTIVE-DATE
             TO BPPRC-EFFECTIVE-DATE
           MOVE BPYM-NEXT-DUE-DATE
             TO BPPRC-NEXT-DUE-DATE
           MOVE BPYM-SOURCE-CD
             TO BPPRC-SOURCE-CD
           MOVE BPYM-REMIT-METHOD-CD
             TO BPPRC-REMIT-METHOD-CD
           MOVE BPYM-REASON-CD
             TO BPPRC-REASON-CD
           MOVE BPYM-STATUS-CD
             TO BPPRC-STATUS-CD
           MOVE BPYM-SUSPEND-FLAG
             TO BPPRC-SUSPEND-FLAG
           MOVE BPYM-OVERRIDE-FLAG
             TO BPPRC-OVERRIDE-FLAG
           MOVE BPYM-HOLD-FLAG
             TO BPPRC-HOLD-FLAG
           MOVE BPYM-EXCEPT-CNT
             TO BPPRC-EXCEPT-CNT
           MOVE BPYM-CYCLE-CNT
             TO BPPRC-CYCLE-CNT
           MOVE BPYM-MARGIN
             TO BPPRC-MARGIN
           MOVE BPYM-PENALTY-PCT
             TO BPPRC-PENALTY-PCT
           MOVE BPYM-ADDR
             TO BPPRC-ADDR
           MOVE BPYM-NAME
             TO BPPRC-NAME
           CALL 'BPPRC' USING BPPRC-PARMS
           EVALUATE BPPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BPPRC-PAYMENT-AMT
                     TO BPYM-PAYMENT-AMT
                   MOVE BPPRC-PAYMENT-YTD
                     TO BPYM-PAYMENT-YTD
                   MOVE BPPRC-FEE-YTD
                     TO BPYM-FEE-YTD
                   MOVE BPPRC-OVERRIDE-FLAG
                     TO BPYM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-BPPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE BPYT-KEY TO RD-KEY
           MOVE BPYT-TRAN-CD TO RD-CODE
           MOVE BPYT-PAYMENT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE BPYT-KEY TO RD-KEY
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
           DISPLAY 'BPYCLSE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'BPYCLSE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
