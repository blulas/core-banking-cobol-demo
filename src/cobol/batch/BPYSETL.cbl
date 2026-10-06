      *================================================================*
      * PROGRAM  : BPYSETL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : BILL PAYMENT SERVICE - SETTLEMENT. READS THE BPY    *
      *            TRANSACTIONS, UPDATES THE BPY MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CONSUMER BILL PAYMENT PAYEE     *
      *            MAINTENANCE AND CHECK OR EFT DISBURSEMENT           *
      * INPUT    : BPYMST (KSDS), BPYTRN, BANKCTL                      *
      * OUTPUT   : BPYOUT, BPYSERPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, BPSTAT, BPPRC, BPELG    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-04-04 EWS  ORIGINAL                                       *
      * 1994-05-23 EWS  FEE ROUNDING - HALF UP CHG6503                 *
      * 1994-10-02 SKN  RETURNED CUTOFF MOVED CHG6683                  *
      * 1996-10-08 CRP  NEW RETURNED REPORT COLUMNS REQ5148            *
      * 1998-07-05 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-01-11 PQR  FEE NOTICE TEXT PER LEGAL REQ0438              *
      * 1999-01-03 TWH  PAYMENT NOTICE TEXT PER LEGAL INC7886          *
      * 2001-12-28 TWH  REFUND STATUS CODES ADDED INC8872              *
      * 2001-09-10 BHS  RETURNED CALCULATION CORRECTED PRB9567         *
      * 2003-02-05 SKN  REFUND FEE SCHEDULE UPDATE REQ5939             *
      * 2004-04-16 PQR  REFUND RERUN PROTECTION REQ2281                *
      * 2005-02-05 PQR  PAYMENT TABLE EXPANDED REQ5096                 *
      * 2008-11-03 SKN  PERFORMANCE - FEE READ AHEAD REQ6875           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BPYSETL.
       AUTHOR. B H SANDERS.

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
           SELECT REPORT-FILE ASSIGN TO "BPYSERPT"
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
                                           VALUE 24.000000.
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
      * WS-RETURNED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RETURNED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000239000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000778000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000828500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000528500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000563500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003650000000695000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004150000000671000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005150000000520000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000040000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006250000000693000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006350000000771000'.
       01  WS-RETURNED-GRID-TBL REDEFINES WS-RETURNED-GRID-VALUES.
           05  WS-RETURNED-GRID-ROW OCCURS 11 TIMES.
               10  WS-RETURNED-GRID-LOW    PIC 9(09)V99.
               10  WS-RETURNED-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-REFUND-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REFUND-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000686500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000316000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000859000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004000000000552500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004050000000146000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009050000000129000'.
       01  WS-REFUND-TIER-TBL REDEFINES WS-REFUND-TIER-VALUES.
           05  WS-REFUND-TIER-ROW OCCURS 6 TIMES.
               10  WS-REFUND-TIER-LOW      PIC 9(09)V99.
               10  WS-REFUND-TIER-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-REFUND-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REFUND-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000183500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000785500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000632000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000528500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002250000000586000'.
       01  WS-REFUND-GRID-TBL REDEFINES WS-REFUND-GRID-VALUES.
           05  WS-REFUND-GRID-ROW OCCURS 5 TIMES.
               10  WS-REFUND-GRID-LOW      PIC 9(09)V99.
               10  WS-REFUND-GRID-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  BPYSETL  BILL PAYMENT SERVICE'.
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
       COPY BPPRCP.
       COPY BPELGP.

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
               PERFORM 3000-AGE-NEXT-DUE
               PERFORM 3010-ACCRUE-REFUND
               PERFORM 3020-ASSESS-HOLD
               PERFORM 3030-COUNT-EXCEPT
               PERFORM 3040-ACCUM-TRAN
               PERFORM 3050-SET-CYCLE
               PERFORM 3060-ASSESS-EXEMPT
               PERFORM 3070-SET-REASON
               PERFORM 3080-EDIT-REJECT
               PERFORM 3090-PRORATE-PAYMENT
               PERFORM 3100-SET-PAYEE-TYPE
               PERFORM 3110-SPLIT-REFUND
               PERFORM 3120-ACCUM-PAYEE-TYPE
               PERFORM 3130-SELECT-RATE
               PERFORM 3140-PRORATE-RETURNED
               PERFORM 3150-AGE-NEXT-DUE
               PERFORM 3160-LIMIT-FEE
               PERFORM 3170-COUNT-EXCEPT
               PERFORM 3180-SPLIT-PAYMENT
               PERFORM 3190-PRORATE-RETURNED
               PERFORM 3200-DESCRIBE-STATUS
               PERFORM 3210-FLAG-PAYMENT
               PERFORM 3220-CHECK-EXPIRY
               PERFORM 3230-ASSESS-AUTO-RENEW
               PERFORM 3240-SPLIT-FEE
               PERFORM 3250-EDIT-DESC
               PERFORM 3260-ACCRUE-PENALTY-PCT
               PERFORM 3270-SELECT-RATE
               PERFORM 3280-ACCRUE-RETURNED
               PERFORM 3290-SPLIT-RETURNED
               PERFORM 3300-COUNT-SEQ
               PERFORM 3310-FLAG-PAYMENT
               PERFORM 3320-AGE-LAST-STMT
               PERFORM 3330-AGE-NOTICE
               PERFORM 3340-ACCRUE-PAYMENT
               PERFORM 3350-CHECK-LAST-STMT
               PERFORM 3360-FLAG-PAYMENT
               PERFORM 3370-AGE-EFF
               PERFORM 3380-PRORATE-PAYMENT
               PERFORM 3390-COUNT-CYCLE
               PERFORM 3400-COUNT-ITEM
               PERFORM 3410-DESCRIBE-STATUS
               PERFORM 3420-COUNT-EXCEPT
               PERFORM 3430-COUNT-EXCEPT
               PERFORM 3440-DESCRIBE-STATUS
               PERFORM 3450-COUNT-CYCLE
               PERFORM 3460-DESCRIBE-REMIT-METHOD
               PERFORM 3470-COUNT-PAYMENT
               PERFORM 3480-CHECK-NOTICE
               PERFORM 3490-AGE-LAST-ACT
               PERFORM 3500-ACCRUE-PAYMENT
               PERFORM 3510-CHECK-CLOSE
               PERFORM 3520-AGE-LAST-STMT
               PERFORM 3530-ACCRUE-REFUND
               PERFORM 3540-SPLIT-REFUND
               PERFORM 3550-AGE-NEXT-DUE
               PERFORM 3560-SET-TRAN
               PERFORM 3570-EDIT-REF-NO
               PERFORM 3580-SPLIT-PAYMENT
               PERFORM 3590-ACCUM-TRAN
               PERFORM 3600-COUNT-PAYMENT
               PERFORM 3610-SPLIT-RETURNED
               PERFORM 3620-FLAG-PAYMENT
               PERFORM 3630-ACCUM-REASON
               PERFORM 3640-ASSESS-EXEMPT
               PERFORM 3650-SPLIT-PAYMENT
               PERFORM 3660-PRORATE-REFUND
               PERFORM 3670-AGE-EFFECTIVE
               PERFORM 3680-FLAG-FEE
               PERFORM 3690-AGE-LAST-ACT
               PERFORM 3700-AGE-NEXT-DUE
               PERFORM 3710-ASSESS-AUTO-RENEW
               PERFORM 3720-ASSESS-EXEMPT
               PERFORM 3730-LIMIT-REFUND
               PERFORM 3740-LIMIT-PAYMENT
               PERFORM 3750-EDIT-CONTACT
               PERFORM 3760-PRORATE-REFUND
               PERFORM 3770-LIMIT-FEE
               PERFORM 3780-LIMIT-REFUND
               PERFORM 3790-SET-CYCLE
               PERFORM 3800-EDIT-MEMO
               PERFORM 3810-AGE-EFFECTIVE
               PERFORM 3820-ASSESS-HOLD
               PERFORM 3830-SPLIT-REFUND
               PERFORM 3840-COUNT-EXCEPT
               PERFORM 3850-CHECK-NEXT-DUE
               PERFORM 3860-CHECK-LAST-STMT
               PERFORM 3870-SELECT-RATE
               PERFORM 3880-PRORATE-FEE
               PERFORM 3890-DESCRIBE-REASON
               PERFORM 3900-SPLIT-REFUND
               PERFORM 3910-COUNT-PAYMENT
               PERFORM 3920-PRORATE-RETURNED
               PERFORM 3930-ACCRUE-RETURNED
               PERFORM 3940-DESCRIBE-REMIT-METHOD
               PERFORM 3950-SET-REMIT-METHOD
               PERFORM 3960-AGE-LAST-STMT
               PERFORM 3970-AGE-CLOSE
               PERFORM 3980-FLAG-RETURNED
               PERFORM 3990-PRORATE-RETURNED
               PERFORM 4000-SET-STATUS
               PERFORM 4010-LIMIT-FEE
               PERFORM 4020-LIMIT-PAYMENT
               PERFORM 4030-CHECK-CLOSE
               PERFORM 4040-CHECK-NEXT-DUE
               PERFORM 4050-SPLIT-FEE
               PERFORM 4060-SELECT-RATE
               PERFORM 4070-EDIT-OPERATOR
               PERFORM 4080-SPLIT-RETURNED
               PERFORM 4090-ACCRUE-RETURNED
               PERFORM 4100-PRORATE-FEE
               PERFORM 4110-DESCRIBE-SOURCE
               PERFORM 4120-FLAG-FEE
               PERFORM 4130-FLAG-FEE
               PERFORM 4140-PRORATE-RETURNED
               PERFORM 4150-SET-PAYEE-TYPE
               PERFORM 4160-FLAG-ITEM
               PERFORM 4170-ASSESS-REVERSAL
               PERFORM 4180-PRORATE-REFUND
               PERFORM 4190-FLAG-RETURNED
               PERFORM 4200-SET-CHANNEL
               PERFORM 4210-SET-PAYEE-TYPE
               PERFORM 4220-ASSESS-HOLD
               PERFORM 4230-SELECT-RATE
               PERFORM 4240-ASSESS-SUSPEND
               PERFORM 4250-LIMIT-PAYMENT
               PERFORM 4260-DESCRIBE-REASON
               PERFORM 4270-SPLIT-RETURNED
               PERFORM 4280-EDIT-CONTACT
               PERFORM 4290-CHECK-OPEN
               PERFORM 4300-ACCRUE-REFUND
               PERFORM 4310-FLAG-FEE
               PERFORM 4320-SET-REMIT-METHOD
               PERFORM 4330-AGE-CLOSE
               PERFORM 4340-COUNT-EXCEPT
               PERFORM 4350-SET-PAYEE-TYPE
               PERFORM 4360-ASSESS-SUSPEND
               PERFORM 4370-LIMIT-PAYMENT
               PERFORM 4380-EDIT-REJECT
               PERFORM 4390-SELECT-MARGIN
               PERFORM 4400-DESCRIBE-TRAN
               PERFORM 4410-PRORATE-REFUND
               PERFORM 4420-ASSESS-PREFERRED
               PERFORM 4430-DESCRIBE-REASON
               PERFORM 4440-COUNT-PAYMENT
               PERFORM 4450-EDIT-MEMO
               PERFORM 4460-SET-SOURCE
               PERFORM 4470-CHECK-OPEN
               PERFORM 4480-CHECK-CLOSE
               PERFORM 4490-LIMIT-ITEM
               PERFORM 4500-SET-PAYEE-TYPE
               PERFORM 4510-PRORATE-REFUND
               PERFORM 7020-CALL-BPSTAT
               PERFORM 7010-CALL-BPPRC
               PERFORM 7000-CALL-BPELG
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
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3000-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-REFUND.
           IF BPYT-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-REFUND-AMT * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYM-RETURNED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3020-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPYM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE BPYM-PAYMENT-YTD ROUNDED = WS-WORK-AMT-2 *
                       BPYM-MARGIN / 200
               WHEN BPYM-REASON-CD = 'AX'
                   MOVE 12.00 TO BPYM-PAYMENT-YTD
               WHEN BPYM-REASON-CD = 'BX'
                   MOVE 20.00 TO BPYM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE BPYM-PAYMENT-YTD ROUNDED = WS-WORK-AMT-2 *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYM-PAYMENT-YTD > 0 AND BPYM-PAYMENT-YTD < 2.00
               MOVE 2.00 TO BPYM-PAYMENT-YTD
           END-IF
           IF BPYM-PAYMENT-YTD > 150.00
               MOVE 150.00 TO BPYM-PAYMENT-YTD
           END-IF
           ADD BPYM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3040-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-TRAN-CD
                   ADD BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN EFF DATE IS PAST
      * FP -> JH WHEN EFF DATE IS PAST
      * JH -> JV WHEN EFF DATE IS PAST
      * JV -> RM WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FP' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FP'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JH' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JH'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JV' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JV'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PAYMENT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3060-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BPYM-PAYMENT-YTD *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-REMIT-METHOD-CD = 'CGV'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN BPYM-REMIT-METHOD-CD = 'FNS'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BPYM-PAYMENT-YTD *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> BX WHEN EFFECTIVE DATE IS PAST
      * BX -> GM WHEN EFFECTIVE DATE IS PAST
      * GM -> KU WHEN EFFECTIVE DATE IS PAST
      * KU -> LT WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BX' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BX'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KU' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KU'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E178 REJECT CD REQUIRED
      * E949 PAYMENT AMT NEGATIVE
      * E473 PAYMENT AMT OVER 10000
      * E780 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-REJECT-CD = SPACES
                   MOVE 'E178' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT < 0
                   MOVE 'E949' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT > 10000.00
                   MOVE 'E473' TO WS-REJECT-CODE
               WHEN BPYM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E780' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3090-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-PAYMENT-AMT ROUNDED =
                   BPYM-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-PAYMENT-AMT
           END-IF
           ADD BPYM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * BT -> FR WHEN OPEN DATE IS PAST
      * FR -> LL WHEN OPEN DATE IS PAST
      * LL -> US WHEN OPEN DATE IS PAST
      * US -> XM WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3100-SET-PAYEE-TYPE.
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
      * SPLIT REFUND AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPYM-REFUND-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BPYM-REFUND-AMT - (WS-WORK-AMT * 4)
           COMPUTE BPYT-RETURNED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY PAYEE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-PAYEE-TYPE-CD
                   ADD BPYM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED GRID TIER FOR THE RETURNED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3130-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-GRID-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF BPYM-RETURNED-YTD NOT < WS-RETURNED-GRID-LOW(WS-TX)
                   MOVE WS-RETURNED-GRID-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       3140-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-RETURNED-AMT ROUNDED =
                   BPYT-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYT-RETURNED-AMT
           END-IF
           ADD BPYT-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3150-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3160-LIMIT-FEE.
           IF BPYM-FEE-YTD < 0.00
               MOVE 0.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-FEE-YTD > 10000.00
               MOVE 10000.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3170-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO BPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3180-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPYM-PAYMENT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BPYM-PAYMENT-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETURNED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETURNED YTD.
      *----------------------------------------------------------------*
       3190-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-YTD ROUNDED =
                   BPYM-RETURNED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-RETURNED-YTD
           END-IF
           ADD BPYM-RETURNED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3200-DESCRIBE-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3210-FLAG-PAYMENT.
           IF BPYM-PAYMENT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE FEE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3230-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BPYM-FEE-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-CYCLE-CD = 'DA'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN BPYM-CYCLE-CD = 'FP'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BPYM-FEE-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3240-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYM-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = BPYM-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE BPYM-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E516 DESC REQUIRED
      * E618 REFUND AMT NEGATIVE
      * E286 REFUND AMT OVER 10000
      * E982 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-DESC = SPACES
                   MOVE 'E516' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-AMT < 0
                   MOVE 'E618' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-AMT > 10000.00
                   MOVE 'E286' TO WS-REJECT-CODE
               WHEN BPYM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E982' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3260-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BPYM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPYM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND GRID TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-GRID-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF BPYM-FEE-YTD NOT < WS-REFUND-GRID-LOW(WS-TX)
                   MOVE WS-REFUND-GRID-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETURNED AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-RETURNED.
           IF BPYM-RETURNED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-RETURNED-AMT * BPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BPYT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETURNED YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3290-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPYM-RETURNED-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BPYM-RETURNED-YTD - (WS-WORK-AMT * 3)
           COMPUTE BPYT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 5
               MOVE 'Y' TO BPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3310-FLAG-PAYMENT.
           IF BPYM-PAYMENT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-LAST-STMT.
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
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3330-AGE-NOTICE.
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
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3340-ACCRUE-PAYMENT.
           IF BPYM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-PAYMENT-YTD * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYT-RETURNED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3350-CHECK-LAST-STMT.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3360-FLAG-PAYMENT.
           IF BPYM-PAYMENT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3370-AGE-EFF.
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
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       3380-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-PAYMENT-YTD ROUNDED =
                   BPYM-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-PAYMENT-YTD
           END-IF
           ADD BPYM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-CYCLE.
           IF BPYM-CYCLE-CNT < 9999999
               ADD 1 TO BPYM-CYCLE-CNT
           END-IF
           IF BPYM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO BPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-ITEM.
           IF BPYM-ITEM-CNT < 9999999
               ADD 1 TO BPYM-ITEM-CNT
           END-IF
           IF BPYM-ITEM-CNT NOT < 5
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3410-DESCRIBE-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3430-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3440-DESCRIBE-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3450-COUNT-CYCLE.
           IF BPYM-CYCLE-CNT < 9999999
               ADD 1 TO BPYM-CYCLE-CNT
           END-IF
           IF BPYM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REMIT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3460-DESCRIBE-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FNS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NMS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TAF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VNK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WUA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XCT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-PAYMENT.
           IF BPYM-PAYMENT-CNT < 9999999
               ADD 1 TO BPYM-PAYMENT-CNT
           END-IF
           IF BPYM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3480-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF BPYM-LAST-ACT-DATE > 0 AND BPYM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3500-ACCRUE-PAYMENT.
           IF BPYM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-PAYMENT-AMT * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYT-RETURNED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3510-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-LAST-STMT.
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
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3530-ACCRUE-REFUND.
           IF BPYM-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-REFUND-AMT * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3540-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPYT-REFUND-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BPYT-REFUND-AMT - (WS-WORK-AMT * 2)
           COMPUTE BPYM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3550-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SC -> PD WHEN LAST STMT DATE IS PAST
      * PD -> RT WHEN LAST STMT DATE IS PAST
      * RT -> CN WHEN LAST STMT DATE IS PAST
      * CN -> RF WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3560-SET-TRAN.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E271 REF NO REQUIRED
      * E161 RETURNED AMT NEGATIVE
      * E376 RETURNED AMT OVER 5000
      * E313 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3570-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-REF-NO = SPACES
                   MOVE 'E271' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-AMT < 0
                   MOVE 'E161' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-AMT > 5000.00
                   MOVE 'E376' TO WS-REJECT-CODE
               WHEN BPYT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E313' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3580-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPYM-PAYMENT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BPYM-PAYMENT-AMT - (WS-WORK-AMT * 12)
           COMPUTE BPYM-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3590-ACCUM-TRAN.
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
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-PAYMENT.
           IF BPYM-PAYMENT-CNT < 9999999
               ADD 1 TO BPYM-PAYMENT-CNT
           END-IF
           IF BPYM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RETURNED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3610-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPYT-RETURNED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BPYT-RETURNED-AMT - (WS-WORK-AMT * 2)
           COMPUTE BPYM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3620-FLAG-PAYMENT.
           IF BPYM-PAYMENT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3630-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-REASON-CD
                   ADD BPYT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE PAYMENT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3640-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BPYM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   COMPUTE BPYM-FEE-YTD ROUNDED = BPYM-PAYMENT-YTD *
                       BPYM-RATE / 200
               WHEN BPYM-STATUS-CD = 'JX'
                   MOVE 12.00 TO BPYM-FEE-YTD
               WHEN BPYM-STATUS-CD = 'LR'
                   MOVE 15.00 TO BPYM-FEE-YTD
               WHEN OTHER
                   COMPUTE BPYM-FEE-YTD ROUNDED = BPYM-PAYMENT-YTD *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-FEE-YTD > 0 AND BPYM-FEE-YTD < 1.00
               MOVE 1.00 TO BPYM-FEE-YTD
           END-IF
           IF BPYM-FEE-YTD > 250.00
               MOVE 250.00 TO BPYM-FEE-YTD
           END-IF
           ADD BPYM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3650-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPYT-PAYMENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPYT-PAYMENT-AMT - (WS-WORK-AMT * 3)
           COMPUTE BPYT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REFUND YTD.
      *----------------------------------------------------------------*
       3660-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-REFUND-YTD ROUNDED =
                   BPYM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-REFUND-YTD
           END-IF
           ADD BPYM-REFUND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3670-AGE-EFFECTIVE.
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
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3680-FLAG-FEE.
           IF BPYM-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF BPYM-LAST-ACT-DATE > 0 AND BPYM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE RETURNED AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3710-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO BPYT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-HOLD-FLAG = 'Y'
                   COMPUTE BPYT-PAYMENT-AMT ROUNDED = BPYT-RETURNED-AMT
                       * BPYM-PENALTY-PCT / 200
               WHEN BPYM-CYCLE-CD = 'DA'
                   MOVE 12.00 TO BPYT-PAYMENT-AMT
               WHEN BPYM-CYCLE-CD = 'FP'
                   MOVE 20.00 TO BPYT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE BPYT-PAYMENT-AMT ROUNDED = BPYT-RETURNED-AMT
                       * BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYT-PAYMENT-AMT > 0 AND BPYT-PAYMENT-AMT < 10.00
               MOVE 10.00 TO BPYT-PAYMENT-AMT
           END-IF
           IF BPYT-PAYMENT-AMT > 35.00
               MOVE 35.00 TO BPYT-PAYMENT-AMT
           END-IF
           ADD BPYT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE PAYMENT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3720-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BPYM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   COMPUTE BPYM-PAYMENT-AMT ROUNDED = BPYM-PAYMENT-YTD *
                       BPYM-MARGIN / 200
               WHEN BPYM-PAYEE-TYPE-CD = 'BT'
                   MOVE 15.00 TO BPYM-PAYMENT-AMT
               WHEN BPYM-PAYEE-TYPE-CD = 'FR'
                   MOVE 10.00 TO BPYM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE BPYM-PAYMENT-AMT ROUNDED = BPYM-PAYMENT-YTD *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYM-PAYMENT-AMT > 0 AND BPYM-PAYMENT-AMT < 2.00
               MOVE 2.00 TO BPYM-PAYMENT-AMT
           END-IF
           IF BPYM-PAYMENT-AMT > 75.00
               MOVE 75.00 TO BPYM-PAYMENT-AMT
           END-IF
           ADD BPYM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3730-LIMIT-REFUND.
           IF BPYT-REFUND-AMT < 25.00
               MOVE 25.00 TO BPYT-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-REFUND-AMT > 50000.00
               MOVE 50000.00 TO BPYT-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3740-LIMIT-PAYMENT.
           IF BPYM-PAYMENT-AMT < 25.00
               MOVE 25.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-PAYMENT-AMT > 5000.00
               MOVE 5000.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E755 CONTACT REQUIRED
      * E706 RETURNED AMT NEGATIVE
      * E124 RETURNED AMT OVER 250000
      * E693 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3750-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-CONTACT = SPACES
                   MOVE 'E755' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-AMT < 0
                   MOVE 'E706' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-AMT > 250000.00
                   MOVE 'E124' TO WS-REJECT-CODE
               WHEN BPYM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E693' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REFUND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3760-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-REFUND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3770-LIMIT-FEE.
           IF BPYT-FEE-AMT < 50.00
               MOVE 50.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-FEE-AMT > 2500.00
               MOVE 2500.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3780-LIMIT-REFUND.
           IF BPYM-REFUND-AMT < 0.00
               MOVE 0.00 TO BPYM-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-REFUND-AMT > 5000.00
               MOVE 5000.00 TO BPYM-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN POST DATE IS PAST
      * FP -> JH WHEN POST DATE IS PAST
      * JH -> JV WHEN POST DATE IS PAST
      * JV -> RM WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FP' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FP'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JH' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JH'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JV' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JV'
                   IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E799 MEMO REQUIRED
      * E231 PAYMENT AMT NEGATIVE
      * E606 PAYMENT AMT OVER 99999
      * E380 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-MEMO = SPACES
                   MOVE 'E799' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT < 0
                   MOVE 'E231' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT > 99999.00
                   MOVE 'E606' TO WS-REJECT-CODE
               WHEN BPYM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E380' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3810-AGE-EFFECTIVE.
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
                   ADD BPYM-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BPYM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPYM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE RETURNED AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3820-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPYM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BPYM-FEE-YTD ROUNDED = BPYM-RETURNED-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO BPYM-FEE-YTD
               WHEN BPYT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO BPYM-FEE-YTD
               WHEN OTHER
                   COMPUTE BPYM-FEE-YTD ROUNDED = BPYM-RETURNED-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYM-FEE-YTD > 0 AND BPYM-FEE-YTD < 1.00
               MOVE 1.00 TO BPYM-FEE-YTD
           END-IF
           IF BPYM-FEE-YTD > 25.00
               MOVE 25.00 TO BPYM-FEE-YTD
           END-IF
           ADD BPYM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT REFUND YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPYM-REFUND-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BPYM-REFUND-YTD - (WS-WORK-AMT * 3)
           COMPUTE BPYT-RETURNED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3860-CHECK-LAST-STMT.
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
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3870-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BPYT-FEE-AMT NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3880-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3890-DESCRIBE-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BX'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REFUND AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3900-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPYT-REFUND-AMT / 12
           COMPUTE WS-WORK-AMT-2 = BPYT-REFUND-AMT - (WS-WORK-AMT * 12)
           COMPUTE BPYT-RETURNED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3910-COUNT-PAYMENT.
           IF BPYM-PAYMENT-CNT < 9999999
               ADD 1 TO BPYM-PAYMENT-CNT
           END-IF
           IF BPYM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETURNED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3920-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-RETURNED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETURNED AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3930-ACCRUE-RETURNED.
           IF BPYT-RETURNED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-RETURNED-AMT * BPYM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPYM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REMIT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3940-DESCRIBE-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FNS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NMS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TAF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VNK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WUA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XCT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REMIT METHOD CD:
      * CGV -> FNS WHEN EXPIRY DATE IS PAST
      * FNS -> NMS WHEN EXPIRY DATE IS PAST
      * NMS -> TAF WHEN EXPIRY DATE IS PAST
      * TAF -> VNK WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3950-SET-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNS'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NMS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NMS'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TAF' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TAF'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VNK' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3960-AGE-LAST-STMT.
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
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3970-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETURNED AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3980-FLAG-RETURNED.
           IF BPYM-RETURNED-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYM-RETURNED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       3990-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-RETURNED-AMT ROUNDED =
                   BPYM-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYT-RETURNED-AMT
           END-IF
           ADD BPYT-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN NEXT DUE DATE IS PAST
      * LR -> NZ WHEN NEXT DUE DATE IS PAST
      * NZ -> RV WHEN NEXT DUE DATE IS PAST
      * RV -> TM WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4000-SET-STATUS.
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
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4010-LIMIT-FEE.
           IF BPYT-FEE-AMT < 0.00
               MOVE 0.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-FEE-AMT > 2500.00
               MOVE 2500.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4020-LIMIT-PAYMENT.
           IF BPYM-PAYMENT-AMT < 10.00
               MOVE 10.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-PAYMENT-AMT > 2500.00
               MOVE 2500.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4030-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4040-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4050-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYT-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BPYT-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE BPYM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED GRID TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4060-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-GRID-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF BPYT-REFUND-AMT NOT < WS-RETURNED-GRID-LOW(WS-TX)
                   MOVE WS-RETURNED-GRID-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E848 OPERATOR REQUIRED
      * E090 PAYMENT AMT NEGATIVE
      * E392 PAYMENT AMT OVER 25000
      * E759 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4070-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-OPERATOR = SPACES
                   MOVE 'E848' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT < 0
                   MOVE 'E090' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT > 25000.00
                   MOVE 'E392' TO WS-REJECT-CODE
               WHEN BPYM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E759' TO WS-REJECT-CODE
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
       4080-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE BPYM-RETURNED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETURNED AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4090-ACCRUE-RETURNED.
           IF BPYM-RETURNED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-RETURNED-AMT * BPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BPYM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REFUND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       4100-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-FEE-AMT ROUNDED =
                   BPYM-REFUND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYT-FEE-AMT
           END-IF
           ADD BPYT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4110-DESCRIBE-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'HD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'NE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4120-FLAG-FEE.
           IF BPYM-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4130-FLAG-FEE.
           IF BPYM-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       4140-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-AMT ROUNDED =
                   BPYM-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-RETURNED-AMT
           END-IF
           ADD BPYM-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * BT -> FR WHEN LAST STMT DATE IS PAST
      * FR -> LL WHEN LAST STMT DATE IS PAST
      * LL -> US WHEN LAST STMT DATE IS PAST
      * US -> XM WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4150-SET-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FR' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FR'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LL' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LL'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4160-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE REFUND AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4170-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = BPYT-REFUND-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-REMIT-METHOD-CD = 'CGV'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN BPYM-REMIT-METHOD-CD = 'FNS'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = BPYT-REFUND-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       4180-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-REFUND-AMT ROUNDED =
                   BPYM-PAYMENT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYT-REFUND-AMT
           END-IF
           ADD BPYT-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETURNED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4190-FLAG-RETURNED.
           IF BPYT-RETURNED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYT-RETURNED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT DUE DATE IS PAST
      * BAT -> MAL WHEN NEXT DUE DATE IS PAST
      * MAL -> TEL WHEN NEXT DUE DATE IS PAST
      * TEL -> ONL WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4200-SET-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * BT -> FR WHEN EXPIRY DATE IS PAST
      * FR -> LL WHEN EXPIRY DATE IS PAST
      * LL -> US WHEN EXPIRY DATE IS PAST
      * US -> XM WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FR' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FR'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LL' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LL'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4220-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPYM-RETURNED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = WS-WORK-AMT-2 *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-SOURCE-CD = 'CT'
                   MOVE 3.00 TO BPYM-RETURNED-YTD
               WHEN BPYM-SOURCE-CD = 'FV'
                   MOVE 7.00 TO BPYM-RETURNED-YTD
               WHEN OTHER
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = WS-WORK-AMT-2 *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYM-RETURNED-YTD > 0 AND BPYM-RETURNED-YTD < 2.00
               MOVE 2.00 TO BPYM-RETURNED-YTD
           END-IF
           IF BPYM-RETURNED-YTD > 250.00
               MOVE 250.00 TO BPYM-RETURNED-YTD
           END-IF
           ADD BPYM-RETURNED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4230-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF BPYM-PAYMENT-AMT NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED YTD FEE ON THE RETURNED AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4240-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BPYM-RETURNED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = BPYT-RETURNED-AMT
                       * BPYM-RATE / 200
               WHEN BPYT-TRAN-CD = 'SC'
                   MOVE 5.00 TO BPYM-RETURNED-YTD
               WHEN BPYT-TRAN-CD = 'PD'
                   MOVE 12.00 TO BPYM-RETURNED-YTD
               WHEN OTHER
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = BPYT-RETURNED-AMT
                       * BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-RETURNED-YTD > 0 AND BPYM-RETURNED-YTD < 10.00
               MOVE 10.00 TO BPYM-RETURNED-YTD
           END-IF
           IF BPYM-RETURNED-YTD > 50.00
               MOVE 50.00 TO BPYM-RETURNED-YTD
           END-IF
           ADD BPYM-RETURNED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4250-LIMIT-PAYMENT.
           IF BPYM-PAYMENT-AMT < 0.00
               MOVE 0.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-PAYMENT-AMT > 5000.00
               MOVE 5000.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4260-DESCRIBE-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RETURNED AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4270-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPYM-RETURNED-AMT / 2
           COMPUTE WS-WORK-AMT-2 = BPYM-RETURNED-AMT - (WS-WORK-AMT * 2)
           COMPUTE BPYM-REFUND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E833 CONTACT REQUIRED
      * E376 PAYMENT AMT NEGATIVE
      * E227 PAYMENT AMT OVER 25000
      * E526 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4280-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-CONTACT = SPACES
                   MOVE 'E833' TO WS-REJECT-CODE
               WHEN BPYM-PAYMENT-AMT < 0
                   MOVE 'E376' TO WS-REJECT-CODE
               WHEN BPYM-PAYMENT-AMT > 25000.00
                   MOVE 'E227' TO WS-REJECT-CODE
               WHEN BPYM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E526' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4290-CHECK-OPEN.
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
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4300-ACCRUE-REFUND.
           IF BPYM-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-REFUND-AMT * BPYM-RATE / 100 / 365
               ADD WS-WORK-AMT TO BPYM-RETURNED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4310-FLAG-FEE.
           IF BPYM-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REMIT METHOD CD:
      * CGV -> FNS WHEN NOTICE DATE IS PAST
      * FNS -> NMS WHEN NOTICE DATE IS PAST
      * NMS -> TAF WHEN NOTICE DATE IS PAST
      * TAF -> VNK WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4320-SET-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNS'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NMS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NMS'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TAF' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TAF'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VNK' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4330-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4340-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * BT -> FR WHEN OPEN DATE IS PAST
      * FR -> LL WHEN OPEN DATE IS PAST
      * LL -> US WHEN OPEN DATE IS PAST
      * US -> XM WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4350-SET-PAYEE-TYPE.
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
      * ASSESS THE FEE FEE ON THE RETURNED AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4360-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BPYM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE BPYM-FEE-AMT ROUNDED = BPYM-RETURNED-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-CYCLE-CD = 'DA'
                   MOVE 10.00 TO BPYM-FEE-AMT
               WHEN BPYM-CYCLE-CD = 'FP'
                   MOVE 15.00 TO BPYM-FEE-AMT
               WHEN OTHER
                   COMPUTE BPYM-FEE-AMT ROUNDED = BPYM-RETURNED-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYM-FEE-AMT > 0 AND BPYM-FEE-AMT < 5.00
               MOVE 5.00 TO BPYM-FEE-AMT
           END-IF
           IF BPYM-FEE-AMT > 25.00
               MOVE 25.00 TO BPYM-FEE-AMT
           END-IF
           ADD BPYM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4370-LIMIT-PAYMENT.
           IF BPYM-PAYMENT-YTD < 25.00
               MOVE 25.00 TO BPYM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-PAYMENT-YTD > 500.00
               MOVE 500.00 TO BPYM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E928 REJECT CD REQUIRED
      * E340 WORK AMT NEGATIVE
      * E346 WORK AMT OVER 25000
      * E838 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4380-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-REJECT-CD = SPACES
                   MOVE 'E928' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E340' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E346' TO WS-REJECT-CODE
               WHEN BPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E838' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED GRID TIER FOR THE RETURNED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4390-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-GRID-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF BPYM-RETURNED-YTD NOT < WS-RETURNED-GRID-LOW(WS-TX)
                   MOVE WS-RETURNED-GRID-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4400-DESCRIBE-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       4410-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-REFUND-AMT ROUNDED =
                   BPYT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE RETURNED AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4420-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO BPYT-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE BPYT-REFUND-AMT ROUNDED = BPYT-RETURNED-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO BPYT-REFUND-AMT
               WHEN BPYT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO BPYT-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYT-REFUND-AMT ROUNDED = BPYT-RETURNED-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYT-REFUND-AMT > 0 AND BPYT-REFUND-AMT < 2.00
               MOVE 2.00 TO BPYT-REFUND-AMT
           END-IF
           IF BPYT-REFUND-AMT > 35.00
               MOVE 35.00 TO BPYT-REFUND-AMT
           END-IF
           ADD BPYT-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4430-DESCRIBE-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4440-COUNT-PAYMENT.
           IF BPYM-PAYMENT-CNT < 9999999
               ADD 1 TO BPYM-PAYMENT-CNT
           END-IF
           IF BPYM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO BPYM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E850 MEMO REQUIRED
      * E563 REFUND AMT NEGATIVE
      * E214 REFUND AMT OVER 25000
      * E903 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4450-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-MEMO = SPACES
                   MOVE 'E850' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-AMT < 0
                   MOVE 'E563' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-AMT > 25000.00
                   MOVE 'E214' TO WS-REJECT-CODE
               WHEN BPYT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E903' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CT -> FV WHEN CLOSE DATE IS PAST
      * FV -> HD WHEN CLOSE DATE IS PAST
      * HD -> KD WHEN CLOSE DATE IS PAST
      * KD -> NE WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4460-SET-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FV' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FV'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HD'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4470-CHECK-OPEN.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4480-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4490-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * BT -> FR WHEN EXPIRY DATE IS PAST
      * FR -> LL WHEN EXPIRY DATE IS PAST
      * LL -> US WHEN EXPIRY DATE IS PAST
      * US -> XM WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4500-SET-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FR' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FR'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LL' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LL'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       4510-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-REFUND-AMT ROUNDED =
                   BPYM-PAYMENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYT-REFUND-AMT
           END-IF
           ADD BPYT-REFUND-AMT TO WS-TOTAL-PRORATED.

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
                   MOVE BPSTAT-REFUND-AMT
                     TO BPYM-REFUND-AMT
                   MOVE BPSTAT-PAYMENT-YTD
                     TO BPYM-PAYMENT-YTD
                   MOVE BPSTAT-RETURNED-YTD
                     TO BPYM-RETURNED-YTD
                   MOVE BPSTAT-FEE-AMT
                     TO BPYM-FEE-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-BPSTAT' TO ABND-PARAGRAPH
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
                   MOVE BPPRC-SUSPEND-FLAG
                     TO BPYM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-BPPRC' TO ABND-PARAGRAPH
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
                   MOVE BPELG-REFUND-YTD
                     TO BPYM-REFUND-YTD
                   MOVE BPELG-PAYMENT-AMT
                     TO BPYM-PAYMENT-AMT
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
           DISPLAY 'BPYSETL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'BPYSETL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
