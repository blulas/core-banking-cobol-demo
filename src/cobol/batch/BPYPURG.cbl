      *================================================================*
      * PROGRAM  : BPYPURG                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : BILL PAYMENT SERVICE - PURGE AND ARCHIVE. READS THE *
      *            BPY TRANSACTIONS, UPDATES THE BPY MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. CONSUMER BILL PAYMENT  *
      *            PAYEE MAINTENANCE AND CHECK OR EFT DISBURSEMENT     *
      * INPUT    : BPYMST (KSDS), BPYTRN, BANKCTL                      *
      * OUTPUT   : BPYOUT, BPYPURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, BPSTAT                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-08-12 BHS  ORIGINAL                                       *
      * 1992-11-10 GAF  PERFORMANCE - PAYMENT READ AHEAD PRB6994       *
      * 1995-01-23 DPS  RETURNED LIMIT REVISED INC5364                 *
      * 1999-10-09 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-12-09 LMB  AUDIT FINDING - FEE CONTROL TOTALS CHG8920     *
      * 2003-01-04 MKL  PERFORMANCE - RETURNED READ AHEAD INC1981      *
      * 2005-01-11 DPS  RETURNED NOTICE TEXT PER LEGAL INC7062         *
      * 2005-08-26 RJT  RETURNED FEE SCHEDULE UPDATE INC4233           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BPYPURG.
       AUTHOR. M K LOWE.

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
           SELECT REPORT-FILE ASSIGN TO "BPYPURPT"
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
                                           VALUE 16.000000.
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
      * WS-FEE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000435000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000123500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000642000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007550000000515500'.
       01  WS-FEE-SCHED-TBL REDEFINES WS-FEE-SCHED-VALUES.
           05  WS-FEE-SCHED-ROW OCCURS 4 TIMES.
               10  WS-FEE-SCHED-LOW        PIC 9(09)V99.
               10  WS-FEE-SCHED-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000771000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000056500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000718000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000645000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003850000000797000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003950000000591000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004950000000301000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000189000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005150000000333000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000558500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010200000000714000'.
       01  WS-FEE-BAND-TBL REDEFINES WS-FEE-BAND-VALUES.
           05  WS-FEE-BAND-ROW OCCURS 11 TIMES.
               10  WS-FEE-BAND-LOW         PIC 9(09)V99.
               10  WS-FEE-BAND-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-REFUND-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REFUND-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000574000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000705000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000804000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000594500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000293000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000563000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000228000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016050000000811000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018550000000687000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021050000000368500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026050000000309500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026100000000776000'.
       01  WS-REFUND-SCHED-TBL REDEFINES WS-REFUND-SCHED-VALUES.
           05  WS-REFUND-SCHED-ROW OCCURS 12 TIMES.
               10  WS-REFUND-SCHED-LOW     PIC 9(09)V99.
               10  WS-REFUND-SCHED-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RETURNED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RETURNED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000389000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000698000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000435500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007600000000428500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007850000000772500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008850000000202000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009350000000567000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019350000000084000'.
       01  WS-RETURNED-GRID-TBL REDEFINES WS-RETURNED-GRID-VALUES.
           05  WS-RETURNED-GRID-ROW OCCURS 8 TIMES.
               10  WS-RETURNED-GRID-LOW    PIC 9(09)V99.
               10  WS-RETURNED-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  BPYPURG  BILL PAYMENT SERVICE'.
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
               PERFORM 3000-SPLIT-REFUND
               PERFORM 3010-ACCUM-STATUS
               PERFORM 3020-ASSESS-PREFERRED
               PERFORM 3030-SET-REASON
               PERFORM 3040-EDIT-DESC
               PERFORM 3050-ASSESS-EXEMPT
               PERFORM 3060-FLAG-FEE
               PERFORM 3070-SET-CHANNEL
               PERFORM 3080-ASSESS-PREFERRED
               PERFORM 3090-COUNT-CYCLE
               PERFORM 3100-DESCRIBE-TRAN
               PERFORM 3110-ACCUM-PAYEE-TYPE
               PERFORM 3120-LIMIT-RETURNED
               PERFORM 3130-CHECK-EXPIRY
               PERFORM 3140-SET-REMIT-METHOD
               PERFORM 3150-ACCUM-STATUS
               PERFORM 3160-ACCUM-REMIT-METHOD
               PERFORM 3170-ACCUM-STATUS
               PERFORM 3180-AGE-OPEN
               PERFORM 3190-FLAG-PAYMENT
               PERFORM 3200-ASSESS-EXEMPT
               PERFORM 3210-CHECK-NEXT-DUE
               PERFORM 3220-DESCRIBE-TRAN
               PERFORM 3230-SELECT-MARGIN
               PERFORM 3240-SET-CYCLE
               PERFORM 3250-ACCUM-CYCLE
               PERFORM 3260-LIMIT-PAYMENT
               PERFORM 3270-ACCUM-PAYEE-TYPE
               PERFORM 3280-SELECT-RATE
               PERFORM 3290-ASSESS-OVERRIDE
               PERFORM 3300-AGE-LAST-STMT
               PERFORM 3310-SPLIT-PAYMENT
               PERFORM 3320-ACCRUE-PENALTY-PCT
               PERFORM 3330-ACCRUE-REFUND
               PERFORM 3340-EDIT-REJECT
               PERFORM 3350-DESCRIBE-CHANNEL
               PERFORM 3360-ASSESS-OVERRIDE
               PERFORM 3370-SET-STATUS
               PERFORM 3380-LIMIT-FEE
               PERFORM 3390-DESCRIBE-STATUS
               PERFORM 3400-COUNT-SEQ
               PERFORM 3410-SPLIT-PAYMENT
               PERFORM 3420-SELECT-RATE
               PERFORM 3430-ACCRUE-FEE
               PERFORM 3440-SPLIT-ITEM
               PERFORM 3450-ACCUM-SOURCE
               PERFORM 3460-PRORATE-FEE
               PERFORM 3470-LIMIT-PAYMENT
               PERFORM 3480-FLAG-FEE
               PERFORM 3490-ASSESS-OVERRIDE
               PERFORM 3500-SET-REASON
               PERFORM 3510-ACCUM-STATUS
               PERFORM 3520-ASSESS-SUSPEND
               PERFORM 3530-ACCUM-SOURCE
               PERFORM 3540-ACCUM-SOURCE
               PERFORM 3550-FLAG-PAYMENT
               PERFORM 3560-PRORATE-RETURNED
               PERFORM 3570-SELECT-RATE
               PERFORM 3580-COUNT-ITEM
               PERFORM 3590-ACCUM-CYCLE
               PERFORM 3600-LIMIT-RETURNED
               PERFORM 3610-COUNT-PAYMENT
               PERFORM 3620-SET-REASON
               PERFORM 3630-LIMIT-FEE
               PERFORM 3640-EDIT-CUST-ID
               PERFORM 3650-DESCRIBE-REMIT-METHOD
               PERFORM 3660-AGE-NOTICE
               PERFORM 3670-PRORATE-REFUND
               PERFORM 3680-AGE-EFFECTIVE
               PERFORM 3690-PRORATE-PAYMENT
               PERFORM 3700-ACCUM-CHANNEL
               PERFORM 3710-PRORATE-REFUND
               PERFORM 3720-FLAG-ITEM
               PERFORM 3730-CHECK-EXPIRY
               PERFORM 3740-ACCRUE-FEE
               PERFORM 3750-LIMIT-RETURNED
               PERFORM 3760-FLAG-REFUND
               PERFORM 3770-ASSESS-PREFERRED
               PERFORM 3780-EDIT-DESC
               PERFORM 3790-SELECT-MARGIN
               PERFORM 3800-FLAG-FEE
               PERFORM 3810-ACCUM-PAYEE-TYPE
               PERFORM 3820-AGE-MATURITY
               PERFORM 3830-ACCRUE-FEE
               PERFORM 3840-SET-CYCLE
               PERFORM 3850-LIMIT-FEE
               PERFORM 3860-EDIT-REF-NO
               PERFORM 3870-LIMIT-FEE
               PERFORM 3880-DESCRIBE-CYCLE
               PERFORM 3890-SELECT-MARGIN
               PERFORM 3900-EDIT-CUST-ID
               PERFORM 3910-ASSESS-PREFERRED
               PERFORM 3920-SET-PAYEE-TYPE
               PERFORM 3930-AGE-EFF
               PERFORM 3940-SET-REMIT-METHOD
               PERFORM 3950-LIMIT-FEE
               PERFORM 3960-SELECT-RATE
               PERFORM 3970-ACCRUE-RATE
               PERFORM 3980-PRORATE-FEE
               PERFORM 3990-SET-SOURCE
               PERFORM 4000-SELECT-MARGIN
               PERFORM 4010-PRORATE-REFUND
               PERFORM 4020-COUNT-PAYMENT
               PERFORM 4030-ASSESS-AUTO-RENEW
               PERFORM 4040-FLAG-FEE
               PERFORM 4050-AGE-NOTICE
               PERFORM 4060-AGE-OPEN
               PERFORM 4070-SET-CHANNEL
               PERFORM 4080-COUNT-SEQ
               PERFORM 4090-LIMIT-FEE
               PERFORM 4100-COUNT-PAYMENT
               PERFORM 7020-CALL-BPSTAT
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
      * SPLIT REFUND YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPYM-REFUND-YTD / 3
           COMPUTE WS-WORK-AMT-2 = BPYM-REFUND-YTD - (WS-WORK-AMT * 3)
           COMPUTE BPYT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3010-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-STATUS-CD
                   ADD BPYM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE REFUND AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3020-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO BPYM-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE BPYM-REFUND-AMT ROUNDED = BPYT-REFUND-AMT *
                       BPYM-MARGIN / 200
               WHEN BPYM-SOURCE-CD = 'CT'
                   MOVE 15.00 TO BPYM-REFUND-AMT
               WHEN BPYM-SOURCE-CD = 'FV'
                   MOVE 15.00 TO BPYM-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYM-REFUND-AMT ROUNDED = BPYT-REFUND-AMT *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYM-REFUND-AMT > 0 AND BPYM-REFUND-AMT < 1.00
               MOVE 1.00 TO BPYM-REFUND-AMT
           END-IF
           IF BPYM-REFUND-AMT > 100.00
               MOVE 100.00 TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> BX WHEN CLOSE DATE IS PAST
      * BX -> GM WHEN CLOSE DATE IS PAST
      * GM -> KU WHEN CLOSE DATE IS PAST
      * KU -> LT WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BX' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BX'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KU' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KU'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E640 DESC REQUIRED
      * E385 FEE AMT NEGATIVE
      * E058 FEE AMT OVER 10000
      * E979 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3040-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-DESC = SPACES
                   MOVE 'E640' TO WS-REJECT-CODE
               WHEN BPYT-FEE-AMT < 0
                   MOVE 'E385' TO WS-REJECT-CODE
               WHEN BPYT-FEE-AMT > 10000.00
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN BPYM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E979' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED FEE ON THE PAYMENT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3050-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BPYM-RETURNED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   COMPUTE BPYM-RETURNED-AMT ROUNDED = BPYM-PAYMENT-YTD
                       * BPYM-RATE / 200
               WHEN BPYT-TRAN-CD = 'SC'
                   MOVE 20.00 TO BPYM-RETURNED-AMT
               WHEN BPYT-TRAN-CD = 'PD'
                   MOVE 5.00 TO BPYM-RETURNED-AMT
               WHEN OTHER
                   COMPUTE BPYM-RETURNED-AMT ROUNDED = BPYM-PAYMENT-YTD
                       * BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-RETURNED-AMT > 0 AND BPYM-RETURNED-AMT < 5.00
               MOVE 5.00 TO BPYM-RETURNED-AMT
           END-IF
           IF BPYM-RETURNED-AMT > 250.00
               MOVE 250.00 TO BPYM-RETURNED-AMT
           END-IF
           ADD BPYM-RETURNED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3060-FLAG-FEE.
           IF BPYM-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN CLOSE DATE IS PAST
      * BAT -> MAL WHEN CLOSE DATE IS PAST
      * MAL -> TEL WHEN CLOSE DATE IS PAST
      * TEL -> ONL WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE PAYMENT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3080-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO BPYM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE BPYM-PAYMENT-YTD ROUNDED = BPYT-PAYMENT-AMT *
                       BPYM-RATE / 200
               WHEN BPYM-REASON-CD = 'AX'
                   MOVE 15.00 TO BPYM-PAYMENT-YTD
               WHEN BPYM-REASON-CD = 'BX'
                   MOVE 12.00 TO BPYM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE BPYM-PAYMENT-YTD ROUNDED = BPYT-PAYMENT-AMT *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-PAYMENT-YTD > 0 AND BPYM-PAYMENT-YTD < 2.00
               MOVE 2.00 TO BPYM-PAYMENT-YTD
           END-IF
           IF BPYM-PAYMENT-YTD > 35.00
               MOVE 35.00 TO BPYM-PAYMENT-YTD
           END-IF
           ADD BPYM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3090-COUNT-CYCLE.
           IF BPYM-CYCLE-CNT < 9999999
               ADD 1 TO BPYM-CYCLE-CNT
           END-IF
           IF BPYM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3100-DESCRIBE-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'PD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY PAYEE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3110-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-PAYEE-TYPE-CD
                   ADD BPYM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3120-LIMIT-RETURNED.
           IF BPYM-RETURNED-AMT < 100.00
               MOVE 100.00 TO BPYM-RETURNED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-RETURNED-AMT > 10000.00
               MOVE 10000.00 TO BPYM-RETURNED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-EXPIRY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REMIT METHOD CD:
      * CGV -> FNS WHEN MATURITY DATE IS PAST
      * FNS -> NMS WHEN MATURITY DATE IS PAST
      * NMS -> TAF WHEN MATURITY DATE IS PAST
      * TAF -> VNK WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3140-SET-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNS'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NMS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NMS'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TAF' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TAF'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VNK' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3150-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-STATUS-CD
                   ADD BPYT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY REMIT METHOD CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-REMIT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-REMIT-METHOD-CD
                   ADD BPYM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-REMIT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-STATUS-CD
                   ADD BPYM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3180-AGE-OPEN.
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
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3190-FLAG-PAYMENT.
           IF BPYM-PAYMENT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE RETURNED AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3200-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BPYT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   COMPUTE BPYT-FEE-AMT ROUNDED = BPYM-RETURNED-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-REASON-CD = 'AX'
                   MOVE 20.00 TO BPYT-FEE-AMT
               WHEN BPYM-REASON-CD = 'BX'
                   MOVE 20.00 TO BPYT-FEE-AMT
               WHEN OTHER
                   COMPUTE BPYT-FEE-AMT ROUNDED = BPYM-RETURNED-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYT-FEE-AMT > 0 AND BPYT-FEE-AMT < 10.00
               MOVE 10.00 TO BPYT-FEE-AMT
           END-IF
           IF BPYT-FEE-AMT > 250.00
               MOVE 250.00 TO BPYT-FEE-AMT
           END-IF
           ADD BPYT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO BPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3220-DESCRIBE-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3230-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BPYM-FEE-YTD NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN POST DATE IS PAST
      * FP -> JH WHEN POST DATE IS PAST
      * JH -> JV WHEN POST DATE IS PAST
      * JV -> RM WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3240-SET-CYCLE.
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
      * ACCUMULATE FEE YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3250-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-CYCLE-CD
                   ADD BPYM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3260-LIMIT-PAYMENT.
           IF BPYM-PAYMENT-AMT < 0.00
               MOVE 0.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-PAYMENT-AMT > 500.00
               MOVE 500.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY PAYEE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3270-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-PAYEE-TYPE-CD
                   ADD BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BPYT-REFUND-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE REFUND YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3290-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BPYM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   COMPUTE BPYM-FEE-YTD ROUNDED = BPYM-REFUND-YTD *
                       BPYM-MARGIN / 200
               WHEN BPYM-REASON-CD = 'AX'
                   MOVE 7.00 TO BPYM-FEE-YTD
               WHEN BPYM-REASON-CD = 'BX'
                   MOVE 20.00 TO BPYM-FEE-YTD
               WHEN OTHER
                   COMPUTE BPYM-FEE-YTD ROUNDED = BPYM-REFUND-YTD *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYM-FEE-YTD > 0 AND BPYM-FEE-YTD < 10.00
               MOVE 10.00 TO BPYM-FEE-YTD
           END-IF
           IF BPYM-FEE-YTD > 75.00
               MOVE 75.00 TO BPYM-FEE-YTD
           END-IF
           ADD BPYM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-LAST-STMT.
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
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPYM-PAYMENT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BPYM-PAYMENT-AMT - (WS-WORK-AMT * 4)
           COMPUTE BPYT-RETURNED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3320-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BPYM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BPYM-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-REFUND.
           IF BPYT-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-REFUND-AMT * BPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BPYT-RETURNED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E679 REJECT CD REQUIRED
      * E043 REFUND AMT NEGATIVE
      * E757 REFUND AMT OVER 5000
      * E081 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3340-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-REJECT-CD = SPACES
                   MOVE 'E679' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-AMT < 0
                   MOVE 'E043' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-AMT > 5000.00
                   MOVE 'E757' TO WS-REJECT-CODE
               WHEN BPYM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E081' TO WS-REJECT-CODE
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
       3350-DESCRIBE-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3360-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = BPYT-FEE-AMT *
                       BPYM-RATE / 200
               WHEN BPYM-CYCLE-CD = 'DA'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN BPYM-CYCLE-CD = 'FP'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = BPYT-FEE-AMT *
                       BPYM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN NOTICE DATE IS PAST
      * LR -> NZ WHEN NOTICE DATE IS PAST
      * NZ -> RV WHEN NOTICE DATE IS PAST
      * RV -> TM WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3370-SET-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LR' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LR'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3380-LIMIT-FEE.
           IF BPYM-FEE-YTD < 0.00
               MOVE 0.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-FEE-YTD > 5000.00
               MOVE 5000.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3390-DESCRIBE-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 12
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPYM-PAYMENT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = BPYM-PAYMENT-YTD - (WS-WORK-AMT * 2)
           COMPUTE BPYM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3420-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BPYT-REFUND-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3430-ACCRUE-FEE.
           IF BPYM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-FEE-YTD * BPYM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BPYM-REFUND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3440-SPLIT-ITEM.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-SOURCE-CD
                   ADD BPYM-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3460-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-FEE-YTD ROUNDED =
                   BPYM-PAYMENT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-FEE-YTD
           END-IF
           ADD BPYM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3470-LIMIT-PAYMENT.
           IF BPYM-PAYMENT-AMT < 50.00
               MOVE 50.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-PAYMENT-AMT > 50000.00
               MOVE 50000.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3480-FLAG-FEE.
           IF BPYT-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED YTD FEE ON THE REFUND YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3490-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BPYM-RETURNED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = BPYM-REFUND-YTD *
                       BPYM-RATE / 200
               WHEN BPYM-STATUS-CD = 'JX'
                   MOVE 5.00 TO BPYM-RETURNED-YTD
               WHEN BPYM-STATUS-CD = 'LR'
                   MOVE 12.00 TO BPYM-RETURNED-YTD
               WHEN OTHER
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = BPYM-REFUND-YTD *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-RETURNED-YTD > 0 AND BPYM-RETURNED-YTD < 10.00
               MOVE 10.00 TO BPYM-RETURNED-YTD
           END-IF
           IF BPYM-RETURNED-YTD > 75.00
               MOVE 75.00 TO BPYM-RETURNED-YTD
           END-IF
           ADD BPYM-RETURNED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> BX WHEN NEXT DUE DATE IS PAST
      * BX -> GM WHEN NEXT DUE DATE IS PAST
      * GM -> KU WHEN NEXT DUE DATE IS PAST
      * KU -> LT WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3500-SET-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BX' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BX'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KU' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KU'
                   IF BPYM-NEXT-DUE-DATE > 0 AND BPYM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-STATUS-CD
                   ADD BPYM-REFUND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-REFUND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE WORK AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3520-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BPYM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-HOLD-FLAG = 'Y'
                   COMPUTE BPYM-PAYMENT-YTD ROUNDED = WS-WORK-AMT *
                       BPYM-RATE / 200
               WHEN BPYT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO BPYM-PAYMENT-YTD
               WHEN BPYT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO BPYM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE BPYM-PAYMENT-YTD ROUNDED = WS-WORK-AMT *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-PAYMENT-YTD > 0 AND BPYM-PAYMENT-YTD < 10.00
               MOVE 10.00 TO BPYM-PAYMENT-YTD
           END-IF
           IF BPYM-PAYMENT-YTD > 250.00
               MOVE 250.00 TO BPYM-PAYMENT-YTD
           END-IF
           ADD BPYM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE RETURNED AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3530-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-SOURCE-CD
                   ADD BPYT-RETURNED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-RETURNED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3540-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-SOURCE-CD
                   ADD BPYM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3550-FLAG-PAYMENT.
           IF BPYM-PAYMENT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REFUND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETURNED YTD.
      *----------------------------------------------------------------*
       3560-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-YTD ROUNDED =
                   BPYM-REFUND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-RETURNED-YTD
           END-IF
           ADD BPYM-RETURNED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED GRID TIER FOR THE RETURNED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3570-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-GRID-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
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
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3580-COUNT-ITEM.
           IF BPYM-ITEM-CNT < 9999999
               ADD 1 TO BPYM-ITEM-CNT
           END-IF
           IF BPYM-ITEM-CNT NOT < 5
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RETURNED YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3590-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-CYCLE-CD
                   ADD BPYM-RETURNED-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-RETURNED-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3600-LIMIT-RETURNED.
           IF BPYM-RETURNED-YTD < 25.00
               MOVE 25.00 TO BPYM-RETURNED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-RETURNED-YTD > 50000.00
               MOVE 50000.00 TO BPYM-RETURNED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3610-COUNT-PAYMENT.
           IF BPYM-PAYMENT-CNT < 9999999
               ADD 1 TO BPYM-PAYMENT-CNT
           END-IF
           IF BPYM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> BX WHEN MATURITY DATE IS PAST
      * BX -> GM WHEN MATURITY DATE IS PAST
      * GM -> KU WHEN MATURITY DATE IS PAST
      * KU -> LT WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BX' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BX'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KU' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KU'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3630-LIMIT-FEE.
           IF BPYM-FEE-YTD < 10.00
               MOVE 10.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-FEE-YTD > 5000.00
               MOVE 5000.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E642 CUST ID REQUIRED
      * E602 RETURNED AMT NEGATIVE
      * E700 RETURNED AMT OVER 10000
      * E416 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-CUST-ID = SPACES
                   MOVE 'E642' TO WS-REJECT-CODE
               WHEN BPYT-RETURNED-AMT < 0
                   MOVE 'E602' TO WS-REJECT-CODE
               WHEN BPYT-RETURNED-AMT > 10000.00
                   MOVE 'E700' TO WS-REJECT-CODE
               WHEN BPYM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E416' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REMIT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3650-DESCRIBE-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FNS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NMS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TAF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VNK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WUA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XCT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3660-AGE-NOTICE.
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
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RETURNED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-REFUND-AMT ROUNDED =
                   BPYM-RETURNED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-EFFECTIVE.
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
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3690-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-PAYMENT-AMT ROUNDED =
                   BPYM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYT-PAYMENT-AMT
           END-IF
           ADD BPYT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3700-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REFUND YTD.
      *----------------------------------------------------------------*
       3710-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-REFUND-YTD ROUNDED =
                   BPYM-PAYMENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-REFUND-YTD
           END-IF
           ADD BPYM-REFUND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3720-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3730-CHECK-EXPIRY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-FEE.
           IF BPYT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-FEE-AMT * BPYM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPYM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3750-LIMIT-RETURNED.
           IF BPYM-RETURNED-AMT < 10.00
               MOVE 10.00 TO BPYM-RETURNED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-RETURNED-AMT > 2500.00
               MOVE 2500.00 TO BPYM-RETURNED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3760-FLAG-REFUND.
           IF BPYM-REFUND-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYM-REFUND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE REFUND YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3770-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BPYM-REFUND-YTD *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-SOURCE-CD = 'CT'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN BPYM-SOURCE-CD = 'FV'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = BPYM-REFUND-YTD *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 150.00
               MOVE 150.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E298 DESC REQUIRED
      * E568 WORK AMT NEGATIVE
      * E485 WORK AMT OVER 25000
      * E442 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3780-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-DESC = SPACES
                   MOVE 'E298' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E568' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E485' TO WS-REJECT-CODE
               WHEN BPYM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E442' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3790-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF BPYM-PAYMENT-YTD NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3800-FLAG-FEE.
           IF BPYT-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY PAYEE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3810-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-PAYEE-TYPE-CD
                   ADD BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3820-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3830-ACCRUE-FEE.
           IF BPYM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-FEE-AMT * BPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO BPYM-RETURNED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN EXPIRY DATE IS PAST
      * FP -> JH WHEN EXPIRY DATE IS PAST
      * JH -> JV WHEN EXPIRY DATE IS PAST
      * JV -> RM WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FP' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FP'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JH' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JH'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JV' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JV'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3850-LIMIT-FEE.
           IF BPYM-FEE-YTD < 25.00
               MOVE 25.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-FEE-YTD > 2500.00
               MOVE 2500.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E203 REF NO REQUIRED
      * E604 REFUND AMT NEGATIVE
      * E873 REFUND AMT OVER 25000
      * E067 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3860-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-REF-NO = SPACES
                   MOVE 'E203' TO WS-REJECT-CODE
               WHEN BPYT-REFUND-AMT < 0
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN BPYT-REFUND-AMT > 25000.00
                   MOVE 'E873' TO WS-REJECT-CODE
               WHEN BPYM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E067' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3870-LIMIT-FEE.
           IF BPYT-FEE-AMT < 0.00
               MOVE 0.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-FEE-AMT > 2500.00
               MOVE 2500.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'JH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED GRID TIER FOR THE REFUND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3890-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-GRID-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF BPYM-REFUND-AMT NOT < WS-RETURNED-GRID-LOW(WS-TX)
                   MOVE WS-RETURNED-GRID-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E859 CUST ID REQUIRED
      * E793 FEE AMT NEGATIVE
      * E426 FEE AMT OVER 25000
      * E195 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3900-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-CUST-ID = SPACES
                   MOVE 'E859' TO WS-REJECT-CODE
               WHEN BPYT-FEE-AMT < 0
                   MOVE 'E793' TO WS-REJECT-CODE
               WHEN BPYT-FEE-AMT > 25000.00
                   MOVE 'E426' TO WS-REJECT-CODE
               WHEN BPYM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E195' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE FEE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3910-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO BPYT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE BPYT-FEE-AMT ROUNDED = BPYM-FEE-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-REASON-CD = 'AX'
                   MOVE 10.00 TO BPYT-FEE-AMT
               WHEN BPYM-REASON-CD = 'BX'
                   MOVE 15.00 TO BPYT-FEE-AMT
               WHEN OTHER
                   COMPUTE BPYT-FEE-AMT ROUNDED = BPYM-FEE-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYT-FEE-AMT > 0 AND BPYT-FEE-AMT < 2.00
               MOVE 2.00 TO BPYT-FEE-AMT
           END-IF
           IF BPYT-FEE-AMT > 25.00
               MOVE 25.00 TO BPYT-FEE-AMT
           END-IF
           ADD BPYT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * BT -> FR WHEN EFFECTIVE DATE IS PAST
      * FR -> LL WHEN EFFECTIVE DATE IS PAST
      * LL -> US WHEN EFFECTIVE DATE IS PAST
      * US -> XM WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3920-SET-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FR' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FR'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LL' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LL'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3930-AGE-EFF.
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
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REMIT METHOD CD:
      * CGV -> FNS WHEN OPEN DATE IS PAST
      * FNS -> NMS WHEN OPEN DATE IS PAST
      * NMS -> TAF WHEN OPEN DATE IS PAST
      * TAF -> VNK WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNS'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NMS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NMS'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TAF' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TAF'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VNK' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3950-LIMIT-FEE.
           IF BPYM-FEE-AMT < 10.00
               MOVE 10.00 TO BPYM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-FEE-AMT > 2500.00
               MOVE 2500.00 TO BPYM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3970-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYT-RETURNED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETURNED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3980-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-FEE-AMT ROUNDED =
                   BPYT-RETURNED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYT-FEE-AMT
           END-IF
           ADD BPYT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CT -> FV WHEN EFF DATE IS PAST
      * FV -> HD WHEN EFF DATE IS PAST
      * HD -> KD WHEN EFF DATE IS PAST
      * KD -> NE WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3990-SET-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FV' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FV'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HD'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED GRID TIER FOR THE RETURNED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4000-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-GRID-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF BPYT-RETURNED-AMT NOT < WS-RETURNED-GRID-LOW(WS-TX)
                   MOVE WS-RETURNED-GRID-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       4010-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-REFUND-AMT ROUNDED =
                   BPYT-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4020-COUNT-PAYMENT.
           IF BPYM-PAYMENT-CNT < 9999999
               ADD 1 TO BPYM-PAYMENT-CNT
           END-IF
           IF BPYM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE PAYMENT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4030-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO BPYT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   COMPUTE BPYT-PAYMENT-AMT ROUNDED = BPYM-PAYMENT-AMT *
                       BPYM-MARGIN / 200
               WHEN BPYM-PAYEE-TYPE-CD = 'BT'
                   MOVE 12.00 TO BPYT-PAYMENT-AMT
               WHEN BPYM-PAYEE-TYPE-CD = 'FR'
                   MOVE 7.00 TO BPYT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE BPYT-PAYMENT-AMT ROUNDED = BPYM-PAYMENT-AMT *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYT-PAYMENT-AMT > 0 AND BPYT-PAYMENT-AMT < 10.00
               MOVE 10.00 TO BPYT-PAYMENT-AMT
           END-IF
           IF BPYT-PAYMENT-AMT > 50.00
               MOVE 50.00 TO BPYT-PAYMENT-AMT
           END-IF
           ADD BPYT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4040-FLAG-FEE.
           IF BPYM-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-NOTICE.
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
               WHEN WS-DAYS NOT > 30
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-OPEN.
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
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NOTICE DATE IS PAST
      * BAT -> MAL WHEN NOTICE DATE IS PAST
      * MAL -> TEL WHEN NOTICE DATE IS PAST
      * TEL -> ONL WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4070-SET-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4080-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 10
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4090-LIMIT-FEE.
           IF BPYT-FEE-AMT < 100.00
               MOVE 100.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-FEE-AMT > 10000.00
               MOVE 10000.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-PAYMENT.
           IF BPYM-PAYMENT-CNT < 9999999
               ADD 1 TO BPYM-PAYMENT-CNT
           END-IF
           IF BPYM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
           END-IF.

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
                   MOVE BPSTAT-PAYMENT-AMT
                     TO BPYM-PAYMENT-AMT
                   MOVE BPSTAT-RETURNED-AMT
                     TO BPYM-RETURNED-AMT
                   MOVE BPSTAT-REFUND-AMT
                     TO BPYM-REFUND-AMT
                   MOVE BPSTAT-PAYMENT-YTD
                     TO BPYM-PAYMENT-YTD
                   MOVE BPSTAT-RETURNED-YTD
                     TO BPYM-RETURNED-YTD
                   MOVE BPSTAT-FEE-AMT
                     TO BPYM-FEE-AMT
                   MOVE BPSTAT-SUSPEND-FLAG
                     TO BPYM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-BPSTAT' TO ABND-PARAGRAPH
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
           DISPLAY 'BPYPURG READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'BPYPURG' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
