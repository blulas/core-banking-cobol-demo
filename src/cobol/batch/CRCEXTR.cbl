      *================================================================*
      * PROGRAM  : CRCEXTR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PROPRIETARY CREDIT CARD - NIGHTLY EXTRACT. READS THE*
      *            CRC TRANSACTIONS, UPDATES THE CRC MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. IN-HOUSE CREDIT CARD   *
      *            BILLING, FINANCE CHARGE AND DELINQUENCY             *
      * INPUT    : CRCMST (KSDS), CRCTRN, BANKCTL                      *
      * OUTPUT   : CRCOUT, CRCEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CRCAL, CRDTE            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-07-17 RJT  ORIGINAL                                       *
      * 1996-05-05 PQR  PERFORMANCE - PURCHASE READ AHEAD REQ0964      *
      * 1999-05-24 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-08-12 PQR  NEW MIN-DUE REPORT COLUMNS CHG2278             *
      * 2003-04-02 BHS  AUDIT FINDING - FINANCE CONTROL TOTALS CHG6155 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRCEXTR.
       AUTHOR. C R PATEL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "CRCMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CRCM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "CRCTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "CRCOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "CRCEXRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY CRCMST.
       FD  TRAN-FILE.
       COPY CRCTRN.
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
                                           VALUE 8.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 91.
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
      * WS-PAYMENT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000377000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000368500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000521000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000381500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000791500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000021000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000281000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003800000000232000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004800000000344000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014800000000348500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015800000000814500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020800000000482000'.
       01  WS-PAYMENT-GRID-TBL REDEFINES WS-PAYMENT-GRID-VALUES.
           05  WS-PAYMENT-GRID-ROW OCCURS 12 TIMES.
               10  WS-PAYMENT-GRID-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PURCHASE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PURCHASE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000513500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000146500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000494000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000383500'.
       01  WS-PURCHASE-TIER-TBL REDEFINES WS-PURCHASE-TIER-VALUES.
           05  WS-PURCHASE-TIER-ROW OCCURS 4 TIMES.
               10  WS-PURCHASE-TIER-LOW    PIC 9(09)V99.
               10  WS-PURCHASE-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PURCHASE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PURCHASE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000594000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000719000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007500000000409000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000725000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000544500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012850000000050500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022850000000690000'.
       01  WS-PURCHASE-BAND-TBL REDEFINES WS-PURCHASE-BAND-VALUES.
           05  WS-PURCHASE-BAND-ROW OCCURS 7 TIMES.
               10  WS-PURCHASE-BAND-LOW    PIC 9(09)V99.
               10  WS-PURCHASE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-MIN-DUE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-MIN-DUE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000783000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000594000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000538500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000894500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006750000000787000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007000000000239000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000338500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000865000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000490000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022600000000360000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025100000000345000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030100000000227000'.
       01  WS-MIN-DUE-GRID-TBL REDEFINES WS-MIN-DUE-GRID-VALUES.
           05  WS-MIN-DUE-GRID-ROW OCCURS 12 TIMES.
               10  WS-MIN-DUE-GRID-LOW     PIC 9(09)V99.
               10  WS-MIN-DUE-GRID-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CRCEXTR  PROPRIETARY CREDIT CARD'.
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
       COPY CRCALP.
       COPY CRDTEP.

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
               MOVE 'CRCMST' TO ABND-FILE-NAME
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
           MOVE CRCT-KEY TO CRCM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-SPLIT-LIMIT
               PERFORM 3010-SET-TYPE
               PERFORM 3020-EDIT-OPERATOR
               PERFORM 3030-ASSESS-AUTO-RENEW
               PERFORM 3040-COUNT-RETRY
               PERFORM 3050-SET-STATUS
               PERFORM 3060-EDIT-DESC
               PERFORM 3070-CHECK-LAST-ACT
               PERFORM 3080-ACCRUE-DISCOUNT
               PERFORM 3090-SELECT-DISCOUNT
               PERFORM 3100-ACCUM-SOURCE
               PERFORM 3110-ACCUM-BRANCH
               PERFORM 3120-SET-TRAN
               PERFORM 3130-PRORATE-MIN-DUE
               PERFORM 3140-ASSESS-PREFERRED
               PERFORM 3150-ASSESS-EXEMPT
               PERFORM 3160-CHECK-LAST-PAY
               PERFORM 3170-SPLIT-LIMIT
               PERFORM 3180-COUNT-RETRY
               PERFORM 3190-EDIT-ADDR
               PERFORM 3200-SPLIT-MIN-DUE
               PERFORM 3210-SELECT-PENALTY-PCT
               PERFORM 3220-PRORATE-LIMIT
               PERFORM 3230-SPLIT-CASH-ADV
               PERFORM 3240-FLAG-MIN-DUE
               PERFORM 3250-ACCUM-CHANNEL
               PERFORM 3260-AGE-MATURITY
               PERFORM 3270-ASSESS-STOP
               PERFORM 3280-SELECT-DISCOUNT
               PERFORM 3290-SPLIT-LATE-FEE
               PERFORM 3300-CHECK-MATURITY
               PERFORM 3310-DESCRIBE-TRAN
               PERFORM 3320-SPLIT-LIMIT
               PERFORM 3330-ACCUM-SOURCE
               PERFORM 3340-EDIT-ADDR
               PERFORM 3350-AGE-EFFECTIVE
               PERFORM 3360-SET-STATUS
               PERFORM 3370-CHECK-EFFECTIVE
               PERFORM 3380-LIMIT-FINANCE
               PERFORM 3390-ACCUM-STATUS
               PERFORM 3400-COUNT-SEQ
               PERFORM 3410-COUNT-CYCLE
               PERFORM 3420-PRORATE-PAYMENT
               PERFORM 3430-PRORATE-PAYMENT
               PERFORM 3440-PRORATE-LIMIT
               PERFORM 3450-DESCRIBE-CARD-TYPE
               PERFORM 3460-AGE-MATURITY
               PERFORM 3470-CHECK-LAST-PAY
               PERFORM 3480-ASSESS-EXEMPT
               PERFORM 3490-FLAG-PAYMENT
               PERFORM 3500-DESCRIBE-STATUS
               PERFORM 3510-SPLIT-CASH-ADV
               PERFORM 3520-FLAG-MIN-DUE
               PERFORM 3530-AGE-OPEN
               PERFORM 3540-LIMIT-CASH-ADV
               PERFORM 3550-COUNT-ITEM
               PERFORM 3560-AGE-MATURITY
               PERFORM 3570-SPLIT-PAYMENT
               PERFORM 3580-CHECK-OPEN
               PERFORM 3590-SELECT-PENALTY-PCT
               PERFORM 3600-COUNT-RETRY
               PERFORM 3610-FLAG-MIN-DUE
               PERFORM 3620-PRORATE-PURCHASE
               PERFORM 3630-ASSESS-PREFERRED
               PERFORM 3640-LIMIT-PURCHASE
               PERFORM 3650-ACCUM-STATUS
               PERFORM 3660-EDIT-DESC
               PERFORM 3670-EDIT-REF-NO
               PERFORM 3680-EDIT-CUST-ID
               PERFORM 3690-SPLIT-FINANCE
               PERFORM 3700-AGE-MATURITY
               PERFORM 3710-LIMIT-MIN-DUE
               PERFORM 3720-EDIT-REF-NO
               PERFORM 3730-COUNT-SEQ
               PERFORM 3740-CHECK-POST
               PERFORM 3750-SELECT-DISCOUNT
               PERFORM 3760-AGE-EFF
               PERFORM 3770-DESCRIBE-BRANCH
               PERFORM 3780-ACCUM-TYPE
               PERFORM 3790-ASSESS-AUTO-RENEW
               PERFORM 3800-SELECT-PENALTY-PCT
               PERFORM 3810-SET-BILL-CYCLE
               PERFORM 3820-ACCRUE-CASH-ADV
               PERFORM 3830-SET-TYPE
               PERFORM 3840-COUNT-SEQ
               PERFORM 3850-ASSESS-REVERSAL
               PERFORM 3860-DESCRIBE-CHANNEL
               PERFORM 3870-FLAG-MIN-DUE
               PERFORM 3880-ACCUM-TRAN
               PERFORM 3890-SELECT-RATE
               PERFORM 3900-COUNT-ITEM
               PERFORM 3910-EDIT-MEMO
               PERFORM 3920-ACCUM-CHANNEL
               PERFORM 3930-COUNT-NOTICE
               PERFORM 3940-ASSESS-OVERRIDE
               PERFORM 3950-ACCUM-BRANCH
               PERFORM 3960-SELECT-RATE
               PERFORM 3970-ASSESS-PREFERRED
               PERFORM 3980-PRORATE-FINANCE
               PERFORM 3990-LIMIT-LIMIT
               PERFORM 4000-ACCUM-STATUS
               PERFORM 4010-COUNT-SEQ
               PERFORM 4020-ASSESS-STOP
               PERFORM 4030-ASSESS-PREFERRED
               PERFORM 4040-SET-STATUS
               PERFORM 4050-SELECT-PENALTY-PCT
               PERFORM 4060-ACCUM-BILL-CYCLE
               PERFORM 4070-EDIT-CONTACT
               PERFORM 4080-SET-CHANNEL
               PERFORM 4090-COUNT-CYCLE
               PERFORM 4100-ASSESS-REVERSAL
               PERFORM 4110-EDIT-CUST-ID
               PERFORM 4120-ASSESS-PREFERRED
               PERFORM 4130-COUNT-CYCLE
               PERFORM 4140-PRORATE-LIMIT
               PERFORM 4150-SET-CARD-TYPE
               PERFORM 4160-SPLIT-PAYMENT
               PERFORM 4170-DESCRIBE-BILL-CYCLE
               PERFORM 4180-ASSESS-AUTO-RENEW
               PERFORM 4190-COUNT-SEQ
               PERFORM 4200-ACCRUE-CASH-ADV
               PERFORM 4210-SELECT-DISCOUNT
               PERFORM 4220-LIMIT-LATE-FEE
               PERFORM 4230-FLAG-PURCHASE
               PERFORM 4240-COUNT-CYCLE
               PERFORM 4250-EDIT-REF-NO
               PERFORM 4260-FLAG-FINANCE
               PERFORM 4270-ACCRUE-FINANCE
               PERFORM 4280-ACCUM-CHANNEL
               PERFORM 4290-SET-BRANCH
               PERFORM 4300-DESCRIBE-TYPE
               PERFORM 4310-FLAG-ITEM
               PERFORM 4320-SPLIT-LIMIT
               PERFORM 4330-ACCUM-TRAN
               PERFORM 4340-SELECT-PENALTY-PCT
               PERFORM 4350-ASSESS-AUTO-RENEW
               PERFORM 4360-ACCRUE-PAYMENT
               PERFORM 4370-AGE-NEXT-REVIEW
               PERFORM 4380-FLAG-LIMIT
               PERFORM 4390-CHECK-MATURITY
               PERFORM 4400-ACCUM-CARD-TYPE
               PERFORM 4410-FLAG-CASH-ADV
               PERFORM 4420-FLAG-PAYMENT
               PERFORM 4430-ACCRUE-DISCOUNT
               PERFORM 4440-DESCRIBE-BRANCH
               PERFORM 4450-AGE-NEXT-REVIEW
               PERFORM 4460-DESCRIBE-STATUS
               PERFORM 4470-ACCRUE-MIN-DUE
               PERFORM 4480-SPLIT-LIMIT
               PERFORM 4490-ASSESS-PREFERRED
               PERFORM 4500-ASSESS-AUTO-RENEW
               PERFORM 4510-SET-TRAN
               PERFORM 4520-DESCRIBE-BRANCH
               PERFORM 4530-SPLIT-CASH-ADV
               PERFORM 4540-COUNT-CYCLE
               PERFORM 4550-PRORATE-PAYMENT
               PERFORM 4560-LIMIT-CASH-ADV
               PERFORM 4570-DESCRIBE-BRANCH
               PERFORM 4580-SELECT-RATE
               PERFORM 4590-LIMIT-FINANCE
               PERFORM 4600-DESCRIBE-BILL-CYCLE
               PERFORM 4610-SELECT-DISCOUNT
               PERFORM 4620-SELECT-RATE
               PERFORM 4630-SPLIT-PURCHASE
               PERFORM 4640-COUNT-RETRY
               PERFORM 4650-LIMIT-PAYMENT
               PERFORM 4660-COUNT-NOTICE
               PERFORM 4670-COUNT-CYCLE
               PERFORM 4680-EDIT-DESC
               PERFORM 4690-FLAG-MIN-DUE
               PERFORM 4700-CHECK-LAST-ACT
               PERFORM 4710-SELECT-DISCOUNT
               PERFORM 4720-SPLIT-LATE-FEE
               PERFORM 4730-AGE-MATURITY
               PERFORM 4740-EDIT-OPERATOR
               PERFORM 7040-CALL-CRCAL
               PERFORM 7020-CALL-CRDTE
               REWRITE CRC-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'CRCMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM CRC-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * SPLIT LIMIT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRCM-LIMIT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CRCM-LIMIT-YTD - (WS-WORK-AMT * 12)
           COMPUTE CRCM-LIMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * GP -> UV WHEN CLOSE DATE IS PAST
      * UV -> YK WHEN CLOSE DATE IS PAST
      * YK -> GP WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-TYPE.
           EVALUATE CRCM-TYPE-CD
               WHEN 'GP'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YK' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YK'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GP' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E661 OPERATOR REQUIRED
      * E473 CASH ADV YTD NEGATIVE
      * E716 CASH ADV YTD OVER 50000
      * E175 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3020-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCT-OPERATOR = SPACES
                   MOVE 'E661' TO WS-REJECT-CODE
               WHEN CRCM-CASH-ADV-YTD < 0
                   MOVE 'E473' TO WS-REJECT-CODE
               WHEN CRCM-CASH-ADV-YTD > 50000.00
                   MOVE 'E716' TO WS-REJECT-CODE
               WHEN CRCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E175' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CASH ADV YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3030-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CRCM-CASH-ADV-YTD *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCM-TYPE-CD = 'GP'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN CRCM-TYPE-CD = 'UV'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CRCM-CASH-ADV-YTD *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 5
               MOVE 'Y' TO CRCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN EFFECTIVE DATE IS PAST
      * DDY -> DPB WHEN EFFECTIVE DATE IS PAST
      * DPB -> UHF WHEN EFFECTIVE DATE IS PAST
      * UHF -> WLL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E539 DESC REQUIRED
      * E649 LATE FEE YTD NEGATIVE
      * E206 LATE FEE YTD OVER 99999
      * E977 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3060-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-DESC = SPACES
                   MOVE 'E539' TO WS-REJECT-CODE
               WHEN CRCM-LATE-FEE-YTD < 0
                   MOVE 'E649' TO WS-REJECT-CODE
               WHEN CRCM-LATE-FEE-YTD > 99999.00
                   MOVE 'E206' TO WS-REJECT-CODE
               WHEN CRCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E977' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CRCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN DUE GRID TIER FOR THE FINANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-DUE-GRID-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CRCM-FINANCE-YTD NOT < WS-MIN-DUE-GRID-LOW(WS-TX)
                   MOVE WS-MIN-DUE-GRID-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-SOURCE-CD
                   ADD CRCM-LIMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-LIMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3110-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-BRANCH-CD
                   ADD CRCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PU -> CA WHEN NEXT REVIEW DATE IS PAST
      * CA -> PY WHEN NEXT REVIEW DATE IS PAST
      * PY -> FC WHEN NEXT REVIEW DATE IS PAST
      * FC -> LF WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-TRAN.
           EVALUATE CRCT-TRAN-CD
               WHEN 'PU'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'CA' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CA'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'PY' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'FC' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LF' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN DUE YTD.
      *----------------------------------------------------------------*
       3130-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-MIN-DUE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-MIN-DUE-YTD
           END-IF
           ADD CRCM-MIN-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE CASH ADV AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3140-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRCT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE CRCT-PAYMENT-AMT ROUNDED = CRCT-CASH-ADV-AMT
                       * CRCM-PENALTY-PCT / 200
               WHEN CRCM-BILL-CYCLE-CD = 'B'
                   MOVE 12.00 TO CRCT-PAYMENT-AMT
               WHEN CRCM-BILL-CYCLE-CD = 'D'
                   MOVE 10.00 TO CRCT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRCT-PAYMENT-AMT ROUNDED = CRCT-CASH-ADV-AMT
                       * CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCT-PAYMENT-AMT > 0 AND CRCT-PAYMENT-AMT < 2.00
               MOVE 2.00 TO CRCT-PAYMENT-AMT
           END-IF
           IF CRCT-PAYMENT-AMT > 25.00
               MOVE 25.00 TO CRCT-PAYMENT-AMT
           END-IF
           ADD CRCT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ADV YTD FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3150-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CRCM-CASH-ADV-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE CRCM-CASH-ADV-YTD ROUNDED = WS-WORK-AMT *
                       CRCM-DISCOUNT / 200
               WHEN CRCM-BILL-CYCLE-CD = 'B'
                   MOVE 5.00 TO CRCM-CASH-ADV-YTD
               WHEN CRCM-BILL-CYCLE-CD = 'D'
                   MOVE 5.00 TO CRCM-CASH-ADV-YTD
               WHEN OTHER
                   COMPUTE CRCM-CASH-ADV-YTD ROUNDED = WS-WORK-AMT *
                       CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCM-CASH-ADV-YTD > 0 AND CRCM-CASH-ADV-YTD < 2.00
               MOVE 2.00 TO CRCM-CASH-ADV-YTD
           END-IF
           IF CRCM-CASH-ADV-YTD > 50.00
               MOVE 50.00 TO CRCM-CASH-ADV-YTD
           END-IF
           ADD CRCM-CASH-ADV-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3160-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LIMIT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRCM-LIMIT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CRCM-LIMIT-YTD - (WS-WORK-AMT * 12)
           COMPUTE CRCM-CASH-ADV-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3180-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 3
               MOVE 'Y' TO CRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E502 ADDR REQUIRED
      * E041 FINANCE YTD NEGATIVE
      * E957 FINANCE YTD OVER 250000
      * E697 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3190-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-ADDR = SPACES
                   MOVE 'E502' TO WS-REJECT-CODE
               WHEN CRCM-FINANCE-YTD < 0
                   MOVE 'E041' TO WS-REJECT-CODE
               WHEN CRCM-FINANCE-YTD > 250000.00
                   MOVE 'E957' TO WS-REJECT-CODE
               WHEN CRCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E697' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MIN DUE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3200-SPLIT-MIN-DUE.
           COMPUTE WS-WORK-AMT = CRCM-MIN-DUE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CRCM-MIN-DUE-YTD - (WS-WORK-AMT * 4)
           COMPUTE CRCM-FINANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN DUE GRID TIER FOR THE LIMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-DUE-GRID-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CRCM-LIMIT-YTD NOT < WS-MIN-DUE-GRID-LOW(WS-TX)
                   MOVE WS-MIN-DUE-GRID-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       3220-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-LIMIT-AMT ROUNDED =
                   CRCM-PURCHASE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-LIMIT-AMT
           END-IF
           ADD CRCM-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CASH ADV AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3230-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = CRCT-CASH-ADV-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CRCT-CASH-ADV-AMT - (WS-WORK-AMT *
               12)
           COMPUTE CRCT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MIN DUE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3240-FLAG-MIN-DUE.
           IF CRCM-MIN-DUE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCM-MIN-DUE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FINANCE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3250-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCT-CHANNEL-CD
                   ADD CRCM-FINANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-FINANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE PAYMENT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3270-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CRCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   COMPUTE CRCM-PAYMENT-AMT ROUNDED = CRCT-PAYMENT-AMT *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCM-TYPE-CD = 'GP'
                   MOVE 7.00 TO CRCM-PAYMENT-AMT
               WHEN CRCM-TYPE-CD = 'UV'
                   MOVE 7.00 TO CRCM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRCM-PAYMENT-AMT ROUNDED = CRCT-PAYMENT-AMT *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-PAYMENT-AMT > 0 AND CRCM-PAYMENT-AMT < 1.00
               MOVE 1.00 TO CRCM-PAYMENT-AMT
           END-IF
           IF CRCM-PAYMENT-AMT > 150.00
               MOVE 150.00 TO CRCM-PAYMENT-AMT
           END-IF
           ADD CRCM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE MIN DUE GRID TIER FOR THE LIMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-DUE-GRID-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CRCT-LIMIT-AMT NOT < WS-MIN-DUE-GRID-LOW(WS-TX)
                   MOVE WS-MIN-DUE-GRID-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3290-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRCM-LATE-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CRCM-LATE-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE CRCM-LIMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3300-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-TRAN.
           EVALUATE CRCT-TRAN-CD
               WHEN 'PU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'CA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT LIMIT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3320-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRCT-LIMIT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CRCT-LIMIT-AMT - (WS-WORK-AMT * 12)
           COMPUTE CRCM-CASH-ADV-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3330-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-SOURCE-CD
                   ADD CRCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E997 ADDR REQUIRED
      * E151 WORK AMT 2 NEGATIVE
      * E876 WORK AMT 2 OVER 250000
      * E733 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3340-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-ADDR = SPACES
                   MOVE 'E997' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E151' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 250000.00
                   MOVE 'E876' TO WS-REJECT-CODE
               WHEN CRCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E733' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3350-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN POST DATE IS PAST
      * DDY -> DPB WHEN POST DATE IS PAST
      * DPB -> UHF WHEN POST DATE IS PAST
      * UHF -> WLL WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3360-SET-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   IF CRCT-POST-DATE > 0 AND CRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRCT-POST-DATE > 0 AND CRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRCT-POST-DATE > 0 AND CRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRCT-POST-DATE > 0 AND CRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3370-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FINANCE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3380-LIMIT-FINANCE.
           IF CRCM-FINANCE-AMT < 50.00
               MOVE 50.00 TO CRCM-FINANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-FINANCE-AMT > 10000.00
               MOVE 10000.00 TO CRCM-FINANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3390-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-STATUS-CD
                   ADD CRCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-SEQ.
           IF CRCT-SEQ < 9999999
               ADD 1 TO CRCT-SEQ
           END-IF
           IF CRCT-SEQ NOT < 6
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3410-COUNT-CYCLE.
           IF CRCM-CYCLE-CNT < 9999999
               ADD 1 TO CRCM-CYCLE-CNT
           END-IF
           IF CRCM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3420-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCT-PAYMENT-AMT ROUNDED =
                   CRCM-PURCHASE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCT-PAYMENT-AMT
           END-IF
           ADD CRCT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE CASH ADV AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       3430-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-PAYMENT-YTD ROUNDED =
                   CRCT-CASH-ADV-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-PAYMENT-YTD
           END-IF
           ADD CRCM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE MIN DUE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LIMIT YTD.
      *----------------------------------------------------------------*
       3440-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-LIMIT-YTD ROUNDED =
                   CRCM-MIN-DUE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-LIMIT-YTD
           END-IF
           ADD CRCM-LIMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3450-DESCRIBE-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCT-LIMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCT-LIMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CRCT-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD CRCT-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3470-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FINANCE YTD FEE ON THE LATE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3480-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CRCM-FINANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE CRCM-FINANCE-YTD ROUNDED = CRCM-LATE-FEE-AMT
                       * CRCM-PENALTY-PCT / 200
               WHEN CRCM-SOURCE-CD = 'C'
                   MOVE 20.00 TO CRCM-FINANCE-YTD
               WHEN CRCM-SOURCE-CD = 'H'
                   MOVE 7.00 TO CRCM-FINANCE-YTD
               WHEN OTHER
                   COMPUTE CRCM-FINANCE-YTD ROUNDED = CRCM-LATE-FEE-AMT
                       * CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-FINANCE-YTD > 0 AND CRCM-FINANCE-YTD < 2.00
               MOVE 2.00 TO CRCM-FINANCE-YTD
           END-IF
           IF CRCM-FINANCE-YTD > 150.00
               MOVE 150.00 TO CRCM-FINANCE-YTD
           END-IF
           ADD CRCM-FINANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3490-FLAG-PAYMENT.
           IF CRCM-PAYMENT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3510-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE CRCM-CASH-ADV-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MIN DUE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3520-FLAG-MIN-DUE.
           IF CRCT-MIN-DUE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCT-MIN-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3530-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-LIMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-LIMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH ADV YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3540-LIMIT-CASH-ADV.
           IF CRCM-CASH-ADV-YTD < 50.00
               MOVE 50.00 TO CRCM-CASH-ADV-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-CASH-ADV-YTD > 2500.00
               MOVE 2500.00 TO CRCM-CASH-ADV-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3550-COUNT-ITEM.
           IF CRCM-ITEM-CNT < 9999999
               ADD 1 TO CRCM-ITEM-CNT
           END-IF
           IF CRCM-ITEM-CNT NOT < 12
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3560-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3570-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = CRCT-PAYMENT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CRCT-PAYMENT-AMT - (WS-WORK-AMT * 12)
           COMPUTE CRCT-MIN-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3580-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CRCM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN DUE GRID TIER FOR THE LATE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-DUE-GRID-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CRCM-LATE-FEE-AMT NOT < WS-MIN-DUE-GRID-LOW(WS-TX)
                   MOVE WS-MIN-DUE-GRID-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 3
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MIN DUE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3610-FLAG-MIN-DUE.
           IF CRCT-MIN-DUE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCT-MIN-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PURCHASE YTD.
      *----------------------------------------------------------------*
       3620-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-PURCHASE-YTD ROUNDED =
                   CRCM-LIMIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-PURCHASE-YTD
           END-IF
           ADD CRCM-PURCHASE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE LIMIT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3630-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CRCT-LIMIT-AMT *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCM-STATUS-CD = 'BAL'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN CRCM-STATUS-CD = 'DDY'
                   MOVE 12.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CRCT-LIMIT-AMT *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 25.00
               MOVE 25.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3640-LIMIT-PURCHASE.
           IF CRCM-PURCHASE-AMT < 25.00
               MOVE 25.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-PURCHASE-AMT > 1000.00
               MOVE 1000.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3650-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E085 DESC REQUIRED
      * E350 LATE FEE YTD NEGATIVE
      * E769 LATE FEE YTD OVER 10000
      * E409 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-DESC = SPACES
                   MOVE 'E085' TO WS-REJECT-CODE
               WHEN CRCM-LATE-FEE-YTD < 0
                   MOVE 'E350' TO WS-REJECT-CODE
               WHEN CRCM-LATE-FEE-YTD > 10000.00
                   MOVE 'E769' TO WS-REJECT-CODE
               WHEN CRCM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E409' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E555 REF NO REQUIRED
      * E306 PURCHASE YTD NEGATIVE
      * E456 PURCHASE YTD OVER 25000
      * E077 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3670-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-REF-NO = SPACES
                   MOVE 'E555' TO WS-REJECT-CODE
               WHEN CRCM-PURCHASE-YTD < 0
                   MOVE 'E306' TO WS-REJECT-CODE
               WHEN CRCM-PURCHASE-YTD > 25000.00
                   MOVE 'E456' TO WS-REJECT-CODE
               WHEN CRCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E077' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E396 CUST ID REQUIRED
      * E763 LIMIT YTD NEGATIVE
      * E977 LIMIT YTD OVER 99999
      * E155 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CUST-ID = SPACES
                   MOVE 'E396' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-YTD < 0
                   MOVE 'E763' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-YTD > 99999.00
                   MOVE 'E977' TO WS-REJECT-CODE
               WHEN CRCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E155' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FINANCE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3690-SPLIT-FINANCE.
           COMPUTE WS-WORK-AMT = CRCM-FINANCE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CRCM-FINANCE-YTD - (WS-WORK-AMT * 3)
           COMPUTE CRCT-CASH-ADV-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MIN DUE AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3710-LIMIT-MIN-DUE.
           IF CRCT-MIN-DUE-AMT < 0.00
               MOVE 0.00 TO CRCT-MIN-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCT-MIN-DUE-AMT > 5000.00
               MOVE 5000.00 TO CRCT-MIN-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E060 REF NO REQUIRED
      * E228 PAYMENT AMT NEGATIVE
      * E654 PAYMENT AMT OVER 1000000
      * E355 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-REF-NO = SPACES
                   MOVE 'E060' TO WS-REJECT-CODE
               WHEN CRCT-PAYMENT-AMT < 0
                   MOVE 'E228' TO WS-REJECT-CODE
               WHEN CRCT-PAYMENT-AMT > 1000000.00
                   MOVE 'E654' TO WS-REJECT-CODE
               WHEN CRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E355' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-SEQ.
           IF CRCT-SEQ < 9999999
               ADD 1 TO CRCT-SEQ
           END-IF
           IF CRCT-SEQ NOT < 3
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3740-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF CRCT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO CRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE TIER TIER FOR THE FINANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3750-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-TIER-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCM-FINANCE-YTD NOT < WS-PURCHASE-TIER-LOW(WS-TX)
                   MOVE WS-PURCHASE-TIER-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3760-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LNX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MGS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YNJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-TYPE-CD
                   ADD CRCM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ADV YTD FEE ON THE PURCHASE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3790-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CRCM-CASH-ADV-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE CRCM-CASH-ADV-YTD ROUNDED = CRCM-PURCHASE-AMT
                       * CRCM-RATE / 200
               WHEN CRCM-TYPE-CD = 'GP'
                   MOVE 12.00 TO CRCM-CASH-ADV-YTD
               WHEN CRCM-TYPE-CD = 'UV'
                   MOVE 20.00 TO CRCM-CASH-ADV-YTD
               WHEN OTHER
                   COMPUTE CRCM-CASH-ADV-YTD ROUNDED = CRCM-PURCHASE-AMT
                       * CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-CASH-ADV-YTD > 0 AND CRCM-CASH-ADV-YTD < 1.00
               MOVE 1.00 TO CRCM-CASH-ADV-YTD
           END-IF
           IF CRCM-CASH-ADV-YTD > 50.00
               MOVE 50.00 TO CRCM-CASH-ADV-YTD
           END-IF
           ADD CRCM-CASH-ADV-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE LATE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CRCM-LATE-FEE-YTD NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BILL CYCLE CD:
      * B -> D WHEN LAST ACT DATE IS PAST
      * D -> E WHEN LAST ACT DATE IS PAST
      * E -> F WHEN LAST ACT DATE IS PAST
      * F -> H WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ADV AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3820-ACCRUE-CASH-ADV.
           IF CRCT-CASH-ADV-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCT-CASH-ADV-AMT * CRCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO CRCM-PURCHASE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * GP -> UV WHEN CLOSE DATE IS PAST
      * UV -> YK WHEN CLOSE DATE IS PAST
      * YK -> GP WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3830-SET-TYPE.
           EVALUATE CRCM-TYPE-CD
               WHEN 'GP'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YK' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YK'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GP' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-SEQ.
           IF CRCT-SEQ < 9999999
               ADD 1 TO CRCT-SEQ
           END-IF
           IF CRCT-SEQ NOT < 10
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT FEE ON THE LATE FEE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3850-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CRCM-LIMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CRCM-LIMIT-AMT ROUNDED = CRCM-LATE-FEE-AMT *
                       CRCM-RATE / 200
               WHEN CRCT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO CRCM-LIMIT-AMT
               WHEN CRCT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO CRCM-LIMIT-AMT
               WHEN OTHER
                   COMPUTE CRCM-LIMIT-AMT ROUNDED = CRCM-LATE-FEE-AMT *
                       CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-LIMIT-AMT > 0 AND CRCM-LIMIT-AMT < 1.00
               MOVE 1.00 TO CRCM-LIMIT-AMT
           END-IF
           IF CRCM-LIMIT-AMT > 25.00
               MOVE 25.00 TO CRCM-LIMIT-AMT
           END-IF
           ADD CRCM-LIMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3860-DESCRIBE-CHANNEL.
           EVALUATE CRCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MIN DUE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3870-FLAG-MIN-DUE.
           IF CRCT-MIN-DUE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCT-MIN-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3880-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCT-TRAN-CD
                   ADD CRCM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE LATE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3890-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CRCM-LATE-FEE-YTD NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3900-COUNT-ITEM.
           IF CRCM-ITEM-CNT < 9999999
               ADD 1 TO CRCM-ITEM-CNT
           END-IF
           IF CRCM-ITEM-CNT NOT < 3
               MOVE 'Y' TO CRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E982 MEMO REQUIRED
      * E839 PAYMENT YTD NEGATIVE
      * E375 PAYMENT YTD OVER 50000
      * E013 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCT-MEMO = SPACES
                   MOVE 'E982' TO WS-REJECT-CODE
               WHEN CRCM-PAYMENT-YTD < 0
                   MOVE 'E839' TO WS-REJECT-CODE
               WHEN CRCM-PAYMENT-YTD > 50000.00
                   MOVE 'E375' TO WS-REJECT-CODE
               WHEN CRCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E013' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN DUE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3920-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCT-CHANNEL-CD
                   ADD CRCM-MIN-DUE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-MIN-DUE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3930-COUNT-NOTICE.
           IF CRCM-NOTICE-CNT < 9999999
               ADD 1 TO CRCM-NOTICE-CNT
           END-IF
           IF CRCM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE FEE ON THE LATE FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3940-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PURCHASE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   COMPUTE CRCM-PURCHASE-AMT ROUNDED = CRCM-LATE-FEE-AMT
                       * CRCM-PENALTY-PCT / 200
               WHEN CRCT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO CRCM-PURCHASE-AMT
               WHEN CRCT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO CRCM-PURCHASE-AMT
               WHEN OTHER
                   COMPUTE CRCM-PURCHASE-AMT ROUNDED = CRCM-LATE-FEE-AMT
                       * CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-PURCHASE-AMT > 0 AND CRCM-PURCHASE-AMT < 5.00
               MOVE 5.00 TO CRCM-PURCHASE-AMT
           END-IF
           IF CRCM-PURCHASE-AMT > 35.00
               MOVE 35.00 TO CRCM-PURCHASE-AMT
           END-IF
           ADD CRCM-PURCHASE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT YTD BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3950-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-BRANCH-CD
                   ADD CRCM-LIMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-LIMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE PURCHASE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CRCM-PURCHASE-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE CASH ADV YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3970-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE CRCM-PAYMENT-AMT ROUNDED = CRCM-CASH-ADV-YTD
                       * CRCM-RATE / 200
               WHEN CRCM-CARD-TYPE-CD = 'PGU'
                   MOVE 3.00 TO CRCM-PAYMENT-AMT
               WHEN CRCM-CARD-TYPE-CD = 'TRN'
                   MOVE 15.00 TO CRCM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRCM-PAYMENT-AMT ROUNDED = CRCM-CASH-ADV-YTD
                       * CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-PAYMENT-AMT > 0 AND CRCM-PAYMENT-AMT < 2.00
               MOVE 2.00 TO CRCM-PAYMENT-AMT
           END-IF
           IF CRCM-PAYMENT-AMT > 250.00
               MOVE 250.00 TO CRCM-PAYMENT-AMT
           END-IF
           ADD CRCM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FINANCE AMT.
      *----------------------------------------------------------------*
       3980-PRORATE-FINANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-FINANCE-AMT ROUNDED =
                   CRCM-PURCHASE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-FINANCE-AMT
           END-IF
           ADD CRCM-FINANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * LIMIT YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3990-LIMIT-LIMIT.
           IF CRCM-LIMIT-YTD < 0.00
               MOVE 0.00 TO CRCM-LIMIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-LIMIT-YTD > 2500.00
               MOVE 2500.00 TO CRCM-LIMIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ADV AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4000-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-STATUS-CD
                   ADD CRCT-CASH-ADV-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCT-CASH-ADV-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4010-COUNT-SEQ.
           IF CRCT-SEQ < 9999999
               ADD 1 TO CRCT-SEQ
           END-IF
           IF CRCT-SEQ NOT < 3
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE FEE ON THE PURCHASE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4020-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CRCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO CRCM-MIN-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   COMPUTE CRCM-MIN-DUE-AMT ROUNDED = CRCM-PURCHASE-YTD
                       * CRCM-PENALTY-PCT / 200
               WHEN CRCM-SOURCE-CD = 'C'
                   MOVE 15.00 TO CRCM-MIN-DUE-AMT
               WHEN CRCM-SOURCE-CD = 'H'
                   MOVE 15.00 TO CRCM-MIN-DUE-AMT
               WHEN OTHER
                   COMPUTE CRCM-MIN-DUE-AMT ROUNDED = CRCM-PURCHASE-YTD
                       * CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-MIN-DUE-AMT > 0 AND CRCM-MIN-DUE-AMT < 2.00
               MOVE 2.00 TO CRCM-MIN-DUE-AMT
           END-IF
           IF CRCM-MIN-DUE-AMT > 35.00
               MOVE 35.00 TO CRCM-MIN-DUE-AMT
           END-IF
           ADD CRCM-MIN-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ADV FEE ON THE CASH ADV AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4030-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRCT-CASH-ADV-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CRCT-CASH-ADV-AMT ROUNDED = CRCM-CASH-ADV-AMT
                       * CRCM-DISCOUNT / 200
               WHEN CRCM-BRANCH-CD = 'CTB'
                   MOVE 7.00 TO CRCT-CASH-ADV-AMT
               WHEN CRCM-BRANCH-CD = 'LNX'
                   MOVE 5.00 TO CRCT-CASH-ADV-AMT
               WHEN OTHER
                   COMPUTE CRCT-CASH-ADV-AMT ROUNDED = CRCM-CASH-ADV-AMT
                       * CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCT-CASH-ADV-AMT > 0 AND CRCT-CASH-ADV-AMT < 5.00
               MOVE 5.00 TO CRCT-CASH-ADV-AMT
           END-IF
           IF CRCT-CASH-ADV-AMT > 75.00
               MOVE 75.00 TO CRCT-CASH-ADV-AMT
           END-IF
           ADD CRCT-CASH-ADV-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN POST DATE IS PAST
      * DDY -> DPB WHEN POST DATE IS PAST
      * DPB -> UHF WHEN POST DATE IS PAST
      * UHF -> WLL WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4040-SET-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   IF CRCT-POST-DATE > 0 AND CRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRCT-POST-DATE > 0 AND CRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRCT-POST-DATE > 0 AND CRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRCT-POST-DATE > 0 AND CRCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE TIER TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4050-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-TIER-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCM-PAYMENT-YTD NOT < WS-PURCHASE-TIER-LOW(WS-TX)
                   MOVE WS-PURCHASE-TIER-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE YTD BY BILL CYCLE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4060-ACCUM-BILL-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-BILL-CYCLE-CD
                   ADD CRCM-PURCHASE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-BILL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-PURCHASE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E854 CONTACT REQUIRED
      * E464 MIN DUE AMT NEGATIVE
      * E952 MIN DUE AMT OVER 5000
      * E524 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4070-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CONTACT = SPACES
                   MOVE 'E854' TO WS-REJECT-CODE
               WHEN CRCT-MIN-DUE-AMT < 0
                   MOVE 'E464' TO WS-REJECT-CODE
               WHEN CRCT-MIN-DUE-AMT > 5000.00
                   MOVE 'E952' TO WS-REJECT-CODE
               WHEN CRCM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E524' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NOTICE DATE IS PAST
      * BAT -> MAL WHEN NOTICE DATE IS PAST
      * MAL -> TEL WHEN NOTICE DATE IS PAST
      * TEL -> ONL WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4080-SET-CHANNEL.
           EVALUATE CRCT-CHANNEL-CD
               WHEN 'BRN'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-CYCLE.
           IF CRCM-CYCLE-CNT < 9999999
               ADD 1 TO CRCM-CYCLE-CNT
           END-IF
           IF CRCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE WORK AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4100-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CRCM-PAYMENT-AMT ROUNDED = WS-WORK-AMT *
                       CRCM-RATE / 200
               WHEN CRCT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO CRCM-PAYMENT-AMT
               WHEN CRCT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO CRCM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRCM-PAYMENT-AMT ROUNDED = WS-WORK-AMT *
                       CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-PAYMENT-AMT > 0 AND CRCM-PAYMENT-AMT < 2.00
               MOVE 2.00 TO CRCM-PAYMENT-AMT
           END-IF
           IF CRCM-PAYMENT-AMT > 100.00
               MOVE 100.00 TO CRCM-PAYMENT-AMT
           END-IF
           ADD CRCM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E410 CUST ID REQUIRED
      * E002 LIMIT AMT NEGATIVE
      * E660 LIMIT AMT OVER 250000
      * E828 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4110-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CUST-ID = SPACES
                   MOVE 'E410' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-AMT < 0
                   MOVE 'E002' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-AMT > 250000.00
                   MOVE 'E660' TO WS-REJECT-CODE
               WHEN CRCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E828' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE FEE ON THE MIN DUE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4120-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRCM-MIN-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   COMPUTE CRCM-MIN-DUE-AMT ROUNDED = CRCT-MIN-DUE-AMT *
                       CRCM-RATE / 200
               WHEN CRCM-BRANCH-CD = 'CTB'
                   MOVE 12.00 TO CRCM-MIN-DUE-AMT
               WHEN CRCM-BRANCH-CD = 'LNX'
                   MOVE 15.00 TO CRCM-MIN-DUE-AMT
               WHEN OTHER
                   COMPUTE CRCM-MIN-DUE-AMT ROUNDED = CRCT-MIN-DUE-AMT *
                       CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-MIN-DUE-AMT > 0 AND CRCM-MIN-DUE-AMT < 10.00
               MOVE 10.00 TO CRCM-MIN-DUE-AMT
           END-IF
           IF CRCM-MIN-DUE-AMT > 35.00
               MOVE 35.00 TO CRCM-MIN-DUE-AMT
           END-IF
           ADD CRCM-MIN-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4130-COUNT-CYCLE.
           IF CRCM-CYCLE-CNT < 9999999
               ADD 1 TO CRCM-CYCLE-CNT
           END-IF
           IF CRCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LIMIT YTD.
      *----------------------------------------------------------------*
       4140-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-LIMIT-YTD ROUNDED =
                   CRCM-LATE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-LIMIT-YTD
           END-IF
           ADD CRCM-LIMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARD TYPE CD:
      * PGU -> TRN WHEN OPEN DATE IS PAST
      * TRN -> WHR WHEN OPEN DATE IS PAST
      * WHR -> ZWB WHEN OPEN DATE IS PAST
      * ZWB -> PGU WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4150-SET-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TRN' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TRN'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WHR' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WHR'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWB' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZWB'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PGU' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4160-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = CRCM-PAYMENT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CRCM-PAYMENT-AMT - (WS-WORK-AMT * 2)
           COMPUTE CRCM-CASH-ADV-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BILL CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4170-DESCRIBE-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'E'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'P'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE FEE ON THE PURCHASE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4180-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CRCT-MIN-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CRCT-MIN-DUE-AMT ROUNDED = CRCM-PURCHASE-AMT
                       * CRCM-RATE / 200
               WHEN CRCM-STATUS-CD = 'BAL'
                   MOVE 5.00 TO CRCT-MIN-DUE-AMT
               WHEN CRCM-STATUS-CD = 'DDY'
                   MOVE 20.00 TO CRCT-MIN-DUE-AMT
               WHEN OTHER
                   COMPUTE CRCT-MIN-DUE-AMT ROUNDED = CRCM-PURCHASE-AMT
                       * CRCM-RATE / 100
           END-EVALUATE
           IF CRCT-MIN-DUE-AMT > 0 AND CRCT-MIN-DUE-AMT < 2.00
               MOVE 2.00 TO CRCT-MIN-DUE-AMT
           END-IF
           IF CRCT-MIN-DUE-AMT > 150.00
               MOVE 150.00 TO CRCT-MIN-DUE-AMT
           END-IF
           ADD CRCT-MIN-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4190-COUNT-SEQ.
           IF CRCT-SEQ < 9999999
               ADD 1 TO CRCT-SEQ
           END-IF
           IF CRCT-SEQ NOT < 3
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ADV AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4200-ACCRUE-CASH-ADV.
           IF CRCM-CASH-ADV-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-CASH-ADV-AMT * CRCM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN DUE GRID TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4210-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-DUE-GRID-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CRCM-PAYMENT-YTD NOT < WS-MIN-DUE-GRID-LOW(WS-TX)
                   MOVE WS-MIN-DUE-GRID-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4220-LIMIT-LATE-FEE.
           IF CRCM-LATE-FEE-YTD < 100.00
               MOVE 100.00 TO CRCM-LATE-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-LATE-FEE-YTD > 1000.00
               MOVE 1000.00 TO CRCM-LATE-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4230-FLAG-PURCHASE.
           IF CRCM-PURCHASE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-PURCHASE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4240-COUNT-CYCLE.
           IF CRCM-CYCLE-CNT < 9999999
               ADD 1 TO CRCM-CYCLE-CNT
           END-IF
           IF CRCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CRCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E029 REF NO REQUIRED
      * E006 LIMIT YTD NEGATIVE
      * E693 LIMIT YTD OVER 99999
      * E475 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4250-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-REF-NO = SPACES
                   MOVE 'E029' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-YTD < 0
                   MOVE 'E006' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-YTD > 99999.00
                   MOVE 'E693' TO WS-REJECT-CODE
               WHEN CRCM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FINANCE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4260-FLAG-FINANCE.
           IF CRCM-FINANCE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCM-FINANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FINANCE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4270-ACCRUE-FINANCE.
           IF CRCM-FINANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-FINANCE-YTD * CRCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CRCM-LIMIT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN DUE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4280-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCT-CHANNEL-CD
                   ADD CRCM-MIN-DUE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-MIN-DUE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CTB -> LNX WHEN CLOSE DATE IS PAST
      * LNX -> MGS WHEN CLOSE DATE IS PAST
      * MGS -> YNJ WHEN CLOSE DATE IS PAST
      * YNJ -> CTB WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4290-SET-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LNX' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LNX'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MGS' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MGS'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YNJ' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YNJ'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CTB' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4300-DESCRIBE-TYPE.
           EVALUATE CRCM-TYPE-CD
               WHEN 'GP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4310-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LIMIT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4320-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRCM-LIMIT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CRCM-LIMIT-YTD - (WS-WORK-AMT * 2)
           COMPUTE CRCT-MIN-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4330-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCT-TRAN-CD
                   ADD CRCT-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCT-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4340-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CRCT-PAYMENT-AMT NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE LIMIT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4350-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   COMPUTE CRCM-PAYMENT-AMT ROUNDED = CRCT-LIMIT-AMT *
                       CRCM-RATE / 200
               WHEN CRCM-TYPE-CD = 'GP'
                   MOVE 7.00 TO CRCM-PAYMENT-AMT
               WHEN CRCM-TYPE-CD = 'UV'
                   MOVE 3.00 TO CRCM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRCM-PAYMENT-AMT ROUNDED = CRCT-LIMIT-AMT *
                       CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-PAYMENT-AMT > 0 AND CRCM-PAYMENT-AMT < 2.00
               MOVE 2.00 TO CRCM-PAYMENT-AMT
           END-IF
           IF CRCM-PAYMENT-AMT > 75.00
               MOVE 75.00 TO CRCM-PAYMENT-AMT
           END-IF
           ADD CRCM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4360-ACCRUE-PAYMENT.
           IF CRCM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PAYMENT-YTD * CRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CRCM-MIN-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4370-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CRCM-NEXT-REVIEW-DATE > 0 AND CRCM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4380-FLAG-LIMIT.
           IF CRCM-LIMIT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCM-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4390-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ADV YTD BY CARD TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4400-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-CARD-TYPE-CD
                   ADD CRCM-CASH-ADV-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-CASH-ADV-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH ADV YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4410-FLAG-CASH-ADV.
           IF CRCM-CASH-ADV-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCM-CASH-ADV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4420-FLAG-PAYMENT.
           IF CRCT-PAYMENT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4430-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CRCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-PURCHASE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4440-DESCRIBE-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LNX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MGS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YNJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4450-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CRCM-NEXT-REVIEW-DATE > 0 AND CRCM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-MIN-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4460-DESCRIBE-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN DUE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4470-ACCRUE-MIN-DUE.
           IF CRCM-MIN-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-MIN-DUE-AMT * CRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LIMIT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4480-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRCM-LIMIT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CRCM-LIMIT-AMT - (WS-WORK-AMT * 3)
           COMPUTE CRCM-FINANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ADV FEE ON THE FINANCE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4490-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRCT-CASH-ADV-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE CRCT-CASH-ADV-AMT ROUNDED = CRCM-FINANCE-AMT
                       * CRCM-RATE / 200
               WHEN CRCM-SOURCE-CD = 'C'
                   MOVE 15.00 TO CRCT-CASH-ADV-AMT
               WHEN CRCM-SOURCE-CD = 'H'
                   MOVE 20.00 TO CRCT-CASH-ADV-AMT
               WHEN OTHER
                   COMPUTE CRCT-CASH-ADV-AMT ROUNDED = CRCM-FINANCE-AMT
                       * CRCM-RATE / 100
           END-EVALUATE
           IF CRCT-CASH-ADV-AMT > 0 AND CRCT-CASH-ADV-AMT < 10.00
               MOVE 10.00 TO CRCT-CASH-ADV-AMT
           END-IF
           IF CRCT-CASH-ADV-AMT > 50.00
               MOVE 50.00 TO CRCT-CASH-ADV-AMT
           END-IF
           ADD CRCT-CASH-ADV-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4500-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCT-TRAN-CD = 'PU'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN CRCT-TRAN-CD = 'CA'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = WS-WORK-AMT-2 *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PU -> CA WHEN CLOSE DATE IS PAST
      * CA -> PY WHEN CLOSE DATE IS PAST
      * PY -> FC WHEN CLOSE DATE IS PAST
      * FC -> LF WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4510-SET-TRAN.
           EVALUATE CRCT-TRAN-CD
               WHEN 'PU'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CA' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CA'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LF' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4520-DESCRIBE-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LNX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MGS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YNJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CASH ADV YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4530-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = CRCM-CASH-ADV-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CRCM-CASH-ADV-YTD - (WS-WORK-AMT *
               12)
           COMPUTE CRCT-MIN-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4540-COUNT-CYCLE.
           IF CRCM-CYCLE-CNT < 9999999
               ADD 1 TO CRCM-CYCLE-CNT
           END-IF
           IF CRCM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MIN DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       4550-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCT-PAYMENT-AMT ROUNDED =
                   CRCM-MIN-DUE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCT-PAYMENT-AMT
           END-IF
           ADD CRCT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CASH ADV AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4560-LIMIT-CASH-ADV.
           IF CRCT-CASH-ADV-AMT < 0.00
               MOVE 0.00 TO CRCT-CASH-ADV-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCT-CASH-ADV-AMT > 10000.00
               MOVE 10000.00 TO CRCT-CASH-ADV-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4570-DESCRIBE-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LNX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MGS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YNJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE BAND TIER FOR THE LIMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4580-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-BAND-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF CRCM-LIMIT-AMT NOT < WS-PURCHASE-BAND-LOW(WS-TX)
                   MOVE WS-PURCHASE-BAND-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FINANCE AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4590-LIMIT-FINANCE.
           IF CRCM-FINANCE-AMT < 0.00
               MOVE 0.00 TO CRCM-FINANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-FINANCE-AMT > 500.00
               MOVE 500.00 TO CRCM-FINANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BILL CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4600-DESCRIBE-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'E'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'P'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE MIN DUE GRID TIER FOR THE MIN DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4610-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-DUE-GRID-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CRCT-MIN-DUE-AMT NOT < WS-MIN-DUE-GRID-LOW(WS-TX)
                   MOVE WS-MIN-DUE-GRID-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT GRID TIER FOR THE FINANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4620-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-GRID-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CRCM-FINANCE-YTD NOT < WS-PAYMENT-GRID-LOW(WS-TX)
                   MOVE WS-PAYMENT-GRID-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4630-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = CRCM-PURCHASE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CRCM-PURCHASE-YTD - (WS-WORK-AMT * 3)
           COMPUTE CRCM-FINANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4640-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 5
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4650-LIMIT-PAYMENT.
           IF CRCM-PAYMENT-YTD < 25.00
               MOVE 25.00 TO CRCM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-PAYMENT-YTD > 500.00
               MOVE 500.00 TO CRCM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4660-COUNT-NOTICE.
           IF CRCM-NOTICE-CNT < 9999999
               ADD 1 TO CRCM-NOTICE-CNT
           END-IF
           IF CRCM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4670-COUNT-CYCLE.
           IF CRCM-CYCLE-CNT < 9999999
               ADD 1 TO CRCM-CYCLE-CNT
           END-IF
           IF CRCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E953 DESC REQUIRED
      * E450 MIN DUE YTD NEGATIVE
      * E599 MIN DUE YTD OVER 10000
      * E592 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4680-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-DESC = SPACES
                   MOVE 'E953' TO WS-REJECT-CODE
               WHEN CRCM-MIN-DUE-YTD < 0
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN CRCM-MIN-DUE-YTD > 10000.00
                   MOVE 'E599' TO WS-REJECT-CODE
               WHEN CRCM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E592' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MIN DUE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4690-FLAG-MIN-DUE.
           IF CRCM-MIN-DUE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCM-MIN-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4700-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE MIN DUE GRID TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4710-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-MIN-DUE-GRID-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF CRCT-PAYMENT-AMT NOT < WS-MIN-DUE-GRID-LOW(WS-TX)
                   MOVE WS-MIN-DUE-GRID-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4720-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRCM-LATE-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CRCM-LATE-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4730-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E369 OPERATOR REQUIRED
      * E292 CASH ADV YTD NEGATIVE
      * E912 CASH ADV YTD OVER 1000000
      * E141 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4740-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCT-OPERATOR = SPACES
                   MOVE 'E369' TO WS-REJECT-CODE
               WHEN CRCM-CASH-ADV-YTD < 0
                   MOVE 'E292' TO WS-REJECT-CODE
               WHEN CRCM-CASH-ADV-YTD > 1000000.00
                   MOVE 'E912' TO WS-REJECT-CODE
               WHEN CRCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E141' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE CRC FIELDS TO CRCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7040-CALL-CRCAL.
           INITIALIZE CRCAL-PARMS
           MOVE CRCM-LIMIT-AMT
             TO CRCAL-LIMIT-AMT
           MOVE CRCM-CASH-ADV-AMT
             TO CRCAL-CASH-ADV-AMT
           MOVE CRCM-MIN-DUE-YTD
             TO CRCAL-MIN-DUE-YTD
           MOVE CRCM-LATE-FEE-AMT
             TO CRCAL-LATE-FEE-AMT
           MOVE CRCM-FINANCE-YTD
             TO CRCAL-FINANCE-YTD
           MOVE CRCM-CASH-ADV-YTD
             TO CRCAL-CASH-ADV-YTD
           MOVE CRCM-EFFECTIVE-DATE
             TO CRCAL-EFFECTIVE-DATE
           MOVE CRCM-CLOSE-DATE
             TO CRCAL-CLOSE-DATE
           MOVE CRCM-MATURITY-DATE
             TO CRCAL-MATURITY-DATE
           MOVE CRCM-EXPIRY-DATE
             TO CRCAL-EXPIRY-DATE
           MOVE CRCM-STATUS-CD
             TO CRCAL-STATUS-CD
           MOVE CRCM-SOURCE-CD
             TO CRCAL-SOURCE-CD
           MOVE CRCM-CARD-TYPE-CD
             TO CRCAL-CARD-TYPE-CD
           MOVE CRCM-TYPE-CD
             TO CRCAL-TYPE-CD
           MOVE CRCM-EXEMPT-FLAG
             TO CRCAL-EXEMPT-FLAG
           MOVE CRCM-OVERRIDE-FLAG
             TO CRCAL-OVERRIDE-FLAG
           MOVE CRCM-AUTO-RENEW-FLAG
             TO CRCAL-AUTO-RENEW-FLAG
           MOVE CRCM-RETRY-CNT
             TO CRCAL-RETRY-CNT
           MOVE CRCM-CYCLE-CNT
             TO CRCAL-CYCLE-CNT
           MOVE CRCM-PENALTY-PCT
             TO CRCAL-PENALTY-PCT
           MOVE CRCM-RATE
             TO CRCAL-RATE
           MOVE CRCM-DESC
             TO CRCAL-DESC
           MOVE CRCM-CUST-ID
             TO CRCAL-CUST-ID
           CALL 'CRCAL' USING CRCAL-PARMS
           EVALUATE CRCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CRCAL-LIMIT-AMT
                     TO CRCM-LIMIT-AMT
                   MOVE CRCAL-CASH-ADV-AMT
                     TO CRCM-CASH-ADV-AMT
                   MOVE CRCAL-MIN-DUE-YTD
                     TO CRCM-MIN-DUE-YTD
                   MOVE CRCAL-EXEMPT-FLAG
                     TO CRCM-EXEMPT-FLAG
                   MOVE CRCAL-AUTO-RENEW-FLAG
                     TO CRCM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-CRCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE CRC FIELDS TO CRDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-CRDTE.
           INITIALIZE CRDTE-PARMS
           MOVE CRCM-PURCHASE-YTD
             TO CRDTE-PURCHASE-YTD
           MOVE CRCM-LIMIT-AMT
             TO CRDTE-LIMIT-AMT
           MOVE CRCM-FINANCE-AMT
             TO CRDTE-FINANCE-AMT
           MOVE CRCM-LIMIT-YTD
             TO CRDTE-LIMIT-YTD
           MOVE CRCM-PURCHASE-AMT
             TO CRDTE-PURCHASE-AMT
           MOVE CRCM-LATE-FEE-YTD
             TO CRDTE-LATE-FEE-YTD
           MOVE CRCM-EXPIRY-DATE
             TO CRDTE-EXPIRY-DATE
           MOVE CRCM-OPEN-DATE
             TO CRDTE-OPEN-DATE
           MOVE CRCM-LAST-PAY-DATE
             TO CRDTE-LAST-PAY-DATE
           MOVE CRCM-CLOSE-DATE
             TO CRDTE-CLOSE-DATE
           MOVE CRCM-CARD-TYPE-CD
             TO CRDTE-CARD-TYPE-CD
           MOVE CRCM-SOURCE-CD
             TO CRDTE-SOURCE-CD
           MOVE CRCM-TYPE-CD
             TO CRDTE-TYPE-CD
           MOVE CRCM-BRANCH-CD
             TO CRDTE-BRANCH-CD
           MOVE CRCM-PREFERRED-FLAG
             TO CRDTE-PREFERRED-FLAG
           MOVE CRCM-STOP-FLAG
             TO CRDTE-STOP-FLAG
           MOVE CRCM-EXEMPT-FLAG
             TO CRDTE-EXEMPT-FLAG
           MOVE CRCM-NOTICE-CNT
             TO CRDTE-NOTICE-CNT
           MOVE CRCM-CYCLE-CNT
             TO CRDTE-CYCLE-CNT
           MOVE CRCM-DISCOUNT
             TO CRDTE-DISCOUNT
           MOVE CRCM-PENALTY-PCT
             TO CRDTE-PENALTY-PCT
           MOVE CRCM-NAME
             TO CRDTE-NAME
           MOVE CRCM-CUST-ID
             TO CRDTE-CUST-ID
           CALL 'CRDTE' USING CRDTE-PARMS
           EVALUATE CRDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CRDTE-LIMIT-AMT
                     TO CRCM-LIMIT-AMT
                   MOVE CRDTE-LATE-FEE-YTD
                     TO CRCM-LATE-FEE-YTD
                   MOVE CRDTE-PREFERRED-FLAG
                     TO CRCM-PREFERRED-FLAG
                   MOVE CRDTE-EXEMPT-FLAG
                     TO CRCM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-CRDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE CRCT-KEY TO RD-KEY
           MOVE CRCT-TRAN-CD TO RD-CODE
           MOVE CRCT-PAYMENT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE CRCT-KEY TO RD-KEY
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
           DISPLAY 'CRCEXTR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CRCEXTR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
