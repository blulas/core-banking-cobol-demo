      *================================================================*
      * PROGRAM  : CRCPOST                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PROPRIETARY CREDIT CARD - DAILY POSTING. READS THE  *
      *            CRC TRANSACTIONS, UPDATES THE CRC MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. IN-HOUSE CREDIT CARD   *
      *            BILLING, FINANCE CHARGE AND DELINQUENCY             *
      * INPUT    : CRCMST (KSDS), CRCTRN, BANKCTL                      *
      * OUTPUT   : CRCOUT, CRCPORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CRELG, CRVAL, CRDTE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-07-28 RJT  ORIGINAL                                       *
      * 2001-08-15 TWH  PAYMENT TABLE EXPANDED CHG2222                 *
      * 2001-09-20 GAF  NEW PURCHASE REPORT COLUMNS REQ6563            *
      * 2001-07-13 GAF  PERFORMANCE - PAYMENT READ AHEAD REQ5398       *
      * 2006-06-13 GAF  LIMIT FEE SCHEDULE UPDATE PRB7393              *
      * 2006-06-27 RJT  PURCHASE RERUN PROTECTION PRB2095              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRCPOST.
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
           SELECT REPORT-FILE ASSIGN TO "CRCPORPT"
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
                                           VALUE 25.000000.
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
      * WS-LIMIT-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LIMIT-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000545500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000047000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000600000000635500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000850000000657000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010850000000207500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000431000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013600000000153500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018600000000601500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019100000000299000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019150000000007500'.
       01  WS-LIMIT-GRID-TBL REDEFINES WS-LIMIT-GRID-VALUES.
           05  WS-LIMIT-GRID-ROW OCCURS 10 TIMES.
               10  WS-LIMIT-GRID-LOW       PIC 9(09)V99.
               10  WS-LIMIT-GRID-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYMENT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000716500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000243000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000165500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000588000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011350000000572500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012350000000537000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000443000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000236000'.
       01  WS-PAYMENT-SCHED-TBL REDEFINES WS-PAYMENT-SCHED-VALUES.
           05  WS-PAYMENT-SCHED-ROW OCCURS 8 TIMES.
               10  WS-PAYMENT-SCHED-LOW    PIC 9(09)V99.
               10  WS-PAYMENT-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-ADV-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-ADV-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000679000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000577500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000612000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005350000000491000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006350000000808000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006600000000044000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006650000000024500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007650000000180000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007750000000762000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007850000000682500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008100000000546000'.
       01  WS-CASH-ADV-TIER-TBL REDEFINES WS-CASH-ADV-TIER-VALUES.
           05  WS-CASH-ADV-TIER-ROW OCCURS 11 TIMES.
               10  WS-CASH-ADV-TIER-LOW    PIC 9(09)V99.
               10  WS-CASH-ADV-TIER-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CRCPOST  PROPRIETARY CREDIT CARD'.
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
       COPY CRELGP.
       COPY CRVALP.
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
               PERFORM 3000-PRORATE-LATE-FEE
               PERFORM 3010-COUNT-RETRY
               PERFORM 3020-ACCRUE-FINANCE
               PERFORM 3030-CHECK-MATURITY
               PERFORM 3040-CHECK-EFF
               PERFORM 3050-ACCRUE-PAYMENT
               PERFORM 3060-DESCRIBE-BILL-CYCLE
               PERFORM 3070-CHECK-NEXT-REVIEW
               PERFORM 3080-PRORATE-MIN-DUE
               PERFORM 3090-LIMIT-MIN-DUE
               PERFORM 3100-ASSESS-EXEMPT
               PERFORM 3110-ACCRUE-FINANCE
               PERFORM 3120-AGE-LAST-ACT
               PERFORM 3130-SET-SOURCE
               PERFORM 3140-CHECK-MATURITY
               PERFORM 3150-ASSESS-OVERRIDE
               PERFORM 3160-SELECT-PENALTY-PCT
               PERFORM 3170-ACCUM-BILL-CYCLE
               PERFORM 3180-LIMIT-FINANCE
               PERFORM 3190-SELECT-DISCOUNT
               PERFORM 3200-SET-TRAN
               PERFORM 3210-ACCRUE-LIMIT
               PERFORM 3220-AGE-NOTICE
               PERFORM 3230-AGE-CLOSE
               PERFORM 3240-CHECK-EFFECTIVE
               PERFORM 3250-LIMIT-LIMIT
               PERFORM 3260-SELECT-PENALTY-PCT
               PERFORM 3270-DESCRIBE-STATUS
               PERFORM 3280-SELECT-RATE
               PERFORM 3290-ACCRUE-RATE
               PERFORM 3300-SELECT-RATE
               PERFORM 3310-COUNT-ITEM
               PERFORM 3320-ACCUM-BRANCH
               PERFORM 3330-PRORATE-MIN-DUE
               PERFORM 3340-AGE-LAST-ACT
               PERFORM 3350-LIMIT-MIN-DUE
               PERFORM 3360-SELECT-RATE
               PERFORM 3370-SELECT-DISCOUNT
               PERFORM 3380-ASSESS-OVERRIDE
               PERFORM 3390-PRORATE-LIMIT
               PERFORM 3400-COUNT-ITEM
               PERFORM 3410-SELECT-RATE
               PERFORM 3420-ACCUM-CARD-TYPE
               PERFORM 3430-SPLIT-PAYMENT
               PERFORM 3440-LIMIT-FINANCE
               PERFORM 3450-SPLIT-CASH-ADV
               PERFORM 3460-SELECT-DISCOUNT
               PERFORM 3470-ASSESS-EXEMPT
               PERFORM 3480-SELECT-PENALTY-PCT
               PERFORM 3490-ACCRUE-PURCHASE
               PERFORM 3500-LIMIT-PURCHASE
               PERFORM 3510-AGE-EFFECTIVE
               PERFORM 3520-SET-STATUS
               PERFORM 3530-SELECT-PENALTY-PCT
               PERFORM 3540-PRORATE-PAYMENT
               PERFORM 3550-ASSESS-EXEMPT
               PERFORM 3560-COUNT-NOTICE
               PERFORM 3570-AGE-NOTICE
               PERFORM 3580-EDIT-CONTACT
               PERFORM 3590-ASSESS-REVERSAL
               PERFORM 3600-FLAG-LATE-FEE
               PERFORM 3610-SET-TRAN
               PERFORM 3620-FLAG-LATE-FEE
               PERFORM 3630-ACCRUE-PAYMENT
               PERFORM 3640-PRORATE-MIN-DUE
               PERFORM 3650-ACCUM-CHANNEL
               PERFORM 3660-CHECK-LAST-ACT
               PERFORM 3670-DESCRIBE-STATUS
               PERFORM 3680-ACCUM-TYPE
               PERFORM 3690-COUNT-RETRY
               PERFORM 3700-EDIT-ADDR
               PERFORM 3710-ACCUM-CARD-TYPE
               PERFORM 3720-PRORATE-MIN-DUE
               PERFORM 3730-SET-BRANCH
               PERFORM 3740-ASSESS-OVERRIDE
               PERFORM 3750-SET-CHANNEL
               PERFORM 3760-CHECK-NOTICE
               PERFORM 3770-PRORATE-MIN-DUE
               PERFORM 3780-SET-BILL-CYCLE
               PERFORM 3790-EDIT-NAME
               PERFORM 3800-EDIT-ADDR
               PERFORM 3810-SET-STATUS
               PERFORM 3820-LIMIT-ITEM
               PERFORM 3830-LIMIT-PURCHASE
               PERFORM 3840-ASSESS-AUTO-RENEW
               PERFORM 3850-PRORATE-CASH-ADV
               PERFORM 3860-COUNT-NOTICE
               PERFORM 3870-DESCRIBE-CARD-TYPE
               PERFORM 3880-AGE-NOTICE
               PERFORM 3890-CHECK-NEXT-REVIEW
               PERFORM 3900-PRORATE-FINANCE
               PERFORM 3910-AGE-EFFECTIVE
               PERFORM 3920-PRORATE-MIN-DUE
               PERFORM 3930-PRORATE-PAYMENT
               PERFORM 3940-LIMIT-LIMIT
               PERFORM 3950-ASSESS-AUTO-RENEW
               PERFORM 3960-ASSESS-OVERRIDE
               PERFORM 3970-SPLIT-LATE-FEE
               PERFORM 3980-ACCRUE-PURCHASE
               PERFORM 3990-CHECK-NEXT-REVIEW
               PERFORM 4000-SPLIT-MIN-DUE
               PERFORM 4010-EDIT-CUST-ID
               PERFORM 4020-COUNT-SEQ
               PERFORM 4030-PRORATE-PAYMENT
               PERFORM 4040-ASSESS-EXEMPT
               PERFORM 4050-ACCUM-STATUS
               PERFORM 4060-DESCRIBE-STATUS
               PERFORM 4070-SPLIT-CASH-ADV
               PERFORM 4080-FLAG-ITEM
               PERFORM 4090-SPLIT-LIMIT
               PERFORM 4100-SELECT-PENALTY-PCT
               PERFORM 4110-COUNT-SEQ
               PERFORM 4120-PRORATE-PAYMENT
               PERFORM 4130-EDIT-REJECT
               PERFORM 4140-COUNT-CYCLE
               PERFORM 4150-SET-SOURCE
               PERFORM 4160-FLAG-LIMIT
               PERFORM 4170-SET-TYPE
               PERFORM 4180-COUNT-SEQ
               PERFORM 4190-ACCRUE-PURCHASE
               PERFORM 4200-ACCRUE-CASH-ADV
               PERFORM 4210-SET-STATUS
               PERFORM 4220-SPLIT-MIN-DUE
               PERFORM 4230-DESCRIBE-SOURCE
               PERFORM 4240-ACCUM-SOURCE
               PERFORM 4250-ACCRUE-PAYMENT
               PERFORM 4260-ASSESS-OVERRIDE
               PERFORM 4270-SET-SOURCE
               PERFORM 4280-ASSESS-REVERSAL
               PERFORM 4290-ACCRUE-PURCHASE
               PERFORM 4300-COUNT-RETRY
               PERFORM 4310-ACCRUE-FINANCE
               PERFORM 4320-EDIT-REF-NO
               PERFORM 4330-EDIT-DESC
               PERFORM 4340-FLAG-ITEM
               PERFORM 4350-CHECK-EFFECTIVE
               PERFORM 4360-ASSESS-REVERSAL
               PERFORM 4370-ACCUM-CHANNEL
               PERFORM 4380-PRORATE-PAYMENT
               PERFORM 4390-EDIT-REF-NO
               PERFORM 4400-PRORATE-LIMIT
               PERFORM 4410-ACCUM-SOURCE
               PERFORM 4420-COUNT-ITEM
               PERFORM 4430-FLAG-CASH-ADV
               PERFORM 4440-ASSESS-PREFERRED
               PERFORM 4450-AGE-NEXT-REVIEW
               PERFORM 4460-EDIT-REF-NO
               PERFORM 4470-ACCUM-SOURCE
               PERFORM 4480-ACCUM-BILL-CYCLE
               PERFORM 4490-DESCRIBE-BILL-CYCLE
               PERFORM 4500-SET-SOURCE
               PERFORM 4510-FLAG-PURCHASE
               PERFORM 4520-LIMIT-CASH-ADV
               PERFORM 4530-FLAG-LIMIT
               PERFORM 4540-EDIT-CONTACT
               PERFORM 4550-DESCRIBE-STATUS
               PERFORM 4560-SPLIT-CASH-ADV
               PERFORM 4570-SELECT-DISCOUNT
               PERFORM 4580-EDIT-REJECT
               PERFORM 4590-AGE-LAST-PAY
               PERFORM 4600-DESCRIBE-CARD-TYPE
               PERFORM 4610-COUNT-ITEM
               PERFORM 4620-FLAG-PAYMENT
               PERFORM 4630-PRORATE-CASH-ADV
               PERFORM 4640-COUNT-CYCLE
               PERFORM 4650-ACCUM-BILL-CYCLE
               PERFORM 4660-EDIT-NAME
               PERFORM 4670-CHECK-MATURITY
               PERFORM 4680-AGE-MATURITY
               PERFORM 4690-LIMIT-LIMIT
               PERFORM 4700-ACCUM-CHANNEL
               PERFORM 4710-PRORATE-PAYMENT
               PERFORM 4720-SELECT-RATE
               PERFORM 4730-COUNT-SEQ
               PERFORM 7030-CALL-CRELG
               PERFORM 7010-CALL-CRVAL
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
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LATE FEE AMT.
      *----------------------------------------------------------------*
       3000-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-LATE-FEE-AMT ROUNDED =
                   CRCM-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-LATE-FEE-AMT
           END-IF
           ADD CRCM-LATE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 12
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FINANCE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3020-ACCRUE-FINANCE.
           IF CRCM-FINANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-FINANCE-YTD * CRCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CRCM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3030-CHECK-MATURITY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3040-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF CRCT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3050-ACCRUE-PAYMENT.
           IF CRCM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PAYMENT-AMT * CRCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO CRCT-CASH-ADV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BILL CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3060-DESCRIBE-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'D'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'E'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CRCM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FINANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN DUE YTD.
      *----------------------------------------------------------------*
       3080-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-MIN-DUE-YTD ROUNDED =
                   CRCM-FINANCE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-MIN-DUE-YTD
           END-IF
           ADD CRCM-MIN-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * MIN DUE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3090-LIMIT-MIN-DUE.
           IF CRCM-MIN-DUE-AMT < 0.00
               MOVE 0.00 TO CRCM-MIN-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-MIN-DUE-AMT > 10000.00
               MOVE 10000.00 TO CRCM-MIN-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FINANCE YTD FEE ON THE MIN DUE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3100-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CRCM-FINANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CRCM-FINANCE-YTD ROUNDED = CRCT-MIN-DUE-AMT *
                       CRCM-DISCOUNT / 200
               WHEN CRCM-TYPE-CD = 'GP'
                   MOVE 12.00 TO CRCM-FINANCE-YTD
               WHEN CRCM-TYPE-CD = 'UV'
                   MOVE 5.00 TO CRCM-FINANCE-YTD
               WHEN OTHER
                   COMPUTE CRCM-FINANCE-YTD ROUNDED = CRCT-MIN-DUE-AMT *
                       CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCM-FINANCE-YTD > 0 AND CRCM-FINANCE-YTD < 5.00
               MOVE 5.00 TO CRCM-FINANCE-YTD
           END-IF
           IF CRCM-FINANCE-YTD > 35.00
               MOVE 35.00 TO CRCM-FINANCE-YTD
           END-IF
           ADD CRCM-FINANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FINANCE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3110-ACCRUE-FINANCE.
           IF CRCM-FINANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-FINANCE-YTD * CRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * C -> H WHEN MATURITY DATE IS PAST
      * H -> M WHEN MATURITY DATE IS PAST
      * M -> R WHEN MATURITY DATE IS PAST
      * R -> S WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3130-SET-SOURCE.
           EVALUATE CRCM-SOURCE-CD
               WHEN 'C'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-MATURITY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO CRCM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3150-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PURCHASE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CRCM-PURCHASE-YTD ROUNDED = WS-WORK-AMT-2 *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCM-BRANCH-CD = 'CTB'
                   MOVE 3.00 TO CRCM-PURCHASE-YTD
               WHEN CRCM-BRANCH-CD = 'LNX'
                   MOVE 12.00 TO CRCM-PURCHASE-YTD
               WHEN OTHER
                   COMPUTE CRCM-PURCHASE-YTD ROUNDED = WS-WORK-AMT-2 *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-PURCHASE-YTD > 0 AND CRCM-PURCHASE-YTD < 2.00
               MOVE 2.00 TO CRCM-PURCHASE-YTD
           END-IF
           IF CRCM-PURCHASE-YTD > 50.00
               MOVE 50.00 TO CRCM-PURCHASE-YTD
           END-IF
           ADD CRCM-PURCHASE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT SCHED TIER FOR THE CASH ADV YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3160-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-SCHED-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CRCM-CASH-ADV-YTD NOT < WS-PAYMENT-SCHED-LOW(WS-TX)
                   MOVE WS-PAYMENT-SCHED-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ADV AMT BY BILL CYCLE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-BILL-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-BILL-CYCLE-CD
                   ADD CRCT-CASH-ADV-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-BILL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCT-CASH-ADV-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FINANCE YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3180-LIMIT-FINANCE.
           IF CRCM-FINANCE-YTD < 25.00
               MOVE 25.00 TO CRCM-FINANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-FINANCE-YTD > 2500.00
               MOVE 2500.00 TO CRCM-FINANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE CASH ADV AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3190-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CRCM-CASH-ADV-AMT NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PU -> CA WHEN LAST PAY DATE IS PAST
      * CA -> PY WHEN LAST PAY DATE IS PAST
      * PY -> FC WHEN LAST PAY DATE IS PAST
      * FC -> LF WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3200-SET-TRAN.
           EVALUATE CRCT-TRAN-CD
               WHEN 'PU'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CA' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CA'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LF' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-LIMIT.
           IF CRCM-LIMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-LIMIT-AMT * CRCM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRCT-CASH-ADV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3230-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-EFFECTIVE.
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
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3250-LIMIT-LIMIT.
           IF CRCM-LIMIT-AMT < 50.00
               MOVE 50.00 TO CRCM-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-LIMIT-AMT > 2500.00
               MOVE 2500.00 TO CRCM-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV TIER TIER FOR THE PURCHASE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3260-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-TIER-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CRCM-PURCHASE-YTD NOT < WS-CASH-ADV-TIER-LOW(WS-TX)
                   MOVE WS-CASH-ADV-TIER-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3270-DESCRIBE-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT SCHED TIER FOR THE LIMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-SCHED-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CRCT-LIMIT-AMT NOT < WS-PAYMENT-SCHED-LOW(WS-TX)
                   MOVE WS-PAYMENT-SCHED-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3290-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CRCM-FINANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE FINANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3300-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CRCM-FINANCE-YTD NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3310-COUNT-ITEM.
           IF CRCM-ITEM-CNT < 9999999
               ADD 1 TO CRCM-ITEM-CNT
           END-IF
           IF CRCM-ITEM-CNT NOT < 5
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN DUE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3320-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-BRANCH-CD
                   ADD CRCT-MIN-DUE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCT-MIN-DUE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN DUE AMT.
      *----------------------------------------------------------------*
       3330-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCT-MIN-DUE-AMT ROUNDED =
                   CRCM-PURCHASE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCT-MIN-DUE-AMT
           END-IF
           ADD CRCT-MIN-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3340-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * MIN DUE AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3350-LIMIT-MIN-DUE.
           IF CRCT-MIN-DUE-AMT < 0.00
               MOVE 0.00 TO CRCT-MIN-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCT-MIN-DUE-AMT > 1000.00
               MOVE 1000.00 TO CRCT-MIN-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3360-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-TIER-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-CASH-ADV-TIER-LOW(WS-TX)
                   MOVE WS-CASH-ADV-TIER-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE FINANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3370-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CRCM-FINANCE-AMT NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE FEE ON THE PAYMENT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3380-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PURCHASE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   COMPUTE CRCM-PURCHASE-AMT ROUNDED = CRCM-PAYMENT-AMT
                       * CRCM-DISCOUNT / 200
               WHEN CRCM-SOURCE-CD = 'C'
                   MOVE 10.00 TO CRCM-PURCHASE-AMT
               WHEN CRCM-SOURCE-CD = 'H'
                   MOVE 15.00 TO CRCM-PURCHASE-AMT
               WHEN OTHER
                   COMPUTE CRCM-PURCHASE-AMT ROUNDED = CRCM-PAYMENT-AMT
                       * CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCM-PURCHASE-AMT > 0 AND CRCM-PURCHASE-AMT < 10.00
               MOVE 10.00 TO CRCM-PURCHASE-AMT
           END-IF
           IF CRCM-PURCHASE-AMT > 100.00
               MOVE 100.00 TO CRCM-PURCHASE-AMT
           END-IF
           ADD CRCM-PURCHASE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT YTD.
      *----------------------------------------------------------------*
       3390-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-LIMIT-YTD ROUNDED =
                   CRCM-PAYMENT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-LIMIT-YTD
           END-IF
           ADD CRCM-LIMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-ITEM.
           IF CRCM-ITEM-CNT < 9999999
               ADD 1 TO CRCM-ITEM-CNT
           END-IF
           IF CRCM-ITEM-CNT NOT < 6
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3410-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN DUE YTD BY CARD TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3420-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-CARD-TYPE-CD
                   ADD CRCM-MIN-DUE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-MIN-DUE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = CRCT-PAYMENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CRCT-PAYMENT-AMT - (WS-WORK-AMT * 3)
           COMPUTE CRCM-CASH-ADV-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FINANCE YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3440-LIMIT-FINANCE.
           IF CRCM-FINANCE-YTD < 0.00
               MOVE 0.00 TO CRCM-FINANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-FINANCE-YTD > 50000.00
               MOVE 50000.00 TO CRCM-FINANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH ADV AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3450-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = CRCT-CASH-ADV-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CRCT-CASH-ADV-AMT - (WS-WORK-AMT * 4)
           COMPUTE CRCM-LIMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE CASH ADV AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3460-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CRCM-CASH-ADV-AMT NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE CASH ADV AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3470-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CRCT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CRCT-PAYMENT-AMT ROUNDED = CRCT-CASH-ADV-AMT
                       * CRCM-RATE / 200
               WHEN CRCT-TRAN-CD = 'PU'
                   MOVE 20.00 TO CRCT-PAYMENT-AMT
               WHEN CRCT-TRAN-CD = 'CA'
                   MOVE 5.00 TO CRCT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRCT-PAYMENT-AMT ROUNDED = CRCT-CASH-ADV-AMT
                       * CRCM-RATE / 100
           END-EVALUATE
           IF CRCT-PAYMENT-AMT > 0 AND CRCT-PAYMENT-AMT < 5.00
               MOVE 5.00 TO CRCT-PAYMENT-AMT
           END-IF
           IF CRCT-PAYMENT-AMT > 35.00
               MOVE 35.00 TO CRCT-PAYMENT-AMT
           END-IF
           ADD CRCT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE LIMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3480-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CRCT-LIMIT-AMT NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PURCHASE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-PURCHASE.
           IF CRCM-PURCHASE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PURCHASE-YTD * CRCM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3500-LIMIT-PURCHASE.
           IF CRCM-PURCHASE-AMT < 100.00
               MOVE 100.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-PURCHASE-AMT > 10000.00
               MOVE 10000.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3510-AGE-EFFECTIVE.
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
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN EXPIRY DATE IS PAST
      * DDY -> DPB WHEN EXPIRY DATE IS PAST
      * DPB -> UHF WHEN EXPIRY DATE IS PAST
      * UHF -> WLL WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3520-SET-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE LIMIT GRID TIER FOR THE MIN DUE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3530-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-LIMIT-GRID-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CRCM-MIN-DUE-YTD NOT < WS-LIMIT-GRID-LOW(WS-TX)
                   MOVE WS-LIMIT-GRID-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       3540-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-PAYMENT-YTD ROUNDED =
                   CRCM-LIMIT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-PAYMENT-YTD
           END-IF
           ADD CRCM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE LATE FEE YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3550-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CRCM-LATE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE CRCM-LATE-FEE-YTD ROUNDED = WS-WORK-AMT-2 *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCM-TYPE-CD = 'GP'
                   MOVE 12.00 TO CRCM-LATE-FEE-YTD
               WHEN CRCM-TYPE-CD = 'UV'
                   MOVE 20.00 TO CRCM-LATE-FEE-YTD
               WHEN OTHER
                   COMPUTE CRCM-LATE-FEE-YTD ROUNDED = WS-WORK-AMT-2 *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-LATE-FEE-YTD > 0 AND CRCM-LATE-FEE-YTD < 10.00
               MOVE 10.00 TO CRCM-LATE-FEE-YTD
           END-IF
           IF CRCM-LATE-FEE-YTD > 100.00
               MOVE 100.00 TO CRCM-LATE-FEE-YTD
           END-IF
           ADD CRCM-LATE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3560-COUNT-NOTICE.
           IF CRCM-NOTICE-CNT < 9999999
               ADD 1 TO CRCM-NOTICE-CNT
           END-IF
           IF CRCM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3570-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E899 CONTACT REQUIRED
      * E573 LATE FEE AMT NEGATIVE
      * E455 LATE FEE AMT OVER 250000
      * E657 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3580-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CONTACT = SPACES
                   MOVE 'E899' TO WS-REJECT-CODE
               WHEN CRCM-LATE-FEE-AMT < 0
                   MOVE 'E573' TO WS-REJECT-CODE
               WHEN CRCM-LATE-FEE-AMT > 250000.00
                   MOVE 'E455' TO WS-REJECT-CODE
               WHEN CRCM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E657' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE FINANCE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3590-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CRCM-PAYMENT-YTD ROUNDED = CRCM-FINANCE-AMT *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCM-BILL-CYCLE-CD = 'B'
                   MOVE 10.00 TO CRCM-PAYMENT-YTD
               WHEN CRCM-BILL-CYCLE-CD = 'D'
                   MOVE 7.00 TO CRCM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE CRCM-PAYMENT-YTD ROUNDED = CRCM-FINANCE-AMT *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-PAYMENT-YTD > 0 AND CRCM-PAYMENT-YTD < 1.00
               MOVE 1.00 TO CRCM-PAYMENT-YTD
           END-IF
           IF CRCM-PAYMENT-YTD > 35.00
               MOVE 35.00 TO CRCM-PAYMENT-YTD
           END-IF
           ADD CRCM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * LATE FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3600-FLAG-LATE-FEE.
           IF CRCM-LATE-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-LATE-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PU -> CA WHEN EFF DATE IS PAST
      * CA -> PY WHEN EFF DATE IS PAST
      * PY -> FC WHEN EFF DATE IS PAST
      * FC -> LF WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3610-SET-TRAN.
           EVALUATE CRCT-TRAN-CD
               WHEN 'PU'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CA' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CA'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LF' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LATE FEE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3620-FLAG-LATE-FEE.
           IF CRCM-LATE-FEE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCM-LATE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3630-ACCRUE-PAYMENT.
           IF CRCM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PAYMENT-AMT * CRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CRCM-PURCHASE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MIN DUE YTD.
      *----------------------------------------------------------------*
       3640-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-MIN-DUE-YTD ROUNDED =
                   CRCM-LATE-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-MIN-DUE-YTD
           END-IF
           ADD CRCM-MIN-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3650-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCT-CHANNEL-CD
                   ADD CRCM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 90 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3660-CHECK-LAST-ACT.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3680-ACCUM-TYPE.
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
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 12
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E586 ADDR REQUIRED
      * E058 LIMIT AMT NEGATIVE
      * E382 LIMIT AMT OVER 250000
      * E156 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-ADDR = SPACES
                   MOVE 'E586' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-AMT < 0
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-AMT > 250000.00
                   MOVE 'E382' TO WS-REJECT-CODE
               WHEN CRCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E156' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FINANCE YTD BY CARD TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3710-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-CARD-TYPE-CD
                   ADD CRCM-FINANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-FINANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN DUE YTD.
      *----------------------------------------------------------------*
       3720-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-MIN-DUE-YTD ROUNDED =
                   CRCT-LIMIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-MIN-DUE-YTD
           END-IF
           ADD CRCM-MIN-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CTB -> LNX WHEN MATURITY DATE IS PAST
      * LNX -> MGS WHEN MATURITY DATE IS PAST
      * MGS -> YNJ WHEN MATURITY DATE IS PAST
      * YNJ -> CTB WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3730-SET-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LNX' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LNX'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MGS' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MGS'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YNJ' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YNJ'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CTB' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT FEE ON THE CASH ADV AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3740-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CRCT-LIMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CRCT-LIMIT-AMT ROUNDED = CRCT-CASH-ADV-AMT *
                       CRCM-RATE / 200
               WHEN CRCM-TYPE-CD = 'GP'
                   MOVE 3.00 TO CRCT-LIMIT-AMT
               WHEN CRCM-TYPE-CD = 'UV'
                   MOVE 7.00 TO CRCT-LIMIT-AMT
               WHEN OTHER
                   COMPUTE CRCT-LIMIT-AMT ROUNDED = CRCT-CASH-ADV-AMT *
                       CRCM-RATE / 100
           END-EVALUATE
           IF CRCT-LIMIT-AMT > 0 AND CRCT-LIMIT-AMT < 1.00
               MOVE 1.00 TO CRCT-LIMIT-AMT
           END-IF
           IF CRCT-LIMIT-AMT > 25.00
               MOVE 25.00 TO CRCT-LIMIT-AMT
           END-IF
           ADD CRCT-LIMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-CHANNEL.
           EVALUATE CRCT-CHANNEL-CD
               WHEN 'BRN'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MIN DUE YTD.
      *----------------------------------------------------------------*
       3770-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-MIN-DUE-YTD ROUNDED =
                   CRCM-PURCHASE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-MIN-DUE-YTD
           END-IF
           ADD CRCM-MIN-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BILL CYCLE CD:
      * B -> D WHEN EXPIRY DATE IS PAST
      * D -> E WHEN EXPIRY DATE IS PAST
      * E -> F WHEN EXPIRY DATE IS PAST
      * F -> H WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3780-SET-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E810 NAME REQUIRED
      * E176 PAYMENT AMT NEGATIVE
      * E703 PAYMENT AMT OVER 5000
      * E239 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-NAME = SPACES
                   MOVE 'E810' TO WS-REJECT-CODE
               WHEN CRCM-PAYMENT-AMT < 0
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN CRCM-PAYMENT-AMT > 5000.00
                   MOVE 'E703' TO WS-REJECT-CODE
               WHEN CRCM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E239' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E634 ADDR REQUIRED
      * E446 MIN DUE AMT NEGATIVE
      * E679 MIN DUE AMT OVER 250000
      * E230 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-ADDR = SPACES
                   MOVE 'E634' TO WS-REJECT-CODE
               WHEN CRCT-MIN-DUE-AMT < 0
                   MOVE 'E446' TO WS-REJECT-CODE
               WHEN CRCT-MIN-DUE-AMT > 250000.00
                   MOVE 'E679' TO WS-REJECT-CODE
               WHEN CRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN CLOSE DATE IS PAST
      * DDY -> DPB WHEN CLOSE DATE IS PAST
      * DPB -> UHF WHEN CLOSE DATE IS PAST
      * UHF -> WLL WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3820-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3830-LIMIT-PURCHASE.
           IF CRCM-PURCHASE-AMT < 50.00
               MOVE 50.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-PURCHASE-AMT > 10000.00
               MOVE 10000.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE FEE ON THE PURCHASE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3840-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CRCT-MIN-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   COMPUTE CRCT-MIN-DUE-AMT ROUNDED = CRCM-PURCHASE-AMT
                       * CRCM-DISCOUNT / 200
               WHEN CRCM-CARD-TYPE-CD = 'PGU'
                   MOVE 7.00 TO CRCT-MIN-DUE-AMT
               WHEN CRCM-CARD-TYPE-CD = 'TRN'
                   MOVE 5.00 TO CRCT-MIN-DUE-AMT
               WHEN OTHER
                   COMPUTE CRCT-MIN-DUE-AMT ROUNDED = CRCM-PURCHASE-AMT
                       * CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCT-MIN-DUE-AMT > 0 AND CRCT-MIN-DUE-AMT < 2.00
               MOVE 2.00 TO CRCT-MIN-DUE-AMT
           END-IF
           IF CRCT-MIN-DUE-AMT > 100.00
               MOVE 100.00 TO CRCT-MIN-DUE-AMT
           END-IF
           ADD CRCT-MIN-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE LIMIT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH ADV AMT.
      *----------------------------------------------------------------*
       3850-PRORATE-CASH-ADV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCT-CASH-ADV-AMT ROUNDED =
                   CRCM-LIMIT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCT-CASH-ADV-AMT
           END-IF
           ADD CRCT-CASH-ADV-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3860-COUNT-NOTICE.
           IF CRCM-NOTICE-CNT < 9999999
               ADD 1 TO CRCM-NOTICE-CNT
           END-IF
           IF CRCM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3880-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3890-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CRCM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-NEXT-REVIEW-DATE TO DTU-DATE-1
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
      * PRORATE FINANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FINANCE YTD.
      *----------------------------------------------------------------*
       3900-PRORATE-FINANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-FINANCE-YTD ROUNDED =
                   CRCM-FINANCE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-FINANCE-YTD
           END-IF
           ADD CRCM-FINANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-EFFECTIVE.
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
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MIN DUE AMT.
      *----------------------------------------------------------------*
       3920-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCT-MIN-DUE-AMT ROUNDED =
                   CRCM-PAYMENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCT-MIN-DUE-AMT
           END-IF
           ADD CRCT-MIN-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3930-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCT-PAYMENT-AMT ROUNDED =
                   CRCM-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCT-PAYMENT-AMT
           END-IF
           ADD CRCT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * LIMIT YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3940-LIMIT-LIMIT.
           IF CRCM-LIMIT-YTD < 50.00
               MOVE 50.00 TO CRCM-LIMIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-LIMIT-YTD > 50000.00
               MOVE 50000.00 TO CRCM-LIMIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ADV YTD FEE ON THE PAYMENT YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3950-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CRCM-CASH-ADV-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CRCM-CASH-ADV-YTD ROUNDED = CRCM-PAYMENT-YTD
                       * CRCM-PENALTY-PCT / 200
               WHEN CRCM-TYPE-CD = 'GP'
                   MOVE 7.00 TO CRCM-CASH-ADV-YTD
               WHEN CRCM-TYPE-CD = 'UV'
                   MOVE 7.00 TO CRCM-CASH-ADV-YTD
               WHEN OTHER
                   COMPUTE CRCM-CASH-ADV-YTD ROUNDED = CRCM-PAYMENT-YTD
                       * CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-CASH-ADV-YTD > 0 AND CRCM-CASH-ADV-YTD < 1.00
               MOVE 1.00 TO CRCM-CASH-ADV-YTD
           END-IF
           IF CRCM-CASH-ADV-YTD > 35.00
               MOVE 35.00 TO CRCM-CASH-ADV-YTD
           END-IF
           ADD CRCM-CASH-ADV-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ADV FEE ON THE MIN DUE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3960-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CRCM-CASH-ADV-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE CRCM-CASH-ADV-AMT ROUNDED = CRCT-MIN-DUE-AMT
                       * CRCM-DISCOUNT / 200
               WHEN CRCM-TYPE-CD = 'GP'
                   MOVE 3.00 TO CRCM-CASH-ADV-AMT
               WHEN CRCM-TYPE-CD = 'UV'
                   MOVE 12.00 TO CRCM-CASH-ADV-AMT
               WHEN OTHER
                   COMPUTE CRCM-CASH-ADV-AMT ROUNDED = CRCT-MIN-DUE-AMT
                       * CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCM-CASH-ADV-AMT > 0 AND CRCM-CASH-ADV-AMT < 10.00
               MOVE 10.00 TO CRCM-CASH-ADV-AMT
           END-IF
           IF CRCM-CASH-ADV-AMT > 75.00
               MOVE 75.00 TO CRCM-CASH-ADV-AMT
           END-IF
           ADD CRCM-CASH-ADV-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3970-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE CRCM-LATE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PURCHASE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3980-ACCRUE-PURCHASE.
           IF CRCM-PURCHASE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PURCHASE-AMT * CRCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CRCM-FINANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3990-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CRCM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT MIN DUE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-MIN-DUE.
           COMPUTE WS-WORK-AMT = CRCM-MIN-DUE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CRCM-MIN-DUE-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E936 CUST ID REQUIRED
      * E714 MIN DUE AMT NEGATIVE
      * E492 MIN DUE AMT OVER 50000
      * E751 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CUST-ID = SPACES
                   MOVE 'E936' TO WS-REJECT-CODE
               WHEN CRCM-MIN-DUE-AMT < 0
                   MOVE 'E714' TO WS-REJECT-CODE
               WHEN CRCM-MIN-DUE-AMT > 50000.00
                   MOVE 'E492' TO WS-REJECT-CODE
               WHEN CRCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E751' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4020-COUNT-SEQ.
           IF CRCT-SEQ < 9999999
               ADD 1 TO CRCT-SEQ
           END-IF
           IF CRCT-SEQ NOT < 12
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4030-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CRCT-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE FINANCE YTD FEE ON THE LIMIT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4040-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CRCM-FINANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CRCM-FINANCE-YTD ROUNDED = CRCT-LIMIT-AMT *
                       CRCM-RATE / 200
               WHEN CRCT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO CRCM-FINANCE-YTD
               WHEN CRCT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO CRCM-FINANCE-YTD
               WHEN OTHER
                   COMPUTE CRCM-FINANCE-YTD ROUNDED = CRCT-LIMIT-AMT *
                       CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-FINANCE-YTD > 0 AND CRCM-FINANCE-YTD < 5.00
               MOVE 5.00 TO CRCM-FINANCE-YTD
           END-IF
           IF CRCM-FINANCE-YTD > 100.00
               MOVE 100.00 TO CRCM-FINANCE-YTD
           END-IF
           ADD CRCM-FINANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4050-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-STATUS-CD
                   ADD CRCM-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4060-DESCRIBE-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CASH ADV YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4070-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = CRCM-CASH-ADV-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CRCM-CASH-ADV-YTD - (WS-WORK-AMT * 3)
           COMPUTE CRCT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4080-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LIMIT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4090-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRCT-LIMIT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CRCT-LIMIT-AMT - (WS-WORK-AMT * 3)
           COMPUTE CRCM-LATE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV TIER TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4100-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-TIER-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-CASH-ADV-TIER-LOW(WS-TX)
                   MOVE WS-CASH-ADV-TIER-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4110-COUNT-SEQ.
           IF CRCT-SEQ < 9999999
               ADD 1 TO CRCT-SEQ
           END-IF
           IF CRCT-SEQ NOT < 3
               MOVE 'Y' TO CRCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       4120-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-PAYMENT-AMT ROUNDED =
                   CRCM-PURCHASE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-PAYMENT-AMT
           END-IF
           ADD CRCM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E867 REJECT CD REQUIRED
      * E192 PAYMENT AMT NEGATIVE
      * E296 PAYMENT AMT OVER 10000
      * E784 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4130-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCT-REJECT-CD = SPACES
                   MOVE 'E867' TO WS-REJECT-CODE
               WHEN CRCT-PAYMENT-AMT < 0
                   MOVE 'E192' TO WS-REJECT-CODE
               WHEN CRCT-PAYMENT-AMT > 10000.00
                   MOVE 'E296' TO WS-REJECT-CODE
               WHEN CRCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E784' TO WS-REJECT-CODE
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
           IF CRCM-CYCLE-CNT < 9999999
               ADD 1 TO CRCM-CYCLE-CNT
           END-IF
           IF CRCM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * C -> H WHEN EFFECTIVE DATE IS PAST
      * H -> M WHEN EFFECTIVE DATE IS PAST
      * M -> R WHEN EFFECTIVE DATE IS PAST
      * R -> S WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4150-SET-SOURCE.
           EVALUATE CRCM-SOURCE-CD
               WHEN 'C'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LIMIT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4160-FLAG-LIMIT.
           IF CRCM-LIMIT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCM-LIMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * GP -> UV WHEN LAST PAY DATE IS PAST
      * UV -> YK WHEN LAST PAY DATE IS PAST
      * YK -> GP WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4170-SET-TYPE.
           EVALUATE CRCM-TYPE-CD
               WHEN 'GP'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YK' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YK'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GP' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4180-COUNT-SEQ.
           IF CRCT-SEQ < 9999999
               ADD 1 TO CRCT-SEQ
           END-IF
           IF CRCT-SEQ NOT < 3
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PURCHASE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4190-ACCRUE-PURCHASE.
           IF CRCM-PURCHASE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PURCHASE-AMT * CRCM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ADV AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4200-ACCRUE-CASH-ADV.
           IF CRCM-CASH-ADV-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-CASH-ADV-AMT * CRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CRCM-MIN-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN EFF DATE IS PAST
      * DDY -> DPB WHEN EFF DATE IS PAST
      * DPB -> UHF WHEN EFF DATE IS PAST
      * UHF -> WLL WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4220-SPLIT-MIN-DUE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE CRCT-MIN-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4230-DESCRIBE-SOURCE.
           EVALUATE CRCM-SOURCE-CD
               WHEN 'C'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN DUE YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4240-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-SOURCE-CD
                   ADD CRCM-MIN-DUE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-MIN-DUE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-PAYMENT.
           IF CRCM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PAYMENT-YTD * CRCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO CRCM-PURCHASE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ADV FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4260-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CRCM-CASH-ADV-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CRCM-CASH-ADV-AMT ROUNDED = WS-WORK-AMT-2 *
                       CRCM-DISCOUNT / 200
               WHEN CRCM-BILL-CYCLE-CD = 'B'
                   MOVE 20.00 TO CRCM-CASH-ADV-AMT
               WHEN CRCM-BILL-CYCLE-CD = 'D'
                   MOVE 3.00 TO CRCM-CASH-ADV-AMT
               WHEN OTHER
                   COMPUTE CRCM-CASH-ADV-AMT ROUNDED = WS-WORK-AMT-2 *
                       CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCM-CASH-ADV-AMT > 0 AND CRCM-CASH-ADV-AMT < 10.00
               MOVE 10.00 TO CRCM-CASH-ADV-AMT
           END-IF
           IF CRCM-CASH-ADV-AMT > 250.00
               MOVE 250.00 TO CRCM-CASH-ADV-AMT
           END-IF
           ADD CRCM-CASH-ADV-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * C -> H WHEN EFF DATE IS PAST
      * H -> M WHEN EFF DATE IS PAST
      * M -> R WHEN EFF DATE IS PAST
      * R -> S WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4270-SET-SOURCE.
           EVALUATE CRCM-SOURCE-CD
               WHEN 'C'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE MIN DUE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4280-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CRCM-PAYMENT-AMT ROUNDED = CRCM-MIN-DUE-AMT *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCM-BRANCH-CD = 'CTB'
                   MOVE 15.00 TO CRCM-PAYMENT-AMT
               WHEN CRCM-BRANCH-CD = 'LNX'
                   MOVE 12.00 TO CRCM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRCM-PAYMENT-AMT ROUNDED = CRCM-MIN-DUE-AMT *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-PAYMENT-AMT > 0 AND CRCM-PAYMENT-AMT < 2.00
               MOVE 2.00 TO CRCM-PAYMENT-AMT
           END-IF
           IF CRCM-PAYMENT-AMT > 100.00
               MOVE 100.00 TO CRCM-PAYMENT-AMT
           END-IF
           ADD CRCM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PURCHASE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4290-ACCRUE-PURCHASE.
           IF CRCM-PURCHASE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PURCHASE-AMT * CRCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO CRCM-MIN-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4300-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 10
               MOVE 'Y' TO CRCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FINANCE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4310-ACCRUE-FINANCE.
           IF CRCM-FINANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-FINANCE-YTD * CRCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CRCM-CASH-ADV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E296 REF NO REQUIRED
      * E792 LIMIT AMT NEGATIVE
      * E282 LIMIT AMT OVER 99999
      * E589 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4320-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-REF-NO = SPACES
                   MOVE 'E296' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-AMT < 0
                   MOVE 'E792' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-AMT > 99999.00
                   MOVE 'E282' TO WS-REJECT-CODE
               WHEN CRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E589' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E410 DESC REQUIRED
      * E743 LATE FEE AMT NEGATIVE
      * E749 LATE FEE AMT OVER 1000000
      * E330 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4330-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-DESC = SPACES
                   MOVE 'E410' TO WS-REJECT-CODE
               WHEN CRCM-LATE-FEE-AMT < 0
                   MOVE 'E743' TO WS-REJECT-CODE
               WHEN CRCM-LATE-FEE-AMT > 1000000.00
                   MOVE 'E749' TO WS-REJECT-CODE
               WHEN CRCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4340-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4350-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO CRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE FEE ON THE CASH ADV YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4360-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CRCM-MIN-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   COMPUTE CRCM-MIN-DUE-AMT ROUNDED = CRCM-CASH-ADV-YTD
                       * CRCM-RATE / 200
               WHEN CRCM-SOURCE-CD = 'C'
                   MOVE 3.00 TO CRCM-MIN-DUE-AMT
               WHEN CRCM-SOURCE-CD = 'H'
                   MOVE 10.00 TO CRCM-MIN-DUE-AMT
               WHEN OTHER
                   COMPUTE CRCM-MIN-DUE-AMT ROUNDED = CRCM-CASH-ADV-YTD
                       * CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-MIN-DUE-AMT > 0 AND CRCM-MIN-DUE-AMT < 5.00
               MOVE 5.00 TO CRCM-MIN-DUE-AMT
           END-IF
           IF CRCM-MIN-DUE-AMT > 250.00
               MOVE 250.00 TO CRCM-MIN-DUE-AMT
           END-IF
           ADD CRCM-MIN-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4370-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCT-CHANNEL-CD
                   ADD CRCT-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCT-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MIN DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       4380-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-PAYMENT-YTD ROUNDED =
                   CRCT-MIN-DUE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-PAYMENT-YTD
           END-IF
           ADD CRCM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E893 REF NO REQUIRED
      * E064 LATE FEE AMT NEGATIVE
      * E580 LATE FEE AMT OVER 250000
      * E274 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4390-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-REF-NO = SPACES
                   MOVE 'E893' TO WS-REJECT-CODE
               WHEN CRCM-LATE-FEE-AMT < 0
                   MOVE 'E064' TO WS-REJECT-CODE
               WHEN CRCM-LATE-FEE-AMT > 250000.00
                   MOVE 'E580' TO WS-REJECT-CODE
               WHEN CRCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E274' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4400-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCT-LIMIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN DUE YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4410-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-SOURCE-CD
                   ADD CRCM-MIN-DUE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-MIN-DUE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4420-COUNT-ITEM.
           IF CRCM-ITEM-CNT < 9999999
               ADD 1 TO CRCM-ITEM-CNT
           END-IF
           IF CRCM-ITEM-CNT NOT < 3
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH ADV YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4430-FLAG-CASH-ADV.
           IF CRCM-CASH-ADV-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCM-CASH-ADV-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE PURCHASE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4440-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRCT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CRCT-PAYMENT-AMT ROUNDED = CRCM-PURCHASE-YTD
                       * CRCM-PENALTY-PCT / 200
               WHEN CRCM-BILL-CYCLE-CD = 'B'
                   MOVE 3.00 TO CRCT-PAYMENT-AMT
               WHEN CRCM-BILL-CYCLE-CD = 'D'
                   MOVE 20.00 TO CRCT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRCT-PAYMENT-AMT ROUNDED = CRCM-PURCHASE-YTD
                       * CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCT-PAYMENT-AMT > 0 AND CRCT-PAYMENT-AMT < 2.00
               MOVE 2.00 TO CRCT-PAYMENT-AMT
           END-IF
           IF CRCT-PAYMENT-AMT > 150.00
               MOVE 150.00 TO CRCT-PAYMENT-AMT
           END-IF
           ADD CRCT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
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
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E883 REF NO REQUIRED
      * E151 LIMIT YTD NEGATIVE
      * E841 LIMIT YTD OVER 10000
      * E038 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4460-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-REF-NO = SPACES
                   MOVE 'E883' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-YTD < 0
                   MOVE 'E151' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-YTD > 10000.00
                   MOVE 'E841' TO WS-REJECT-CODE
               WHEN CRCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E038' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4470-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-SOURCE-CD
                   ADD CRCT-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCT-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ADV YTD BY BILL CYCLE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4480-ACCUM-BILL-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-BILL-CYCLE-CD
                   ADD CRCM-CASH-ADV-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-BILL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-CASH-ADV-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BILL CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4490-DESCRIBE-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'E'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'F'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'P'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * C -> H WHEN LAST PAY DATE IS PAST
      * H -> M WHEN LAST PAY DATE IS PAST
      * M -> R WHEN LAST PAY DATE IS PAST
      * R -> S WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4500-SET-SOURCE.
           EVALUATE CRCM-SOURCE-CD
               WHEN 'C'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4510-FLAG-PURCHASE.
           IF CRCM-PURCHASE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCM-PURCHASE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CASH ADV AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4520-LIMIT-CASH-ADV.
           IF CRCM-CASH-ADV-AMT < 0.00
               MOVE 0.00 TO CRCM-CASH-ADV-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-CASH-ADV-AMT > 500.00
               MOVE 500.00 TO CRCM-CASH-ADV-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4530-FLAG-LIMIT.
           IF CRCT-LIMIT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCT-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E214 CONTACT REQUIRED
      * E599 PAYMENT YTD NEGATIVE
      * E632 PAYMENT YTD OVER 250000
      * E014 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4540-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CONTACT = SPACES
                   MOVE 'E214' TO WS-REJECT-CODE
               WHEN CRCM-PAYMENT-YTD < 0
                   MOVE 'E599' TO WS-REJECT-CODE
               WHEN CRCM-PAYMENT-YTD > 250000.00
                   MOVE 'E632' TO WS-REJECT-CODE
               WHEN CRCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E014' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4550-DESCRIBE-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4560-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE CRCT-CASH-ADV-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT SCHED TIER FOR THE CASH ADV YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4570-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-SCHED-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CRCM-CASH-ADV-YTD NOT < WS-PAYMENT-SCHED-LOW(WS-TX)
                   MOVE WS-PAYMENT-SCHED-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E027 REJECT CD REQUIRED
      * E147 FINANCE AMT NEGATIVE
      * E579 FINANCE AMT OVER 25000
      * E286 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       4580-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCT-REJECT-CD = SPACES
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN CRCM-FINANCE-AMT < 0
                   MOVE 'E147' TO WS-REJECT-CODE
               WHEN CRCM-FINANCE-AMT > 25000.00
                   MOVE 'E579' TO WS-REJECT-CODE
               WHEN CRCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E286' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4590-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-PURCHASE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-PURCHASE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-PURCHASE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCM-PURCHASE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-PURCHASE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4600-DESCRIBE-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4610-COUNT-ITEM.
           IF CRCM-ITEM-CNT < 9999999
               ADD 1 TO CRCM-ITEM-CNT
           END-IF
           IF CRCM-ITEM-CNT NOT < 5
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4620-FLAG-PAYMENT.
           IF CRCM-PAYMENT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ADV YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH ADV AMT.
      *----------------------------------------------------------------*
       4630-PRORATE-CASH-ADV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCT-CASH-ADV-AMT ROUNDED =
                   CRCM-CASH-ADV-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCT-CASH-ADV-AMT
           END-IF
           ADD CRCT-CASH-ADV-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4640-COUNT-CYCLE.
           IF CRCM-CYCLE-CNT < 9999999
               ADD 1 TO CRCM-CYCLE-CNT
           END-IF
           IF CRCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY BILL CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4650-ACCUM-BILL-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-BILL-CYCLE-CD
                   ADD CRCT-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-BILL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCT-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E387 NAME REQUIRED
      * E492 FINANCE AMT NEGATIVE
      * E583 FINANCE AMT OVER 1000000
      * E620 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4660-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-NAME = SPACES
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN CRCM-FINANCE-AMT < 0
                   MOVE 'E492' TO WS-REJECT-CODE
               WHEN CRCM-FINANCE-AMT > 1000000.00
                   MOVE 'E583' TO WS-REJECT-CODE
               WHEN CRCM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E620' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4670-CHECK-MATURITY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4680-AGE-MATURITY.
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
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4690-LIMIT-LIMIT.
           IF CRCM-LIMIT-AMT < 50.00
               MOVE 50.00 TO CRCM-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-LIMIT-AMT > 1000.00
               MOVE 1000.00 TO CRCM-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN DUE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4700-ACCUM-CHANNEL.
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
      * PRORATE LATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       4710-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-PAYMENT-AMT ROUNDED =
                   CRCM-LATE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-PAYMENT-AMT
           END-IF
           ADD CRCM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4720-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-SCHED-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF WS-WORK-AMT NOT < WS-PAYMENT-SCHED-LOW(WS-TX)
                   MOVE WS-PAYMENT-SCHED-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4730-COUNT-SEQ.
           IF CRCT-SEQ < 9999999
               ADD 1 TO CRCT-SEQ
           END-IF
           IF CRCT-SEQ NOT < 6
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE CRC FIELDS TO CRELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-CRELG.
           INITIALIZE CRELG-PARMS
           MOVE CRCM-LIMIT-YTD
             TO CRELG-LIMIT-YTD
           MOVE CRCM-PURCHASE-AMT
             TO CRELG-PURCHASE-AMT
           MOVE CRCM-MIN-DUE-YTD
             TO CRELG-MIN-DUE-YTD
           MOVE CRCM-LATE-FEE-AMT
             TO CRELG-LATE-FEE-AMT
           MOVE CRCM-CASH-ADV-AMT
             TO CRELG-CASH-ADV-AMT
           MOVE CRCM-LIMIT-AMT
             TO CRELG-LIMIT-AMT
           MOVE CRCM-LAST-ACT-DATE
             TO CRELG-LAST-ACT-DATE
           MOVE CRCM-OPEN-DATE
             TO CRELG-OPEN-DATE
           MOVE CRCM-LAST-PAY-DATE
             TO CRELG-LAST-PAY-DATE
           MOVE CRCM-MATURITY-DATE
             TO CRELG-MATURITY-DATE
           MOVE CRCM-TYPE-CD
             TO CRELG-TYPE-CD
           MOVE CRCM-CARD-TYPE-CD
             TO CRELG-CARD-TYPE-CD
           MOVE CRCM-BILL-CYCLE-CD
             TO CRELG-BILL-CYCLE-CD
           MOVE CRCM-SOURCE-CD
             TO CRELG-SOURCE-CD
           MOVE CRCM-EXEMPT-FLAG
             TO CRELG-EXEMPT-FLAG
           MOVE CRCM-AUTO-RENEW-FLAG
             TO CRELG-AUTO-RENEW-FLAG
           MOVE CRCM-STOP-FLAG
             TO CRELG-STOP-FLAG
           MOVE CRCM-CYCLE-CNT
             TO CRELG-CYCLE-CNT
           MOVE CRCM-RETRY-CNT
             TO CRELG-RETRY-CNT
           MOVE CRCM-DISCOUNT
             TO CRELG-DISCOUNT
           MOVE CRCM-RATE
             TO CRELG-RATE
           MOVE CRCM-DESC
             TO CRELG-DESC
           MOVE CRCM-ADDR
             TO CRELG-ADDR
           CALL 'CRELG' USING CRELG-PARMS
           EVALUATE CRELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CRELG-LIMIT-YTD
                     TO CRCM-LIMIT-YTD
                   MOVE CRELG-PURCHASE-AMT
                     TO CRCM-PURCHASE-AMT
                   MOVE CRELG-MIN-DUE-YTD
                     TO CRCM-MIN-DUE-YTD
                   MOVE CRELG-LATE-FEE-AMT
                     TO CRCM-LATE-FEE-AMT
                   MOVE CRELG-AUTO-RENEW-FLAG
                     TO CRCM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-CRELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE CRC FIELDS TO CRVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-CRVAL.
           INITIALIZE CRVAL-PARMS
           MOVE CRCM-FINANCE-YTD
             TO CRVAL-FINANCE-YTD
           MOVE CRCM-LIMIT-AMT
             TO CRVAL-LIMIT-AMT
           MOVE CRCM-LATE-FEE-YTD
             TO CRVAL-LATE-FEE-YTD
           MOVE CRCM-MIN-DUE-AMT
             TO CRVAL-MIN-DUE-AMT
           MOVE CRCM-MIN-DUE-YTD
             TO CRVAL-MIN-DUE-YTD
           MOVE CRCM-PAYMENT-AMT
             TO CRVAL-PAYMENT-AMT
           MOVE CRCM-OPEN-DATE
             TO CRVAL-OPEN-DATE
           MOVE CRCM-MATURITY-DATE
             TO CRVAL-MATURITY-DATE
           MOVE CRCM-EXPIRY-DATE
             TO CRVAL-EXPIRY-DATE
           MOVE CRCM-EFFECTIVE-DATE
             TO CRVAL-EFFECTIVE-DATE
           MOVE CRCM-TYPE-CD
             TO CRVAL-TYPE-CD
           MOVE CRCM-STATUS-CD
             TO CRVAL-STATUS-CD
           MOVE CRCM-BRANCH-CD
             TO CRVAL-BRANCH-CD
           MOVE CRCM-BILL-CYCLE-CD
             TO CRVAL-BILL-CYCLE-CD
           MOVE CRCM-STOP-FLAG
             TO CRVAL-STOP-FLAG
           MOVE CRCM-PREFERRED-FLAG
             TO CRVAL-PREFERRED-FLAG
           MOVE CRCM-OVERRIDE-FLAG
             TO CRVAL-OVERRIDE-FLAG
           MOVE CRCM-CYCLE-CNT
             TO CRVAL-CYCLE-CNT
           MOVE CRCM-NOTICE-CNT
             TO CRVAL-NOTICE-CNT
           MOVE CRCM-DISCOUNT
             TO CRVAL-DISCOUNT
           MOVE CRCM-PENALTY-PCT
             TO CRVAL-PENALTY-PCT
           MOVE CRCM-CONTACT
             TO CRVAL-CONTACT
           MOVE CRCM-DESC
             TO CRVAL-DESC
           CALL 'CRVAL' USING CRVAL-PARMS
           EVALUATE CRVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CRVAL-FINANCE-YTD
                     TO CRCM-FINANCE-YTD
                   MOVE CRVAL-LATE-FEE-YTD
                     TO CRCM-LATE-FEE-YTD
                   MOVE CRVAL-MIN-DUE-YTD
                     TO CRCM-MIN-DUE-YTD
                   MOVE CRVAL-PAYMENT-AMT
                     TO CRCM-PAYMENT-AMT
                   MOVE CRVAL-PREFERRED-FLAG
                     TO CRCM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-CRVAL' TO ABND-PARAGRAPH
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
                   MOVE CRDTE-FINANCE-AMT
                     TO CRCM-FINANCE-AMT
                   MOVE CRDTE-LATE-FEE-YTD
                     TO CRCM-LATE-FEE-YTD
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
           DISPLAY 'CRCPOST READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CRCPOST' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
