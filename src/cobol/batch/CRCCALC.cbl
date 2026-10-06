      *================================================================*
      * PROGRAM  : CRCCALC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PROPRIETARY CREDIT CARD - PERIODIC CALCULATION.     *
      *            READS THE CRC TRANSACTIONS, UPDATES THE CRC MASTER  *
      *            AND PRODUCES THE CONTROL REPORT. IN-HOUSE CREDIT    *
      *            CARD BILLING, FINANCE CHARGE AND DELINQUENCY        *
      * INPUT    : CRCMST (KSDS), CRCTRN, BANKCTL                      *
      * OUTPUT   : CRCOUT, CRCCARPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CRVAL                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-03-26 PQR  ORIGINAL                                       *
      * 1995-03-28 PQR  PERFORMANCE - MIN-DUE READ AHEAD PRB1488       *
      * 2001-03-17 LMB  CASH-ADV RERUN PROTECTION CHG2672              *
      * 2002-09-06 RJT  PAYMENT TABLE EXPANDED REQ0581                 *
      * 2003-01-05 PQR  MIN-DUE TABLE EXPANDED INC7265                 *
      * 2005-01-02 JLM  PERFORMANCE - CASH-ADV READ AHEAD REQ2393      *
      * 2007-12-17 DPS  LIMIT LIMIT REVISED INC6779                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRCCALC.
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
           SELECT REPORT-FILE ASSIGN TO "CRCCARPT"
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
                                           VALUE 22.000000.
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
      * WS-PAYMENT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000257500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000186000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000592500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000702000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016100000000242500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021100000000739500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021150000000411000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021650000000204500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026650000000855000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031650000000270000'.
       01  WS-PAYMENT-TIER-TBL REDEFINES WS-PAYMENT-TIER-VALUES.
           05  WS-PAYMENT-TIER-ROW OCCURS 10 TIMES.
               10  WS-PAYMENT-TIER-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYMENT-TIERX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-TIERX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000627000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000883500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000259000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012000000000297500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014500000000268000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014600000000045000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015100000000418000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020100000000218000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021100000000448500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031100000000434500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00041100000000506000'.
       01  WS-PAYMENT-TIERX-TBL REDEFINES WS-PAYMENT-TIERX-VALUES.
           05  WS-PAYMENT-TIERX-ROW OCCURS 11 TIMES.
               10  WS-PAYMENT-TIERX-LOW    PIC 9(09)V99.
               10  WS-PAYMENT-TIERX-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FINANCE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FINANCE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000731000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000717000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000811500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030000000000054000'.
       01  WS-FINANCE-SCHED-TBL REDEFINES WS-FINANCE-SCHED-VALUES.
           05  WS-FINANCE-SCHED-ROW OCCURS 4 TIMES.
               10  WS-FINANCE-SCHED-LOW    PIC 9(09)V99.
               10  WS-FINANCE-SCHED-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CRCCALC  PROPRIETARY CREDIT CARD'.
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
       COPY CRVALP.

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
               PERFORM 3000-SET-TRAN
               PERFORM 3010-AGE-NOTICE
               PERFORM 3020-ACCUM-BRANCH
               PERFORM 3030-AGE-CLOSE
               PERFORM 3040-ASSESS-AUTO-RENEW
               PERFORM 3050-SPLIT-LIMIT
               PERFORM 3060-PRORATE-LIMIT
               PERFORM 3070-SELECT-DISCOUNT
               PERFORM 3080-ASSESS-NOTICE-SENT
               PERFORM 3090-SELECT-PENALTY-PCT
               PERFORM 3100-AGE-MATURITY
               PERFORM 3110-AGE-EXPIRY
               PERFORM 3120-ACCRUE-LATE-FEE
               PERFORM 3130-AGE-LAST-ACT
               PERFORM 3140-SET-CARD-TYPE
               PERFORM 3150-SPLIT-LIMIT
               PERFORM 3160-SELECT-DISCOUNT
               PERFORM 3170-DESCRIBE-CHANNEL
               PERFORM 3180-LIMIT-PURCHASE
               PERFORM 3190-FLAG-ITEM
               PERFORM 3200-DESCRIBE-STATUS
               PERFORM 3210-SPLIT-CASH-ADV
               PERFORM 3220-CHECK-EXPIRY
               PERFORM 3230-CHECK-NEXT-REVIEW
               PERFORM 3240-ASSESS-REVERSAL
               PERFORM 3250-SELECT-RATE
               PERFORM 3260-ASSESS-PREFERRED
               PERFORM 3270-SET-STATUS
               PERFORM 3280-ACCRUE-PAYMENT
               PERFORM 3290-DESCRIBE-TYPE
               PERFORM 3300-SPLIT-CASH-ADV
               PERFORM 3310-SET-BRANCH
               PERFORM 3320-SET-CARD-TYPE
               PERFORM 3330-DESCRIBE-CARD-TYPE
               PERFORM 3340-ASSESS-AUTO-RENEW
               PERFORM 3350-SPLIT-LIMIT
               PERFORM 3360-SET-TRAN
               PERFORM 3370-COUNT-NOTICE
               PERFORM 3380-LIMIT-CASH-ADV
               PERFORM 3390-SELECT-RATE
               PERFORM 3400-SPLIT-LATE-FEE
               PERFORM 3410-ACCUM-CHANNEL
               PERFORM 3420-DESCRIBE-STATUS
               PERFORM 3430-CHECK-NOTICE
               PERFORM 3440-CHECK-EXPIRY
               PERFORM 3450-DESCRIBE-CARD-TYPE
               PERFORM 3460-CHECK-CLOSE
               PERFORM 3470-COUNT-NOTICE
               PERFORM 3480-AGE-CLOSE
               PERFORM 3490-CHECK-EFF
               PERFORM 3500-EDIT-CONTACT
               PERFORM 3510-AGE-MATURITY
               PERFORM 3520-DESCRIBE-TRAN
               PERFORM 3530-PRORATE-MIN-DUE
               PERFORM 3540-PRORATE-LIMIT
               PERFORM 3550-SELECT-DISCOUNT
               PERFORM 3560-ACCRUE-PENALTY-PCT
               PERFORM 3570-EDIT-CONTACT
               PERFORM 3580-ASSESS-PREFERRED
               PERFORM 3590-CHECK-CLOSE
               PERFORM 3600-DESCRIBE-SOURCE
               PERFORM 3610-AGE-POST
               PERFORM 3620-SPLIT-PAYMENT
               PERFORM 3630-DESCRIBE-CHANNEL
               PERFORM 3640-COUNT-NOTICE
               PERFORM 3650-ACCUM-STATUS
               PERFORM 3660-CHECK-CLOSE
               PERFORM 3670-FLAG-LIMIT
               PERFORM 3680-EDIT-NAME
               PERFORM 3690-SELECT-DISCOUNT
               PERFORM 3700-ACCUM-CHANNEL
               PERFORM 3710-SET-CHANNEL
               PERFORM 3720-COUNT-RETRY
               PERFORM 3730-PRORATE-CASH-ADV
               PERFORM 3740-CHECK-NEXT-REVIEW
               PERFORM 3750-SELECT-RATE
               PERFORM 3760-ASSESS-NOTICE-SENT
               PERFORM 3770-FLAG-MIN-DUE
               PERFORM 3780-ASSESS-REVERSAL
               PERFORM 3790-CHECK-POST
               PERFORM 3800-AGE-NOTICE
               PERFORM 3810-PRORATE-PURCHASE
               PERFORM 3820-ACCRUE-LIMIT
               PERFORM 3830-EDIT-CONTACT
               PERFORM 3840-ACCUM-CARD-TYPE
               PERFORM 3850-SET-BRANCH
               PERFORM 3860-AGE-EXPIRY
               PERFORM 3870-EDIT-CONTACT
               PERFORM 3880-SPLIT-PURCHASE
               PERFORM 3890-COUNT-RETRY
               PERFORM 3900-ACCRUE-PAYMENT
               PERFORM 3910-FLAG-PAYMENT
               PERFORM 3920-COUNT-RETRY
               PERFORM 3930-DESCRIBE-BRANCH
               PERFORM 3940-ACCRUE-LIMIT
               PERFORM 3950-SET-TRAN
               PERFORM 3960-FLAG-PAYMENT
               PERFORM 3970-PRORATE-LIMIT
               PERFORM 3980-ACCRUE-MIN-DUE
               PERFORM 3990-SET-BILL-CYCLE
               PERFORM 4000-PRORATE-PURCHASE
               PERFORM 4010-SELECT-RATE
               PERFORM 4020-PRORATE-MIN-DUE
               PERFORM 4030-ACCRUE-LATE-FEE
               PERFORM 4040-FLAG-PURCHASE
               PERFORM 4050-CHECK-EXPIRY
               PERFORM 4060-COUNT-ITEM
               PERFORM 4070-AGE-EFF
               PERFORM 4080-CHECK-LAST-ACT
               PERFORM 4090-SPLIT-CASH-ADV
               PERFORM 4100-PRORATE-LIMIT
               PERFORM 4110-SET-SOURCE
               PERFORM 4120-SET-BRANCH
               PERFORM 4130-ACCRUE-LIMIT
               PERFORM 4140-COUNT-CYCLE
               PERFORM 4150-CHECK-POST
               PERFORM 4160-LIMIT-FINANCE
               PERFORM 4170-CHECK-CLOSE
               PERFORM 4180-ACCRUE-CASH-ADV
               PERFORM 4190-SELECT-PENALTY-PCT
               PERFORM 4200-LIMIT-LIMIT
               PERFORM 4210-SET-BRANCH
               PERFORM 4220-EDIT-REF-NO
               PERFORM 4230-ACCUM-TYPE
               PERFORM 4240-ACCRUE-LIMIT
               PERFORM 4250-SPLIT-FINANCE
               PERFORM 4260-DESCRIBE-CHANNEL
               PERFORM 4270-AGE-EFFECTIVE
               PERFORM 4280-LIMIT-PAYMENT
               PERFORM 4290-LIMIT-LIMIT
               PERFORM 4300-CHECK-NEXT-REVIEW
               PERFORM 4310-EDIT-REF-NO
               PERFORM 4320-EDIT-CONTACT
               PERFORM 4330-DESCRIBE-STATUS
               PERFORM 4340-PRORATE-LIMIT
               PERFORM 4350-ACCUM-CHANNEL
               PERFORM 4360-EDIT-MEMO
               PERFORM 4370-FLAG-LIMIT
               PERFORM 4380-AGE-EXPIRY
               PERFORM 4390-ASSESS-PREFERRED
               PERFORM 4400-ACCRUE-PAYMENT
               PERFORM 4410-ACCRUE-MIN-DUE
               PERFORM 4420-ASSESS-PREFERRED
               PERFORM 4430-AGE-LAST-PAY
               PERFORM 4440-SELECT-RATE
               PERFORM 4450-COUNT-NOTICE
               PERFORM 4460-ACCRUE-PAYMENT
               PERFORM 4470-SET-BRANCH
               PERFORM 4480-COUNT-RETRY
               PERFORM 4490-AGE-NOTICE
               PERFORM 4500-AGE-LAST-ACT
               PERFORM 4510-COUNT-ITEM
               PERFORM 4520-SPLIT-LATE-FEE
               PERFORM 4530-PRORATE-MIN-DUE
               PERFORM 4540-PRORATE-FINANCE
               PERFORM 4550-SPLIT-PAYMENT
               PERFORM 4560-ACCRUE-PAYMENT
               PERFORM 7010-CALL-CRVAL
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
      * STATUS TRANSITIONS FOR TRAN CD:
      * PU -> CA WHEN EXPIRY DATE IS PAST
      * CA -> PY WHEN EXPIRY DATE IS PAST
      * PY -> FC WHEN EXPIRY DATE IS PAST
      * FC -> LF WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3000-SET-TRAN.
           EVALUATE CRCT-TRAN-CD
               WHEN 'PU'
                   IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CA' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CA'
                   IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FC' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FC'
                   IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LF' TO CRCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-NOTICE.
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
                   ADD CRCT-LIMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD CRCT-LIMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCT-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCT-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN DUE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3020-ACCUM-BRANCH.
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
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-CLOSE.
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
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE YTD FEE ON THE CASH ADV AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3040-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PURCHASE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   COMPUTE CRCM-PURCHASE-YTD ROUNDED = CRCT-CASH-ADV-AMT
                       * CRCM-PENALTY-PCT / 200
               WHEN CRCM-SOURCE-CD = 'C'
                   MOVE 7.00 TO CRCM-PURCHASE-YTD
               WHEN CRCM-SOURCE-CD = 'H'
                   MOVE 12.00 TO CRCM-PURCHASE-YTD
               WHEN OTHER
                   COMPUTE CRCM-PURCHASE-YTD ROUNDED = CRCT-CASH-ADV-AMT
                       * CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-PURCHASE-YTD > 0 AND CRCM-PURCHASE-YTD < 5.00
               MOVE 5.00 TO CRCM-PURCHASE-YTD
           END-IF
           IF CRCM-PURCHASE-YTD > 35.00
               MOVE 35.00 TO CRCM-PURCHASE-YTD
           END-IF
           ADD CRCM-PURCHASE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT LIMIT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRCM-LIMIT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CRCM-LIMIT-YTD - (WS-WORK-AMT * 3)
           COMPUTE CRCM-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       3060-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-LIMIT-AMT ROUNDED =
                   CRCM-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-LIMIT-AMT
           END-IF
           ADD CRCM-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FINANCE SCHED TIER FOR THE MIN DUE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FINANCE-SCHED-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCM-MIN-DUE-YTD NOT < WS-FINANCE-SCHED-LOW(WS-TX)
                   MOVE WS-FINANCE-SCHED-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE YTD FEE ON THE PURCHASE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3080-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CRCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PURCHASE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   COMPUTE CRCM-PURCHASE-YTD ROUNDED = CRCM-PURCHASE-AMT
                       * CRCM-DISCOUNT / 200
               WHEN CRCT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO CRCM-PURCHASE-YTD
               WHEN CRCT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO CRCM-PURCHASE-YTD
               WHEN OTHER
                   COMPUTE CRCM-PURCHASE-YTD ROUNDED = CRCM-PURCHASE-AMT
                       * CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCM-PURCHASE-YTD > 0 AND CRCM-PURCHASE-YTD < 2.00
               MOVE 2.00 TO CRCM-PURCHASE-YTD
           END-IF
           IF CRCM-PURCHASE-YTD > 35.00
               MOVE 35.00 TO CRCM-PURCHASE-YTD
           END-IF
           ADD CRCM-PURCHASE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT TIER TIER FOR THE MIN DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3090-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-TIER-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CRCT-MIN-DUE-AMT NOT < WS-PAYMENT-TIER-LOW(WS-TX)
                   MOVE WS-PAYMENT-TIER-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-MATURITY.
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
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3110-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3120-ACCRUE-LATE-FEE.
           IF CRCM-LATE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-LATE-FEE-YTD * CRCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-PURCHASE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-LAST-ACT.
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
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARD TYPE CD:
      * PGU -> TRN WHEN LAST ACT DATE IS PAST
      * TRN -> WHR WHEN LAST ACT DATE IS PAST
      * WHR -> ZWB WHEN LAST ACT DATE IS PAST
      * ZWB -> PGU WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3140-SET-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TRN' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TRN'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WHR' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WHR'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWB' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZWB'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PGU' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3150-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE CRCM-LIMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FINANCE SCHED TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3160-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FINANCE-SCHED-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCT-PAYMENT-AMT NOT < WS-FINANCE-SCHED-LOW(WS-TX)
                   MOVE WS-FINANCE-SCHED-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3170-DESCRIBE-CHANNEL.
           EVALUATE CRCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3180-LIMIT-PURCHASE.
           IF CRCM-PURCHASE-AMT < 10.00
               MOVE 10.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-PURCHASE-AMT > 5000.00
               MOVE 5000.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3190-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3200-DESCRIBE-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CASH ADV AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = CRCT-CASH-ADV-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CRCT-CASH-ADV-AMT - (WS-WORK-AMT * 4)
           COMPUTE CRCM-CASH-ADV-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3220-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3230-CHECK-NEXT-REVIEW.
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
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PURCHASE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3240-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CRCM-PURCHASE-YTD *
                       CRCM-RATE / 200
               WHEN CRCM-CARD-TYPE-CD = 'PGU'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN CRCM-CARD-TYPE-CD = 'TRN'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CRCM-PURCHASE-YTD *
                       CRCM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT TIER TIER FOR THE FINANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3250-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-TIER-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CRCM-FINANCE-AMT NOT < WS-PAYMENT-TIER-LOW(WS-TX)
                   MOVE WS-PAYMENT-TIER-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT FEE ON THE WORK AMT 2.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3260-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRCM-LIMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   COMPUTE CRCM-LIMIT-AMT ROUNDED = WS-WORK-AMT-2 *
                       CRCM-DISCOUNT / 200
               WHEN CRCM-SOURCE-CD = 'C'
                   MOVE 12.00 TO CRCM-LIMIT-AMT
               WHEN CRCM-SOURCE-CD = 'H'
                   MOVE 20.00 TO CRCM-LIMIT-AMT
               WHEN OTHER
                   COMPUTE CRCM-LIMIT-AMT ROUNDED = WS-WORK-AMT-2 *
                       CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCM-LIMIT-AMT > 0 AND CRCM-LIMIT-AMT < 10.00
               MOVE 10.00 TO CRCM-LIMIT-AMT
           END-IF
           IF CRCM-LIMIT-AMT > 75.00
               MOVE 75.00 TO CRCM-LIMIT-AMT
           END-IF
           ADD CRCM-LIMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN OPEN DATE IS PAST
      * DDY -> DPB WHEN OPEN DATE IS PAST
      * DPB -> UHF WHEN OPEN DATE IS PAST
      * UHF -> WLL WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3270-SET-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-PAYMENT.
           IF CRCT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCT-PAYMENT-AMT * CRCM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-MIN-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-TYPE.
           EVALUATE CRCM-TYPE-CD
               WHEN 'GP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CASH ADV AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3300-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = CRCM-CASH-ADV-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CRCM-CASH-ADV-AMT - (WS-WORK-AMT * 2)
           COMPUTE CRCM-LATE-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CTB -> LNX WHEN EFF DATE IS PAST
      * LNX -> MGS WHEN EFF DATE IS PAST
      * MGS -> YNJ WHEN EFF DATE IS PAST
      * YNJ -> CTB WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3310-SET-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LNX' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LNX'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MGS' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MGS'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YNJ' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YNJ'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CTB' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARD TYPE CD:
      * PGU -> TRN WHEN CLOSE DATE IS PAST
      * TRN -> WHR WHEN CLOSE DATE IS PAST
      * WHR -> ZWB WHEN CLOSE DATE IS PAST
      * ZWB -> PGU WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TRN' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TRN'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WHR' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WHR'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWB' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZWB'
                   IF CRCM-CLOSE-DATE > 0 AND CRCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PGU' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3330-DESCRIBE-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE YTD FEE ON THE MIN DUE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3340-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PURCHASE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CRCM-PURCHASE-YTD ROUNDED = CRCT-MIN-DUE-AMT
                       * CRCM-PENALTY-PCT / 200
               WHEN CRCM-TYPE-CD = 'GP'
                   MOVE 10.00 TO CRCM-PURCHASE-YTD
               WHEN CRCM-TYPE-CD = 'UV'
                   MOVE 5.00 TO CRCM-PURCHASE-YTD
               WHEN OTHER
                   COMPUTE CRCM-PURCHASE-YTD ROUNDED = CRCT-MIN-DUE-AMT
                       * CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-PURCHASE-YTD > 0 AND CRCM-PURCHASE-YTD < 1.00
               MOVE 1.00 TO CRCM-PURCHASE-YTD
           END-IF
           IF CRCM-PURCHASE-YTD > 25.00
               MOVE 25.00 TO CRCM-PURCHASE-YTD
           END-IF
           ADD CRCM-PURCHASE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT LIMIT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRCT-LIMIT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CRCT-LIMIT-AMT - (WS-WORK-AMT * 4)
           COMPUTE CRCT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PU -> CA WHEN LAST PAY DATE IS PAST
      * CA -> PY WHEN LAST PAY DATE IS PAST
      * PY -> FC WHEN LAST PAY DATE IS PAST
      * FC -> LF WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3360-SET-TRAN.
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
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3370-COUNT-NOTICE.
           IF CRCM-NOTICE-CNT < 9999999
               ADD 1 TO CRCM-NOTICE-CNT
           END-IF
           IF CRCM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CASH ADV AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3380-LIMIT-CASH-ADV.
           IF CRCM-CASH-ADV-AMT < 0.00
               MOVE 0.00 TO CRCM-CASH-ADV-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-CASH-ADV-AMT > 1000.00
               MOVE 1000.00 TO CRCM-CASH-ADV-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT TIERX TIER FOR THE FINANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3390-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-TIERX-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CRCM-FINANCE-AMT NOT < WS-PAYMENT-TIERX-LOW(WS-TX)
                   MOVE WS-PAYMENT-TIERX-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRCM-LATE-FEE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CRCM-LATE-FEE-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3410-ACCUM-CHANNEL.
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
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3420-DESCRIBE-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3430-CHECK-NOTICE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3450-DESCRIBE-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3460-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3470-COUNT-NOTICE.
           IF CRCM-NOTICE-CNT < 9999999
               ADD 1 TO CRCM-NOTICE-CNT
           END-IF
           IF CRCM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3480-AGE-CLOSE.
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
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3490-CHECK-EFF.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO CRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E242 CONTACT REQUIRED
      * E959 WORK AMT NEGATIVE
      * E905 WORK AMT OVER 25000
      * E387 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3500-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CONTACT = SPACES
                   MOVE 'E242' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E959' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E905' TO WS-REJECT-CODE
               WHEN CRCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3510-AGE-MATURITY.
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
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-TRAN.
           EVALUATE CRCT-TRAN-CD
               WHEN 'PU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MIN DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN DUE YTD.
      *----------------------------------------------------------------*
       3530-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-MIN-DUE-YTD ROUNDED =
                   CRCM-MIN-DUE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-MIN-DUE-YTD
           END-IF
           ADD CRCM-MIN-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       3540-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-LIMIT-AMT ROUNDED =
                   CRCM-LATE-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-LIMIT-AMT
           END-IF
           ADD CRCM-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT TIERX TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-TIERX-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-PAYMENT-TIERX-LOW(WS-TX)
                   MOVE WS-PAYMENT-TIERX-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CRCM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-FINANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E749 CONTACT REQUIRED
      * E302 LIMIT AMT NEGATIVE
      * E257 LIMIT AMT OVER 1000000
      * E053 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3570-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CONTACT = SPACES
                   MOVE 'E749' TO WS-REJECT-CODE
               WHEN CRCT-LIMIT-AMT < 0
                   MOVE 'E302' TO WS-REJECT-CODE
               WHEN CRCT-LIMIT-AMT > 1000000.00
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN CRCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E053' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ADV FEE ON THE LIMIT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3580-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRCT-CASH-ADV-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE CRCT-CASH-ADV-AMT ROUNDED = CRCM-LIMIT-YTD *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCT-TRAN-CD = 'PU'
                   MOVE 12.00 TO CRCT-CASH-ADV-AMT
               WHEN CRCT-TRAN-CD = 'CA'
                   MOVE 15.00 TO CRCT-CASH-ADV-AMT
               WHEN OTHER
                   COMPUTE CRCT-CASH-ADV-AMT ROUNDED = CRCM-LIMIT-YTD *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCT-CASH-ADV-AMT > 0 AND CRCT-CASH-ADV-AMT < 2.00
               MOVE 2.00 TO CRCT-CASH-ADV-AMT
           END-IF
           IF CRCT-CASH-ADV-AMT > 100.00
               MOVE 100.00 TO CRCT-CASH-ADV-AMT
           END-IF
           ADD CRCT-CASH-ADV-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-SOURCE.
           EVALUATE CRCM-SOURCE-CD
               WHEN 'C'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'M'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3610-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF CRCT-POST-DATE > 0 AND CRCT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3620-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = CRCM-PAYMENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CRCM-PAYMENT-AMT - (WS-WORK-AMT * 3)
           COMPUTE CRCM-FINANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-CHANNEL.
           EVALUATE CRCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3640-COUNT-NOTICE.
           IF CRCM-NOTICE-CNT < 9999999
               ADD 1 TO CRCM-NOTICE-CNT
           END-IF
           IF CRCM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN DUE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3650-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-STATUS-CD
                   ADD CRCM-MIN-DUE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-MIN-DUE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3660-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-CLOSE-DATE TO DTU-DATE-1
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
      * LIMIT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3670-FLAG-LIMIT.
           IF CRCT-LIMIT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCT-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E107 NAME REQUIRED
      * E887 CASH ADV AMT NEGATIVE
      * E995 CASH ADV AMT OVER 50000
      * E087 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-NAME = SPACES
                   MOVE 'E107' TO WS-REJECT-CODE
               WHEN CRCT-CASH-ADV-AMT < 0
                   MOVE 'E887' TO WS-REJECT-CODE
               WHEN CRCT-CASH-ADV-AMT > 50000.00
                   MOVE 'E995' TO WS-REJECT-CODE
               WHEN CRCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E087' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT TIER TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3690-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-TIER-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CRCM-PAYMENT-YTD NOT < WS-PAYMENT-TIER-LOW(WS-TX)
                   MOVE WS-PAYMENT-TIER-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ADV YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3700-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCT-CHANNEL-CD
                   ADD CRCM-CASH-ADV-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-CASH-ADV-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3710-SET-CHANNEL.
           EVALUATE CRCT-CHANNEL-CD
               WHEN 'BRN'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3720-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 10
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MIN DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH ADV YTD.
      *----------------------------------------------------------------*
       3730-PRORATE-CASH-ADV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-CASH-ADV-YTD ROUNDED =
                   CRCT-MIN-DUE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-CASH-ADV-YTD
           END-IF
           ADD CRCM-CASH-ADV-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3740-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FINANCE SCHED TIER FOR THE LATE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3750-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FINANCE-SCHED-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCM-LATE-FEE-AMT NOT < WS-FINANCE-SCHED-LOW(WS-TX)
                   MOVE WS-FINANCE-SCHED-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PURCHASE FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3760-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CRCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PURCHASE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CRCM-PURCHASE-AMT ROUNDED = WS-WORK-AMT-2 *
                       CRCM-RATE / 200
               WHEN CRCM-TYPE-CD = 'GP'
                   MOVE 7.00 TO CRCM-PURCHASE-AMT
               WHEN CRCM-TYPE-CD = 'UV'
                   MOVE 15.00 TO CRCM-PURCHASE-AMT
               WHEN OTHER
                   COMPUTE CRCM-PURCHASE-AMT ROUNDED = WS-WORK-AMT-2 *
                       CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-PURCHASE-AMT > 0 AND CRCM-PURCHASE-AMT < 1.00
               MOVE 1.00 TO CRCM-PURCHASE-AMT
           END-IF
           IF CRCM-PURCHASE-AMT > 75.00
               MOVE 75.00 TO CRCM-PURCHASE-AMT
           END-IF
           ADD CRCM-PURCHASE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * MIN DUE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3770-FLAG-MIN-DUE.
           IF CRCM-MIN-DUE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCM-MIN-DUE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FINANCE YTD FEE ON THE MIN DUE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3780-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CRCM-FINANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CRCM-FINANCE-YTD ROUNDED = CRCT-MIN-DUE-AMT *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCM-SOURCE-CD = 'C'
                   MOVE 5.00 TO CRCM-FINANCE-YTD
               WHEN CRCM-SOURCE-CD = 'H'
                   MOVE 10.00 TO CRCM-FINANCE-YTD
               WHEN OTHER
                   COMPUTE CRCM-FINANCE-YTD ROUNDED = CRCT-MIN-DUE-AMT *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-FINANCE-YTD > 0 AND CRCM-FINANCE-YTD < 5.00
               MOVE 5.00 TO CRCM-FINANCE-YTD
           END-IF
           IF CRCM-FINANCE-YTD > 75.00
               MOVE 75.00 TO CRCM-FINANCE-YTD
           END-IF
           ADD CRCM-FINANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3790-CHECK-POST.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3800-AGE-NOTICE.
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
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FINANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PURCHASE AMT.
      *----------------------------------------------------------------*
       3810-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-PURCHASE-AMT ROUNDED =
                   CRCM-FINANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-PURCHASE-AMT
           END-IF
           ADD CRCM-PURCHASE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3820-ACCRUE-LIMIT.
           IF CRCM-LIMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-LIMIT-AMT * CRCM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CRCM-MIN-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E475 CONTACT REQUIRED
      * E075 FINANCE AMT NEGATIVE
      * E230 FINANCE AMT OVER 5000
      * E962 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3830-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CONTACT = SPACES
                   MOVE 'E475' TO WS-REJECT-CODE
               WHEN CRCM-FINANCE-AMT < 0
                   MOVE 'E075' TO WS-REJECT-CODE
               WHEN CRCM-FINANCE-AMT > 5000.00
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN CRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E962' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT YTD BY CARD TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3840-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-CARD-TYPE-CD
                   ADD CRCM-LIMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-LIMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CTB -> LNX WHEN NOTICE DATE IS PAST
      * LNX -> MGS WHEN NOTICE DATE IS PAST
      * MGS -> YNJ WHEN NOTICE DATE IS PAST
      * YNJ -> CTB WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3850-SET-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LNX' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LNX'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MGS' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MGS'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YNJ' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YNJ'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CTB' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E779 CONTACT REQUIRED
      * E869 MIN DUE AMT NEGATIVE
      * E792 MIN DUE AMT OVER 250000
      * E453 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3870-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CONTACT = SPACES
                   MOVE 'E779' TO WS-REJECT-CODE
               WHEN CRCT-MIN-DUE-AMT < 0
                   MOVE 'E869' TO WS-REJECT-CODE
               WHEN CRCT-MIN-DUE-AMT > 250000.00
                   MOVE 'E792' TO WS-REJECT-CODE
               WHEN CRCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E453' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3880-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = CRCM-PURCHASE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CRCM-PURCHASE-AMT - (WS-WORK-AMT * 4)
           COMPUTE CRCM-MIN-DUE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3890-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 6
               MOVE 'Y' TO CRCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-PAYMENT.
           IF CRCM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PAYMENT-AMT * CRCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-PURCHASE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3910-FLAG-PAYMENT.
           IF CRCM-PAYMENT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3920-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 12
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'LNX'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MGS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YNJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3940-ACCRUE-LIMIT.
           IF CRCM-LIMIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-LIMIT-YTD * CRCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CRCM-LATE-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PU -> CA WHEN EFF DATE IS PAST
      * CA -> PY WHEN EFF DATE IS PAST
      * PY -> FC WHEN EFF DATE IS PAST
      * FC -> LF WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3950-SET-TRAN.
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
      * PAYMENT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3960-FLAG-PAYMENT.
           IF CRCM-PAYMENT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3970-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CRCM-LIMIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN DUE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3980-ACCRUE-MIN-DUE.
           IF CRCM-MIN-DUE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-MIN-DUE-YTD * CRCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO CRCT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BILL CYCLE CD:
      * B -> D WHEN EFF DATE IS PAST
      * D -> E WHEN EFF DATE IS PAST
      * E -> F WHEN EFF DATE IS PAST
      * F -> H WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3990-SET-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PURCHASE AMT.
      *----------------------------------------------------------------*
       4000-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-PURCHASE-AMT ROUNDED =
                   CRCM-PURCHASE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-PURCHASE-AMT
           END-IF
           ADD CRCM-PURCHASE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FINANCE SCHED TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4010-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FINANCE-SCHED-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCM-PAYMENT-YTD NOT < WS-FINANCE-SCHED-LOW(WS-TX)
                   MOVE WS-FINANCE-SCHED-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MIN DUE AMT.
      *----------------------------------------------------------------*
       4020-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-MIN-DUE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-MIN-DUE-AMT
           END-IF
           ADD CRCM-MIN-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4030-ACCRUE-LATE-FEE.
           IF CRCM-LATE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-LATE-FEE-YTD * CRCM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4040-FLAG-PURCHASE.
           IF CRCM-PURCHASE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCM-PURCHASE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4050-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4060-COUNT-ITEM.
           IF CRCM-ITEM-CNT < 9999999
               ADD 1 TO CRCM-ITEM-CNT
           END-IF
           IF CRCM-ITEM-CNT NOT < 10
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4070-AGE-EFF.
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
                   ADD CRCM-MIN-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-MIN-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-MIN-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-MIN-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-MIN-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4080-CHECK-LAST-ACT.
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
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH ADV YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4090-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = CRCM-CASH-ADV-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CRCM-CASH-ADV-YTD - (WS-WORK-AMT *
               12)
           COMPUTE CRCM-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ADV AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LIMIT YTD.
      *----------------------------------------------------------------*
       4100-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-LIMIT-YTD ROUNDED =
                   CRCM-CASH-ADV-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-LIMIT-YTD
           END-IF
           ADD CRCM-LIMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * C -> H WHEN EFF DATE IS PAST
      * H -> M WHEN EFF DATE IS PAST
      * M -> R WHEN EFF DATE IS PAST
      * R -> S WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4110-SET-SOURCE.
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
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CTB -> LNX WHEN NEXT REVIEW DATE IS PAST
      * LNX -> MGS WHEN NEXT REVIEW DATE IS PAST
      * MGS -> YNJ WHEN NEXT REVIEW DATE IS PAST
      * YNJ -> CTB WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4120-SET-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LNX' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LNX'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MGS' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MGS'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'YNJ' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YNJ'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'CTB' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4130-ACCRUE-LIMIT.
           IF CRCM-LIMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-LIMIT-AMT * CRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CRCM-CASH-ADV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4140-COUNT-CYCLE.
           IF CRCM-CYCLE-CNT < 9999999
               ADD 1 TO CRCM-CYCLE-CNT
           END-IF
           IF CRCM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO CRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4150-CHECK-POST.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FINANCE AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4160-LIMIT-FINANCE.
           IF CRCM-FINANCE-AMT < 50.00
               MOVE 50.00 TO CRCM-FINANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-FINANCE-AMT > 5000.00
               MOVE 5000.00 TO CRCM-FINANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4170-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF CRCM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ADV AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4180-ACCRUE-CASH-ADV.
           IF CRCT-CASH-ADV-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCT-CASH-ADV-AMT * CRCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO CRCM-LIMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FINANCE SCHED TIER FOR THE FINANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4190-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FINANCE-SCHED-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCM-FINANCE-YTD NOT < WS-FINANCE-SCHED-LOW(WS-TX)
                   MOVE WS-FINANCE-SCHED-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4200-LIMIT-LIMIT.
           IF CRCM-LIMIT-YTD < 25.00
               MOVE 25.00 TO CRCM-LIMIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-LIMIT-YTD > 5000.00
               MOVE 5000.00 TO CRCM-LIMIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CTB -> LNX WHEN NEXT REVIEW DATE IS PAST
      * LNX -> MGS WHEN NEXT REVIEW DATE IS PAST
      * MGS -> YNJ WHEN NEXT REVIEW DATE IS PAST
      * YNJ -> CTB WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4210-SET-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LNX' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LNX'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MGS' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MGS'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'YNJ' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YNJ'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'CTB' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E347 REF NO REQUIRED
      * E433 CASH ADV AMT NEGATIVE
      * E591 CASH ADV AMT OVER 250000
      * E065 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4220-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-REF-NO = SPACES
                   MOVE 'E347' TO WS-REJECT-CODE
               WHEN CRCT-CASH-ADV-AMT < 0
                   MOVE 'E433' TO WS-REJECT-CODE
               WHEN CRCT-CASH-ADV-AMT > 250000.00
                   MOVE 'E591' TO WS-REJECT-CODE
               WHEN CRCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E065' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FINANCE YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4230-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-TYPE-CD
                   ADD CRCM-FINANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-FINANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4240-ACCRUE-LIMIT.
           IF CRCM-LIMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-LIMIT-AMT * CRCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO CRCM-MIN-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FINANCE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4250-SPLIT-FINANCE.
           COMPUTE WS-WORK-AMT = CRCM-FINANCE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CRCM-FINANCE-AMT - (WS-WORK-AMT * 12)
           COMPUTE CRCM-LIMIT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4260-DESCRIBE-CHANNEL.
           EVALUATE CRCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4270-AGE-EFFECTIVE.
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
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LATE-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4280-LIMIT-PAYMENT.
           IF CRCT-PAYMENT-AMT < 50.00
               MOVE 50.00 TO CRCT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCT-PAYMENT-AMT > 500.00
               MOVE 500.00 TO CRCT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4290-LIMIT-LIMIT.
           IF CRCM-LIMIT-AMT < 100.00
               MOVE 100.00 TO CRCM-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-LIMIT-AMT > 10000.00
               MOVE 10000.00 TO CRCM-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4300-CHECK-NEXT-REVIEW.
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
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E076 REF NO REQUIRED
      * E507 FINANCE YTD NEGATIVE
      * E795 FINANCE YTD OVER 1000000
      * E583 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4310-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-REF-NO = SPACES
                   MOVE 'E076' TO WS-REJECT-CODE
               WHEN CRCM-FINANCE-YTD < 0
                   MOVE 'E507' TO WS-REJECT-CODE
               WHEN CRCM-FINANCE-YTD > 1000000.00
                   MOVE 'E795' TO WS-REJECT-CODE
               WHEN CRCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E583' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E725 CONTACT REQUIRED
      * E084 CASH ADV AMT NEGATIVE
      * E722 CASH ADV AMT OVER 1000000
      * E263 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4320-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CONTACT = SPACES
                   MOVE 'E725' TO WS-REJECT-CODE
               WHEN CRCT-CASH-ADV-AMT < 0
                   MOVE 'E084' TO WS-REJECT-CODE
               WHEN CRCT-CASH-ADV-AMT > 1000000.00
                   MOVE 'E722' TO WS-REJECT-CODE
               WHEN CRCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E263' TO WS-REJECT-CODE
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
       4330-DESCRIBE-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE MIN DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       4340-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-LIMIT-AMT ROUNDED =
                   CRCT-MIN-DUE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-LIMIT-AMT
           END-IF
           ADD CRCM-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ADV AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4350-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCT-CHANNEL-CD
                   ADD CRCM-CASH-ADV-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-CASH-ADV-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E033 MEMO REQUIRED
      * E256 PURCHASE YTD NEGATIVE
      * E648 PURCHASE YTD OVER 5000
      * E337 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4360-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCT-MEMO = SPACES
                   MOVE 'E033' TO WS-REJECT-CODE
               WHEN CRCM-PURCHASE-YTD < 0
                   MOVE 'E256' TO WS-REJECT-CODE
               WHEN CRCM-PURCHASE-YTD > 5000.00
                   MOVE 'E648' TO WS-REJECT-CODE
               WHEN CRCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E337' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4370-FLAG-LIMIT.
           IF CRCM-LIMIT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-LIMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4380-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CRCM-EXPIRY-DATE > 0 AND CRCM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CRCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PURCHASE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4390-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CRCM-PURCHASE-YTD *
                       CRCM-DISCOUNT / 200
               WHEN CRCT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN CRCT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CRCM-PURCHASE-YTD *
                       CRCM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 1.00
               MOVE 1.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4400-ACCRUE-PAYMENT.
           IF CRCM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PAYMENT-YTD * CRCM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-CASH-ADV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN DUE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4410-ACCRUE-MIN-DUE.
           IF CRCT-MIN-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCT-MIN-DUE-AMT * CRCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO CRCM-LATE-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE LIMIT YTD FEE ON THE LATE FEE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4420-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRCM-LIMIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CRCM-LIMIT-YTD ROUNDED = CRCM-LATE-FEE-YTD *
                       CRCM-RATE / 200
               WHEN CRCM-BILL-CYCLE-CD = 'B'
                   MOVE 10.00 TO CRCM-LIMIT-YTD
               WHEN CRCM-BILL-CYCLE-CD = 'D'
                   MOVE 12.00 TO CRCM-LIMIT-YTD
               WHEN OTHER
                   COMPUTE CRCM-LIMIT-YTD ROUNDED = CRCM-LATE-FEE-YTD *
                       CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-LIMIT-YTD > 0 AND CRCM-LIMIT-YTD < 2.00
               MOVE 2.00 TO CRCM-LIMIT-YTD
           END-IF
           IF CRCM-LIMIT-YTD > 100.00
               MOVE 100.00 TO CRCM-LIMIT-YTD
           END-IF
           ADD CRCM-LIMIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4430-AGE-LAST-PAY.
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
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT TIERX TIER FOR THE LATE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4440-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-TIERX-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CRCM-LATE-FEE-YTD NOT < WS-PAYMENT-TIERX-LOW(WS-TX)
                   MOVE WS-PAYMENT-TIERX-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4450-COUNT-NOTICE.
           IF CRCM-NOTICE-CNT < 9999999
               ADD 1 TO CRCM-NOTICE-CNT
           END-IF
           IF CRCM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4460-ACCRUE-PAYMENT.
           IF CRCM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PAYMENT-AMT * CRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CRCM-MIN-DUE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CTB -> LNX WHEN NEXT REVIEW DATE IS PAST
      * LNX -> MGS WHEN NEXT REVIEW DATE IS PAST
      * MGS -> YNJ WHEN NEXT REVIEW DATE IS PAST
      * YNJ -> CTB WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4470-SET-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'LNX' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LNX'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MGS' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MGS'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'YNJ' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YNJ'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'CTB' TO CRCM-BRANCH-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4480-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 10
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4490-AGE-NOTICE.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4500-AGE-LAST-ACT.
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
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4510-COUNT-ITEM.
           IF CRCM-ITEM-CNT < 9999999
               ADD 1 TO CRCM-ITEM-CNT
           END-IF
           IF CRCM-ITEM-CNT NOT < 10
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4520-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRCM-LATE-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CRCM-LATE-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE CRCM-MIN-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LIMIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN DUE YTD.
      *----------------------------------------------------------------*
       4530-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-MIN-DUE-YTD ROUNDED =
                   CRCM-LIMIT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-MIN-DUE-YTD
           END-IF
           ADD CRCM-MIN-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FINANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4540-PRORATE-FINANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CRCM-FINANCE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4550-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = CRCM-PAYMENT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CRCM-PAYMENT-YTD - (WS-WORK-AMT * 3)
           COMPUTE CRCM-MIN-DUE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4560-ACCRUE-PAYMENT.
           IF CRCM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PAYMENT-YTD * CRCM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRCT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

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
                   MOVE CRVAL-PAYMENT-AMT
                     TO CRCM-PAYMENT-AMT
                   MOVE CRVAL-STOP-FLAG
                     TO CRCM-STOP-FLAG
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
           DISPLAY 'CRCCALC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CRCCALC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
