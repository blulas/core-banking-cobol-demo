      *================================================================*
      * PROGRAM  : CRCSTMT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PROPRIETARY CREDIT CARD - STATEMENT PREPARATION.    *
      *            READS THE CRC TRANSACTIONS, UPDATES THE CRC MASTER  *
      *            AND PRODUCES THE CONTROL REPORT. IN-HOUSE CREDIT    *
      *            CARD BILLING, FINANCE CHARGE AND DELINQUENCY        *
      * INPUT    : CRCMST (KSDS), CRCTRN, BANKCTL                      *
      * OUTPUT   : CRCOUT, CRCSTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CRDTE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-01-01 CRP  ORIGINAL                                       *
      * 1996-03-11 RJT  PAYMENT STATUS CODES ADDED REQ7991             *
      * 1997-06-25 DPS  LATE-FEE EXCEPTIONS TO OPS QUEUE PRB3966       *
      * 1997-06-28 SKN  LIMIT TABLE EXPANDED CHG0418                   *
      * 1998-09-22 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-11-18 TWH  FINANCE RERUN PROTECTION PRB7202               *
      * 2000-07-05 JLM  PERFORMANCE - CASH-ADV READ AHEAD INC5225      *
      * 2003-11-15 JLM  PERFORMANCE - CASH-ADV READ AHEAD PRB2688      *
      * 2004-04-01 EWS  LIMIT CALCULATION CORRECTED PRB4469            *
      * 2004-02-05 TWH  CASH-ADV EXCEPTIONS TO OPS QUEUE CHG4189       *
      * 2006-05-14 SKN  PURCHASE CALCULATION CORRECTED PRB0912         *
      * 2006-10-25 CRP  AUDIT FINDING - MIN-DUE CONTROL TOTALS INC4770 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRCSTMT.
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
           SELECT REPORT-FILE ASSIGN TO "CRCSTRPT"
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
                                           VALUE 20.000000.
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
      * WS-LIMIT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-LIMIT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000196000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000753000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000388500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000164000'.
       01  WS-LIMIT-TIER-TBL REDEFINES WS-LIMIT-TIER-VALUES.
           05  WS-LIMIT-TIER-ROW OCCURS 4 TIMES.
               10  WS-LIMIT-TIER-LOW       PIC 9(09)V99.
               10  WS-LIMIT-TIER-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-ADV-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-ADV-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000880000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000164000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000782000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010150000000529000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010400000000549500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012900000000358000'.
       01  WS-CASH-ADV-SCHED-TBL REDEFINES WS-CASH-ADV-SCHED-VALUES.
           05  WS-CASH-ADV-SCHED-ROW OCCURS 6 TIMES.
               10  WS-CASH-ADV-SCHED-LOW   PIC 9(09)V99.
               10  WS-CASH-ADV-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PURCHASE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PURCHASE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000719500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000444500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000893000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020050000000858000'.
       01  WS-PURCHASE-SCHED-TBL REDEFINES WS-PURCHASE-SCHED-VALUES.
           05  WS-PURCHASE-SCHED-ROW OCCURS 4 TIMES.
               10  WS-PURCHASE-SCHED-LOW   PIC 9(09)V99.
               10  WS-PURCHASE-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYMENT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000533500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000537500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000104000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007600000000889500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000444500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012650000000531000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013150000000610500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014150000000176500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014200000000576500'.
       01  WS-PAYMENT-TIER-TBL REDEFINES WS-PAYMENT-TIER-VALUES.
           05  WS-PAYMENT-TIER-ROW OCCURS 9 TIMES.
               10  WS-PAYMENT-TIER-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-TIER-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CRCSTMT  PROPRIETARY CREDIT CARD'.
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
               PERFORM 3000-PRORATE-LIMIT
               PERFORM 3010-EDIT-DESC
               PERFORM 3020-SET-CARD-TYPE
               PERFORM 3030-SET-CARD-TYPE
               PERFORM 3040-SELECT-RATE
               PERFORM 3050-FLAG-PAYMENT
               PERFORM 3060-SPLIT-CASH-ADV
               PERFORM 3070-ACCUM-CARD-TYPE
               PERFORM 3080-ASSESS-NOTICE-SENT
               PERFORM 3090-EDIT-REF-NO
               PERFORM 3100-PRORATE-MIN-DUE
               PERFORM 3110-ACCUM-SOURCE
               PERFORM 3120-AGE-OPEN
               PERFORM 3130-FLAG-CASH-ADV
               PERFORM 3140-CHECK-LAST-ACT
               PERFORM 3150-SELECT-RATE
               PERFORM 3160-ACCRUE-PAYMENT
               PERFORM 3170-LIMIT-ITEM
               PERFORM 3180-PRORATE-MIN-DUE
               PERFORM 3190-SPLIT-LATE-FEE
               PERFORM 3200-SELECT-RATE
               PERFORM 3210-SPLIT-LATE-FEE
               PERFORM 3220-AGE-LAST-ACT
               PERFORM 3230-FLAG-LATE-FEE
               PERFORM 3240-ACCRUE-MIN-DUE
               PERFORM 3250-SELECT-DISCOUNT
               PERFORM 3260-AGE-POST
               PERFORM 3270-AGE-LAST-PAY
               PERFORM 3280-EDIT-ADDR
               PERFORM 3290-FLAG-ITEM
               PERFORM 3300-ACCRUE-FINANCE
               PERFORM 3310-FLAG-ITEM
               PERFORM 3320-SET-CHANNEL
               PERFORM 3330-ACCRUE-LATE-FEE
               PERFORM 3340-EDIT-MEMO
               PERFORM 3350-COUNT-CYCLE
               PERFORM 3360-COUNT-CYCLE
               PERFORM 3370-SELECT-RATE
               PERFORM 3380-SET-STATUS
               PERFORM 3390-ASSESS-STOP
               PERFORM 3400-COUNT-CYCLE
               PERFORM 3410-CHECK-LAST-ACT
               PERFORM 3420-DESCRIBE-BRANCH
               PERFORM 3430-FLAG-FINANCE
               PERFORM 3440-DESCRIBE-BILL-CYCLE
               PERFORM 3450-ACCUM-STATUS
               PERFORM 3460-ASSESS-OVERRIDE
               PERFORM 3470-SELECT-PENALTY-PCT
               PERFORM 3480-AGE-OPEN
               PERFORM 3490-SELECT-DISCOUNT
               PERFORM 3500-AGE-EFFECTIVE
               PERFORM 3510-COUNT-NOTICE
               PERFORM 3520-CHECK-MATURITY
               PERFORM 3530-PRORATE-LATE-FEE
               PERFORM 3540-SPLIT-LIMIT
               PERFORM 3550-DESCRIBE-TYPE
               PERFORM 3560-PRORATE-MIN-DUE
               PERFORM 3570-ACCUM-CARD-TYPE
               PERFORM 3580-SET-BILL-CYCLE
               PERFORM 3590-DESCRIBE-STATUS
               PERFORM 3600-ASSESS-NOTICE-SENT
               PERFORM 3610-SELECT-PENALTY-PCT
               PERFORM 3620-DESCRIBE-TRAN
               PERFORM 3630-AGE-OPEN
               PERFORM 3640-AGE-LAST-ACT
               PERFORM 3650-FLAG-ITEM
               PERFORM 3660-SET-BILL-CYCLE
               PERFORM 3670-COUNT-ITEM
               PERFORM 3680-FLAG-LIMIT
               PERFORM 3690-EDIT-NAME
               PERFORM 3700-SET-SOURCE
               PERFORM 3710-PRORATE-CASH-ADV
               PERFORM 3720-ACCRUE-PAYMENT
               PERFORM 3730-COUNT-RETRY
               PERFORM 3740-ACCRUE-LIMIT
               PERFORM 3750-ACCRUE-PAYMENT
               PERFORM 3760-ACCRUE-LATE-FEE
               PERFORM 3770-ACCUM-STATUS
               PERFORM 3780-ASSESS-PREFERRED
               PERFORM 3790-AGE-LAST-PAY
               PERFORM 3800-DESCRIBE-BILL-CYCLE
               PERFORM 3810-ACCUM-CHANNEL
               PERFORM 3820-COUNT-RETRY
               PERFORM 3830-SET-BILL-CYCLE
               PERFORM 3840-LIMIT-CASH-ADV
               PERFORM 3850-AGE-MATURITY
               PERFORM 3860-LIMIT-CASH-ADV
               PERFORM 3870-PRORATE-PAYMENT
               PERFORM 3880-SELECT-DISCOUNT
               PERFORM 3890-EDIT-CUST-ID
               PERFORM 3900-ACCRUE-PURCHASE
               PERFORM 3910-CHECK-NEXT-REVIEW
               PERFORM 3920-DESCRIBE-CARD-TYPE
               PERFORM 3930-AGE-NEXT-REVIEW
               PERFORM 3940-DESCRIBE-SOURCE
               PERFORM 3950-PRORATE-PURCHASE
               PERFORM 3960-EDIT-REF-NO
               PERFORM 3970-EDIT-ADDR
               PERFORM 3980-COUNT-SEQ
               PERFORM 3990-ACCUM-SOURCE
               PERFORM 4000-DESCRIBE-BRANCH
               PERFORM 4010-ACCUM-BILL-CYCLE
               PERFORM 4020-PRORATE-CASH-ADV
               PERFORM 4030-DESCRIBE-CARD-TYPE
               PERFORM 4040-DESCRIBE-CARD-TYPE
               PERFORM 4050-DESCRIBE-TRAN
               PERFORM 4060-PRORATE-FINANCE
               PERFORM 4070-ACCUM-STATUS
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
      * PRORATE FINANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LIMIT AMT.
      *----------------------------------------------------------------*
       3000-PRORATE-LIMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-LIMIT-AMT ROUNDED =
                   CRCM-FINANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-LIMIT-AMT
           END-IF
           ADD CRCM-LIMIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E239 DESC REQUIRED
      * E867 LIMIT AMT NEGATIVE
      * E661 LIMIT AMT OVER 25000
      * E641 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3010-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-DESC = SPACES
                   MOVE 'E239' TO WS-REJECT-CODE
               WHEN CRCT-LIMIT-AMT < 0
                   MOVE 'E867' TO WS-REJECT-CODE
               WHEN CRCT-LIMIT-AMT > 25000.00
                   MOVE 'E661' TO WS-REJECT-CODE
               WHEN CRCM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E641' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARD TYPE CD:
      * PGU -> TRN WHEN LAST PAY DATE IS PAST
      * TRN -> WHR WHEN LAST PAY DATE IS PAST
      * WHR -> ZWB WHEN LAST PAY DATE IS PAST
      * ZWB -> PGU WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3020-SET-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TRN' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TRN'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WHR' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WHR'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWB' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZWB'
                   IF CRCM-LAST-PAY-DATE > 0 AND CRCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PGU' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARD TYPE CD:
      * PGU -> TRN WHEN EFF DATE IS PAST
      * TRN -> WHR WHEN EFF DATE IS PAST
      * WHR -> ZWB WHEN EFF DATE IS PAST
      * ZWB -> PGU WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TRN' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TRN'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WHR' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WHR'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWB' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ZWB'
                   IF CRCT-EFF-DATE > 0 AND CRCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PGU' TO CRCM-CARD-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE SCHED TIER FOR THE CASH ADV YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-SCHED-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCM-CASH-ADV-YTD NOT < WS-PURCHASE-SCHED-LOW(WS-TX)
                   MOVE WS-PURCHASE-SCHED-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3050-FLAG-PAYMENT.
           IF CRCM-PAYMENT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH ADV YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3060-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = CRCM-CASH-ADV-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CRCM-CASH-ADV-YTD - (WS-WORK-AMT * 4)
           COMPUTE CRCM-FINANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY CARD TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-CARD-TYPE-CD
                   ADD CRCT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3080-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CRCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   COMPUTE CRCM-PAYMENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCM-TYPE-CD = 'GP'
                   MOVE 3.00 TO CRCM-PAYMENT-AMT
               WHEN CRCM-TYPE-CD = 'UV'
                   MOVE 15.00 TO CRCM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRCM-PAYMENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-PAYMENT-AMT > 0 AND CRCM-PAYMENT-AMT < 1.00
               MOVE 1.00 TO CRCM-PAYMENT-AMT
           END-IF
           IF CRCM-PAYMENT-AMT > 100.00
               MOVE 100.00 TO CRCM-PAYMENT-AMT
           END-IF
           ADD CRCM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E950 REF NO REQUIRED
      * E085 LIMIT AMT NEGATIVE
      * E986 LIMIT AMT OVER 250000
      * E542 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-REF-NO = SPACES
                   MOVE 'E950' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-AMT < 0
                   MOVE 'E085' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-AMT > 250000.00
                   MOVE 'E986' TO WS-REJECT-CODE
               WHEN CRCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E542' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ADV YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MIN DUE AMT.
      *----------------------------------------------------------------*
       3100-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-MIN-DUE-AMT ROUNDED =
                   CRCM-CASH-ADV-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-MIN-DUE-AMT
           END-IF
           ADD CRCM-MIN-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3110-ACCUM-SOURCE.
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
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3120-AGE-OPEN.
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
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH ADV AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3130-FLAG-CASH-ADV.
           IF CRCM-CASH-ADV-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-CASH-ADV-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-LAST-ACT.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV SCHED TIER FOR THE LATE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-SCHED-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF CRCM-LATE-FEE-YTD NOT < WS-CASH-ADV-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ADV-SCHED-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-PAYMENT.
           IF CRCM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PAYMENT-AMT * CRCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CRCT-CASH-ADV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3170-LIMIT-ITEM.
           IF WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MIN DUE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MIN DUE AMT.
      *----------------------------------------------------------------*
       3180-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-MIN-DUE-AMT ROUNDED =
                   CRCT-MIN-DUE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-MIN-DUE-AMT
           END-IF
           ADD CRCM-MIN-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3190-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRCM-LATE-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CRCM-LATE-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE CRCM-LIMIT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE SCHED TIER FOR THE MIN DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-SCHED-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCT-MIN-DUE-AMT NOT < WS-PURCHASE-SCHED-LOW(WS-TX)
                   MOVE WS-PURCHASE-SCHED-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRCM-LATE-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CRCM-LATE-FEE-YTD - (WS-WORK-AMT * 4)
           COMPUTE CRCT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-LAST-ACT.
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
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LATE FEE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3230-FLAG-LATE-FEE.
           IF CRCM-LATE-FEE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCM-LATE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN DUE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-MIN-DUE.
           IF CRCM-MIN-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-MIN-DUE-AMT * CRCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-CASH-ADV-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE SCHED TIER FOR THE FINANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3250-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-SCHED-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCM-FINANCE-YTD NOT < WS-PURCHASE-SCHED-LOW(WS-TX)
                   MOVE WS-PURCHASE-SCHED-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-POST.
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
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3270-AGE-LAST-PAY.
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
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E894 ADDR REQUIRED
      * E151 MIN DUE YTD NEGATIVE
      * E727 MIN DUE YTD OVER 250000
      * E321 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3280-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-ADDR = SPACES
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN CRCM-MIN-DUE-YTD < 0
                   MOVE 'E151' TO WS-REJECT-CODE
               WHEN CRCM-MIN-DUE-YTD > 250000.00
                   MOVE 'E727' TO WS-REJECT-CODE
               WHEN CRCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E321' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3290-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FINANCE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-FINANCE.
           IF CRCM-FINANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-FINANCE-AMT * CRCM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CRCM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3310-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST ACT DATE IS PAST
      * BAT -> MAL WHEN LAST ACT DATE IS PAST
      * MAL -> TEL WHEN LAST ACT DATE IS PAST
      * TEL -> ONL WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-CHANNEL.
           EVALUATE CRCT-CHANNEL-CD
               WHEN 'BRN'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CRCM-LAST-ACT-DATE > 0 AND CRCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CRCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-LATE-FEE.
           IF CRCM-LATE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-LATE-FEE-AMT * CRCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-MIN-DUE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E780 MEMO REQUIRED
      * E380 WORK AMT 2 NEGATIVE
      * E391 WORK AMT 2 OVER 10000
      * E665 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3340-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCT-MEMO = SPACES
                   MOVE 'E780' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E380' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E391' TO WS-REJECT-CODE
               WHEN CRCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E665' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3350-COUNT-CYCLE.
           IF CRCM-CYCLE-CNT < 9999999
               ADD 1 TO CRCM-CYCLE-CNT
           END-IF
           IF CRCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3360-COUNT-CYCLE.
           IF CRCM-CYCLE-CNT < 9999999
               ADD 1 TO CRCM-CYCLE-CNT
           END-IF
           IF CRCM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT TIER TIER FOR THE CASH ADV YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3370-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-TIER-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CRCM-CASH-ADV-YTD NOT < WS-PAYMENT-TIER-LOW(WS-TX)
                   MOVE WS-PAYMENT-TIER-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN MATURITY DATE IS PAST
      * DDY -> DPB WHEN MATURITY DATE IS PAST
      * DPB -> UHF WHEN MATURITY DATE IS PAST
      * UHF -> WLL WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ADV FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3390-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CRCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO CRCM-CASH-ADV-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CRCM-CASH-ADV-AMT ROUNDED = WS-WORK-AMT *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCM-BRANCH-CD = 'CTB'
                   MOVE 7.00 TO CRCM-CASH-ADV-AMT
               WHEN CRCM-BRANCH-CD = 'LNX'
                   MOVE 20.00 TO CRCM-CASH-ADV-AMT
               WHEN OTHER
                   COMPUTE CRCM-CASH-ADV-AMT ROUNDED = WS-WORK-AMT *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCM-CASH-ADV-AMT > 0 AND CRCM-CASH-ADV-AMT < 10.00
               MOVE 10.00 TO CRCM-CASH-ADV-AMT
           END-IF
           IF CRCM-CASH-ADV-AMT > 150.00
               MOVE 150.00 TO CRCM-CASH-ADV-AMT
           END-IF
           ADD CRCM-CASH-ADV-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-CYCLE.
           IF CRCM-CYCLE-CNT < 9999999
               ADD 1 TO CRCM-CYCLE-CNT
           END-IF
           IF CRCM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3410-CHECK-LAST-ACT.
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
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3420-DESCRIBE-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LNX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MGS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'YNJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FINANCE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3430-FLAG-FINANCE.
           IF CRCM-FINANCE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-FINANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BILL CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3440-DESCRIBE-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'E'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'P'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-STATUS-CD
                   ADD CRCM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE LIMIT YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3460-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   COMPUTE CRCM-PAYMENT-YTD ROUNDED = CRCM-LIMIT-YTD *
                       CRCM-RATE / 200
               WHEN CRCT-CHANNEL-CD = 'BRN'
                   MOVE 3.00 TO CRCM-PAYMENT-YTD
               WHEN CRCT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO CRCM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE CRCM-PAYMENT-YTD ROUNDED = CRCM-LIMIT-YTD *
                       CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-PAYMENT-YTD > 0 AND CRCM-PAYMENT-YTD < 1.00
               MOVE 1.00 TO CRCM-PAYMENT-YTD
           END-IF
           IF CRCM-PAYMENT-YTD > 100.00
               MOVE 100.00 TO CRCM-PAYMENT-YTD
           END-IF
           ADD CRCM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT TIER TIER FOR THE MIN DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3470-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-TIER-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CRCM-MIN-DUE-AMT NOT < WS-PAYMENT-TIER-LOW(WS-TX)
                   MOVE WS-PAYMENT-TIER-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3480-AGE-OPEN.
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
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV SCHED TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3490-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-SCHED-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF CRCM-PAYMENT-YTD NOT < WS-CASH-ADV-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ADV-SCHED-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3500-AGE-EFFECTIVE.
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
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LATE-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-NOTICE.
           IF CRCM-NOTICE-CNT < 9999999
               ADD 1 TO CRCM-NOTICE-CNT
           END-IF
           IF CRCM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3520-CHECK-MATURITY.
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
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE LATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO LATE FEE AMT.
      *----------------------------------------------------------------*
       3530-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-LATE-FEE-AMT ROUNDED =
                   CRCM-LATE-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-LATE-FEE-AMT
           END-IF
           ADD CRCM-LATE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT LIMIT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3540-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRCM-LIMIT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CRCM-LIMIT-YTD - (WS-WORK-AMT * 12)
           COMPUTE CRCT-CASH-ADV-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3550-DESCRIBE-TYPE.
           EVALUATE CRCM-TYPE-CD
               WHEN 'GP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN DUE AMT.
      *----------------------------------------------------------------*
       3560-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCT-MIN-DUE-AMT ROUNDED =
                   CRCM-PURCHASE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCT-MIN-DUE-AMT
           END-IF
           ADD CRCT-MIN-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE LIMIT AMT BY CARD TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-CARD-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-CARD-TYPE-CD
                   ADD CRCM-LIMIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-CARD-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-LIMIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BILL CYCLE CD:
      * B -> D WHEN NOTICE DATE IS PAST
      * D -> E WHEN NOTICE DATE IS PAST
      * E -> F WHEN NOTICE DATE IS PAST
      * F -> H WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3580-SET-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3590-DESCRIBE-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE FEE ON THE MIN DUE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3600-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CRCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO CRCT-MIN-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CRCT-MIN-DUE-AMT ROUNDED = CRCM-MIN-DUE-AMT *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCM-BILL-CYCLE-CD = 'B'
                   MOVE 3.00 TO CRCT-MIN-DUE-AMT
               WHEN CRCM-BILL-CYCLE-CD = 'D'
                   MOVE 20.00 TO CRCT-MIN-DUE-AMT
               WHEN OTHER
                   COMPUTE CRCT-MIN-DUE-AMT ROUNDED = CRCM-MIN-DUE-AMT *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF CRCT-MIN-DUE-AMT > 0 AND CRCT-MIN-DUE-AMT < 2.00
               MOVE 2.00 TO CRCT-MIN-DUE-AMT
           END-IF
           IF CRCT-MIN-DUE-AMT > 150.00
               MOVE 150.00 TO CRCT-MIN-DUE-AMT
           END-IF
           ADD CRCT-MIN-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE SCHED TIER FOR THE CASH ADV AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3610-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-SCHED-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCM-CASH-ADV-AMT NOT < WS-PURCHASE-SCHED-LOW(WS-TX)
                   MOVE WS-PURCHASE-SCHED-RATE(WS-TX) TO
                       CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3620-DESCRIBE-TRAN.
           EVALUATE CRCT-TRAN-CD
               WHEN 'PU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'LF'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3630-AGE-OPEN.
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
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3640-AGE-LAST-ACT.
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
                   ADD CRCM-PURCHASE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-PURCHASE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-PURCHASE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCM-PURCHASE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-PURCHASE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3650-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BILL CYCLE CD:
      * B -> D WHEN NOTICE DATE IS PAST
      * D -> E WHEN NOTICE DATE IS PAST
      * E -> F WHEN NOTICE DATE IS PAST
      * F -> H WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3660-SET-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF CRCM-NOTICE-DATE > 0 AND CRCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-ITEM.
           IF CRCM-ITEM-CNT < 9999999
               ADD 1 TO CRCM-ITEM-CNT
           END-IF
           IF CRCM-ITEM-CNT NOT < 6
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3680-FLAG-LIMIT.
           IF CRCT-LIMIT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CRCT-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E800 NAME REQUIRED
      * E512 PAYMENT YTD NEGATIVE
      * E314 PAYMENT YTD OVER 1000000
      * E537 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3690-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-NAME = SPACES
                   MOVE 'E800' TO WS-REJECT-CODE
               WHEN CRCM-PAYMENT-YTD < 0
                   MOVE 'E512' TO WS-REJECT-CODE
               WHEN CRCM-PAYMENT-YTD > 1000000.00
                   MOVE 'E314' TO WS-REJECT-CODE
               WHEN CRCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E537' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * C -> H WHEN LAST PAY DATE IS PAST
      * H -> M WHEN LAST PAY DATE IS PAST
      * M -> R WHEN LAST PAY DATE IS PAST
      * R -> S WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3700-SET-SOURCE.
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
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CASH ADV AMT.
      *----------------------------------------------------------------*
       3710-PRORATE-CASH-ADV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCT-CASH-ADV-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCT-CASH-ADV-AMT
           END-IF
           ADD CRCT-CASH-ADV-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3720-ACCRUE-PAYMENT.
           IF CRCM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PAYMENT-YTD * CRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CRCM-FINANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 6
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-LIMIT.
           IF CRCT-LIMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCT-LIMIT-AMT * CRCM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-FINANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3750-ACCRUE-PAYMENT.
           IF CRCM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PAYMENT-YTD * CRCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CRCM-FINANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3760-ACCRUE-LATE-FEE.
           IF CRCM-LATE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-LATE-FEE-YTD * CRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CRCM-FINANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3770-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-STATUS-CD
                   ADD CRCM-PURCHASE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-PURCHASE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE FEE ON THE PAYMENT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3780-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRCT-MIN-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CRCT-MIN-DUE-AMT ROUNDED = CRCM-PAYMENT-YTD *
                       CRCM-DISCOUNT / 200
               WHEN CRCT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO CRCT-MIN-DUE-AMT
               WHEN CRCT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO CRCT-MIN-DUE-AMT
               WHEN OTHER
                   COMPUTE CRCT-MIN-DUE-AMT ROUNDED = CRCM-PAYMENT-YTD *
                       CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCT-MIN-DUE-AMT > 0 AND CRCT-MIN-DUE-AMT < 5.00
               MOVE 5.00 TO CRCT-MIN-DUE-AMT
           END-IF
           IF CRCT-MIN-DUE-AMT > 25.00
               MOVE 25.00 TO CRCT-MIN-DUE-AMT
           END-IF
           ADD CRCT-MIN-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3790-AGE-LAST-PAY.
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
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-PURCHASE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE BILL CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3800-DESCRIBE-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'E'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'F'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'P'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3810-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCT-CHANNEL-CD
                   ADD CRCT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3820-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 5
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BILL CYCLE CD:
      * B -> D WHEN MATURITY DATE IS PAST
      * D -> E WHEN MATURITY DATE IS PAST
      * E -> F WHEN MATURITY DATE IS PAST
      * F -> H WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3830-SET-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF CRCM-MATURITY-DATE > 0 AND CRCM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH ADV YTD IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3840-LIMIT-CASH-ADV.
           IF CRCM-CASH-ADV-YTD < 100.00
               MOVE 100.00 TO CRCM-CASH-ADV-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-CASH-ADV-YTD > 50000.00
               MOVE 50000.00 TO CRCM-CASH-ADV-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-MATURITY.
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
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH ADV AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3860-LIMIT-CASH-ADV.
           IF CRCT-CASH-ADV-AMT < 50.00
               MOVE 50.00 TO CRCT-CASH-ADV-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCT-CASH-ADV-AMT > 50000.00
               MOVE 50000.00 TO CRCT-CASH-ADV-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE MIN DUE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       3870-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-PAYMENT-YTD ROUNDED =
                   CRCM-MIN-DUE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-PAYMENT-YTD
           END-IF
           ADD CRCM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE PURCHASE SCHED TIER FOR THE CASH ADV YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3880-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PURCHASE-SCHED-RATE(1) TO CRCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CRCM-CASH-ADV-YTD NOT < WS-PURCHASE-SCHED-LOW(WS-TX)
                   MOVE WS-PURCHASE-SCHED-RATE(WS-TX) TO CRCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E940 CUST ID REQUIRED
      * E674 CASH ADV AMT NEGATIVE
      * E811 CASH ADV AMT OVER 50000
      * E673 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CUST-ID = SPACES
                   MOVE 'E940' TO WS-REJECT-CODE
               WHEN CRCT-CASH-ADV-AMT < 0
                   MOVE 'E674' TO WS-REJECT-CODE
               WHEN CRCT-CASH-ADV-AMT > 50000.00
                   MOVE 'E811' TO WS-REJECT-CODE
               WHEN CRCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PURCHASE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-PURCHASE.
           IF CRCM-PURCHASE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PURCHASE-AMT * CRCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3910-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3920-DESCRIBE-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3930-AGE-NEXT-REVIEW.
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
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3940-DESCRIBE-SOURCE.
           EVALUATE CRCM-SOURCE-CD
               WHEN 'C'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3950-PRORATE-PURCHASE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CRCM-PURCHASE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E462 REF NO REQUIRED
      * E133 PURCHASE YTD NEGATIVE
      * E536 PURCHASE YTD OVER 5000
      * E577 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-REF-NO = SPACES
                   MOVE 'E462' TO WS-REJECT-CODE
               WHEN CRCM-PURCHASE-YTD < 0
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN CRCM-PURCHASE-YTD > 5000.00
                   MOVE 'E536' TO WS-REJECT-CODE
               WHEN CRCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E577' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E609 ADDR REQUIRED
      * E477 PURCHASE YTD NEGATIVE
      * E172 PURCHASE YTD OVER 99999
      * E767 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-ADDR = SPACES
                   MOVE 'E609' TO WS-REJECT-CODE
               WHEN CRCM-PURCHASE-YTD < 0
                   MOVE 'E477' TO WS-REJECT-CODE
               WHEN CRCM-PURCHASE-YTD > 99999.00
                   MOVE 'E172' TO WS-REJECT-CODE
               WHEN CRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E767' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-SEQ.
           IF CRCT-SEQ < 9999999
               ADD 1 TO CRCT-SEQ
           END-IF
           IF CRCT-SEQ NOT < 12
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-SOURCE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LNX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MGS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YNJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PURCHASE AMT BY BILL CYCLE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-BILL-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-BILL-CYCLE-CD
                   ADD CRCM-PURCHASE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-BILL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-PURCHASE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ADV YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4020-PRORATE-CASH-ADV.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-CASH-ADV-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4030-DESCRIBE-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4040-DESCRIBE-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4050-DESCRIBE-TRAN.
           EVALUATE CRCT-TRAN-CD
               WHEN 'PU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FINANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FINANCE AMT.
      *----------------------------------------------------------------*
       4060-PRORATE-FINANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-FINANCE-AMT ROUNDED =
                   CRCM-FINANCE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCM-FINANCE-AMT
           END-IF
           ADD CRCM-FINANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-STATUS-CD
                   ADD CRCM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

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
                   MOVE CRDTE-PURCHASE-YTD
                     TO CRCM-PURCHASE-YTD
                   MOVE CRDTE-FINANCE-AMT
                     TO CRCM-FINANCE-AMT
                   MOVE CRDTE-PURCHASE-AMT
                     TO CRCM-PURCHASE-AMT
                   MOVE CRDTE-LATE-FEE-YTD
                     TO CRCM-LATE-FEE-YTD
                   MOVE CRDTE-STOP-FLAG
                     TO CRCM-STOP-FLAG
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
           DISPLAY 'CRCSTMT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CRCSTMT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
