      *================================================================*
      * PROGRAM  : CRCPURG                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : PROPRIETARY CREDIT CARD - PURGE AND ARCHIVE. READS  *
      *            THE CRC TRANSACTIONS, UPDATES THE CRC MASTER AND    *
      *            PRODUCES THE CONTROL REPORT. IN-HOUSE CREDIT CARD   *
      *            BILLING, FINANCE CHARGE AND DELINQUENCY             *
      * INPUT    : CRCMST (KSDS), CRCTRN, BANKCTL                      *
      * OUTPUT   : CRCOUT, CRCPURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CRCAL, CRELG            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-05 BHS  ORIGINAL                                       *
      * 1995-05-16 CRP  PAYMENT EXCEPTIONS TO OPS QUEUE INC3852        *
      * 1996-12-19 DPS  PURCHASE NOTICE TEXT PER LEGAL PRB1981         *
      * 1999-07-27 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-04-15 SKN  PAYMENT CALCULATION CORRECTED CHG1994          *
      * 2000-12-26 EWS  MIN-DUE CUTOFF MOVED REQ3875                   *
      * 2002-11-17 DPS  PERFORMANCE - PAYMENT READ AHEAD PRB7514       *
      * 2006-09-16 CRP  LIMIT LIMIT REVISED CHG4074                    *
      * 2007-08-26 RJT  LATE-FEE EXCEPTIONS TO OPS QUEUE INC3188       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRCPURG.
       AUTHOR. R J TRAVERS.

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
           SELECT REPORT-FILE ASSIGN TO "CRCPURPT"
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
      * WS-FINANCE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FINANCE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000715500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000315000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000864500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011050000000810500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012050000000079500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012100000000196000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012350000000659500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012450000000738500'.
       01  WS-FINANCE-GRID-TBL REDEFINES WS-FINANCE-GRID-VALUES.
           05  WS-FINANCE-GRID-ROW OCCURS 8 TIMES.
               10  WS-FINANCE-GRID-LOW     PIC 9(09)V99.
               10  WS-FINANCE-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CASH-ADV-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CASH-ADV-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000738500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000439000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000333500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000315500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000405500'.
       01  WS-CASH-ADV-SCHED-TBL REDEFINES WS-CASH-ADV-SCHED-VALUES.
           05  WS-CASH-ADV-SCHED-ROW OCCURS 5 TIMES.
               10  WS-CASH-ADV-SCHED-LOW   PIC 9(09)V99.
               10  WS-CASH-ADV-SCHED-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CRCPURG  PROPRIETARY CREDIT CARD'.
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
       COPY CRELGP.

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
               PERFORM 3000-SET-TYPE
               PERFORM 3010-AGE-EFF
               PERFORM 3020-SET-STATUS
               PERFORM 3030-ASSESS-AUTO-RENEW
               PERFORM 3040-ACCRUE-MIN-DUE
               PERFORM 3050-CHECK-NOTICE
               PERFORM 3060-DESCRIBE-STATUS
               PERFORM 3070-COUNT-ITEM
               PERFORM 3080-FLAG-LIMIT
               PERFORM 3090-CHECK-LAST-ACT
               PERFORM 3100-COUNT-RETRY
               PERFORM 3110-SPLIT-LATE-FEE
               PERFORM 3120-SET-TRAN
               PERFORM 3130-SELECT-RATE
               PERFORM 3140-ASSESS-EXEMPT
               PERFORM 3150-AGE-EXPIRY
               PERFORM 3160-SET-SOURCE
               PERFORM 3170-SET-STATUS
               PERFORM 3180-PRORATE-MIN-DUE
               PERFORM 3190-EDIT-REJECT
               PERFORM 3200-LIMIT-PURCHASE
               PERFORM 3210-ACCRUE-PURCHASE
               PERFORM 3220-PRORATE-MIN-DUE
               PERFORM 3230-ACCRUE-CASH-ADV
               PERFORM 3240-AGE-OPEN
               PERFORM 3250-LIMIT-LIMIT
               PERFORM 3260-ACCRUE-MIN-DUE
               PERFORM 3270-SET-TYPE
               PERFORM 3280-ACCUM-SOURCE
               PERFORM 3290-ASSESS-REVERSAL
               PERFORM 3300-ACCUM-BRANCH
               PERFORM 3310-ASSESS-OVERRIDE
               PERFORM 3320-ASSESS-PREFERRED
               PERFORM 3330-ASSESS-OVERRIDE
               PERFORM 3340-SET-SOURCE
               PERFORM 3350-COUNT-RETRY
               PERFORM 3360-LIMIT-LATE-FEE
               PERFORM 3370-SPLIT-PURCHASE
               PERFORM 3380-ACCRUE-PAYMENT
               PERFORM 3390-AGE-EFFECTIVE
               PERFORM 3400-AGE-MATURITY
               PERFORM 3410-FLAG-LIMIT
               PERFORM 3420-FLAG-PURCHASE
               PERFORM 3430-AGE-LAST-ACT
               PERFORM 3440-ASSESS-OVERRIDE
               PERFORM 3450-LIMIT-CASH-ADV
               PERFORM 3460-SELECT-RATE
               PERFORM 3470-ASSESS-PREFERRED
               PERFORM 3480-ASSESS-OVERRIDE
               PERFORM 3490-DESCRIBE-BRANCH
               PERFORM 3500-LIMIT-PAYMENT
               PERFORM 3510-COUNT-NOTICE
               PERFORM 3520-EDIT-ADDR
               PERFORM 3530-LIMIT-PAYMENT
               PERFORM 3540-AGE-LAST-ACT
               PERFORM 3550-PRORATE-LATE-FEE
               PERFORM 3560-ACCRUE-PAYMENT
               PERFORM 3570-DESCRIBE-CARD-TYPE
               PERFORM 3580-COUNT-RETRY
               PERFORM 3590-AGE-MATURITY
               PERFORM 3600-SELECT-RATE
               PERFORM 3610-CHECK-CLOSE
               PERFORM 3620-FLAG-LATE-FEE
               PERFORM 3630-PRORATE-FINANCE
               PERFORM 3640-ACCRUE-LIMIT
               PERFORM 3650-EDIT-ADDR
               PERFORM 3660-CHECK-NEXT-REVIEW
               PERFORM 3670-COUNT-ITEM
               PERFORM 3680-ACCRUE-LATE-FEE
               PERFORM 3690-SET-TRAN
               PERFORM 3700-EDIT-CUST-ID
               PERFORM 3710-CHECK-NEXT-REVIEW
               PERFORM 3720-EDIT-OPERATOR
               PERFORM 3730-EDIT-CUST-ID
               PERFORM 3740-ACCUM-STATUS
               PERFORM 3750-COUNT-SEQ
               PERFORM 3760-CHECK-LAST-PAY
               PERFORM 3770-ACCRUE-CASH-ADV
               PERFORM 3780-ACCRUE-LATE-FEE
               PERFORM 3790-SELECT-PENALTY-PCT
               PERFORM 3800-ACCUM-SOURCE
               PERFORM 3810-ACCRUE-PURCHASE
               PERFORM 3820-ACCUM-BILL-CYCLE
               PERFORM 3830-LIMIT-PURCHASE
               PERFORM 3840-LIMIT-MIN-DUE
               PERFORM 3850-COUNT-CYCLE
               PERFORM 3860-COUNT-CYCLE
               PERFORM 3870-LIMIT-MIN-DUE
               PERFORM 3880-SPLIT-CASH-ADV
               PERFORM 3890-AGE-EFFECTIVE
               PERFORM 3900-EDIT-CUST-ID
               PERFORM 3910-SPLIT-LIMIT
               PERFORM 3920-AGE-NEXT-REVIEW
               PERFORM 3930-ACCUM-CHANNEL
               PERFORM 3940-CHECK-EFFECTIVE
               PERFORM 3950-DESCRIBE-STATUS
               PERFORM 3960-COUNT-SEQ
               PERFORM 3970-ASSESS-PREFERRED
               PERFORM 3980-SPLIT-CASH-ADV
               PERFORM 3990-FLAG-PURCHASE
               PERFORM 4000-LIMIT-PAYMENT
               PERFORM 4010-SET-BILL-CYCLE
               PERFORM 4020-CHECK-EFF
               PERFORM 4030-SET-BRANCH
               PERFORM 4040-LIMIT-PURCHASE
               PERFORM 4050-AGE-CLOSE
               PERFORM 4060-EDIT-ADDR
               PERFORM 4070-ACCUM-CARD-TYPE
               PERFORM 4080-COUNT-RETRY
               PERFORM 4090-FLAG-MIN-DUE
               PERFORM 4100-FLAG-PURCHASE
               PERFORM 4110-COUNT-ITEM
               PERFORM 4120-LIMIT-LIMIT
               PERFORM 4130-SET-STATUS
               PERFORM 4140-ACCUM-BRANCH
               PERFORM 4150-CHECK-EFFECTIVE
               PERFORM 4160-FLAG-LIMIT
               PERFORM 7040-CALL-CRCAL
               PERFORM 7030-CALL-CRELG
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
      * STATUS TRANSITIONS FOR TYPE CD:
      * GP -> UV WHEN EFFECTIVE DATE IS PAST
      * UV -> YK WHEN EFFECTIVE DATE IS PAST
      * YK -> GP WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3000-SET-TYPE.
           EVALUATE CRCM-TYPE-CD
               WHEN 'GP'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UV' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YK' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YK'
                   IF CRCM-EFFECTIVE-DATE > 0 AND CRCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GP' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3010-AGE-EFF.
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
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-CASH-ADV-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN NEXT REVIEW DATE IS PAST
      * DDY -> DPB WHEN NEXT REVIEW DATE IS PAST
      * DPB -> UHF WHEN NEXT REVIEW DATE IS PAST
      * UHF -> WLL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3020-SET-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DDY' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DDY'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DPB' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DPB'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UHF' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UHF'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'WLL' TO CRCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE LATE FEE FEE ON THE LATE FEE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3030-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CRCM-LATE-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   COMPUTE CRCM-LATE-FEE-AMT ROUNDED = CRCM-LATE-FEE-YTD
                       * CRCM-DISCOUNT / 200
               WHEN CRCM-BRANCH-CD = 'CTB'
                   MOVE 5.00 TO CRCM-LATE-FEE-AMT
               WHEN CRCM-BRANCH-CD = 'LNX'
                   MOVE 15.00 TO CRCM-LATE-FEE-AMT
               WHEN OTHER
                   COMPUTE CRCM-LATE-FEE-AMT ROUNDED = CRCM-LATE-FEE-YTD
                       * CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCM-LATE-FEE-AMT > 0 AND CRCM-LATE-FEE-AMT < 5.00
               MOVE 5.00 TO CRCM-LATE-FEE-AMT
           END-IF
           IF CRCM-LATE-FEE-AMT > 25.00
               MOVE 25.00 TO CRCM-LATE-FEE-AMT
           END-IF
           ADD CRCM-LATE-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN DUE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3040-ACCRUE-MIN-DUE.
           IF CRCT-MIN-DUE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCT-MIN-DUE-AMT * CRCM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CRCM-CASH-ADV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3050-CHECK-NOTICE.
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
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3060-DESCRIBE-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3070-COUNT-ITEM.
           IF CRCM-ITEM-CNT < 9999999
               ADD 1 TO CRCM-ITEM-CNT
           END-IF
           IF CRCM-ITEM-CNT NOT < 10
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3080-FLAG-LIMIT.
           IF CRCM-LIMIT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCM-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3090-CHECK-LAST-ACT.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO CRCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 3
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LATE FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3110-SPLIT-LATE-FEE.
           COMPUTE WS-WORK-AMT = CRCM-LATE-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CRCM-LATE-FEE-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PU -> CA WHEN LAST PAY DATE IS PAST
      * CA -> PY WHEN LAST PAY DATE IS PAST
      * PY -> FC WHEN LAST PAY DATE IS PAST
      * FC -> LF WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-TRAN.
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
      * SELECT THE FINANCE GRID TIER FOR THE MIN DUE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3130-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FINANCE-GRID-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CRCM-MIN-DUE-YTD NOT < WS-FINANCE-GRID-LOW(WS-TX)
                   MOVE WS-FINANCE-GRID-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FINANCE FEE ON THE LATE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3140-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CRCM-FINANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CRCM-FINANCE-AMT ROUNDED = CRCM-LATE-FEE-AMT
                       * CRCM-RATE / 200
               WHEN CRCM-BRANCH-CD = 'CTB'
                   MOVE 10.00 TO CRCM-FINANCE-AMT
               WHEN CRCM-BRANCH-CD = 'LNX'
                   MOVE 10.00 TO CRCM-FINANCE-AMT
               WHEN OTHER
                   COMPUTE CRCM-FINANCE-AMT ROUNDED = CRCM-LATE-FEE-AMT
                       * CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-FINANCE-AMT > 0 AND CRCM-FINANCE-AMT < 1.00
               MOVE 1.00 TO CRCM-FINANCE-AMT
           END-IF
           IF CRCM-FINANCE-AMT > 250.00
               MOVE 250.00 TO CRCM-FINANCE-AMT
           END-IF
           ADD CRCM-FINANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3150-AGE-EXPIRY.
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
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-CASH-ADV-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * C -> H WHEN EFFECTIVE DATE IS PAST
      * H -> M WHEN EFFECTIVE DATE IS PAST
      * M -> R WHEN EFFECTIVE DATE IS PAST
      * R -> S WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3160-SET-SOURCE.
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
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN EFFECTIVE DATE IS PAST
      * DDY -> DPB WHEN EFFECTIVE DATE IS PAST
      * DPB -> UHF WHEN EFFECTIVE DATE IS PAST
      * UHF -> WLL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3170-SET-STATUS.
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
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO MIN DUE AMT.
      *----------------------------------------------------------------*
       3180-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCT-MIN-DUE-AMT ROUNDED =
                   CRCT-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CRCT-MIN-DUE-AMT
           END-IF
           ADD CRCT-MIN-DUE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E311 REJECT CD REQUIRED
      * E303 LATE FEE AMT NEGATIVE
      * E592 LATE FEE AMT OVER 99999
      * E645 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3190-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCT-REJECT-CD = SPACES
                   MOVE 'E311' TO WS-REJECT-CODE
               WHEN CRCM-LATE-FEE-AMT < 0
                   MOVE 'E303' TO WS-REJECT-CODE
               WHEN CRCM-LATE-FEE-AMT > 99999.00
                   MOVE 'E592' TO WS-REJECT-CODE
               WHEN CRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E645' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3200-LIMIT-PURCHASE.
           IF CRCM-PURCHASE-AMT < 100.00
               MOVE 100.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-PURCHASE-AMT > 2500.00
               MOVE 2500.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PURCHASE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-PURCHASE.
           IF CRCM-PURCHASE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PURCHASE-AMT * CRCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO CRCM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PURCHASE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO MIN DUE YTD.
      *----------------------------------------------------------------*
       3220-PRORATE-MIN-DUE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-MIN-DUE-YTD ROUNDED =
                   CRCM-PURCHASE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-MIN-DUE-YTD
           END-IF
           ADD CRCM-MIN-DUE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CASH ADV AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-CASH-ADV.
           IF CRCT-CASH-ADV-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCT-CASH-ADV-AMT * CRCM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CRCM-LIMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3240-AGE-OPEN.
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
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-CASH-ADV-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-LIMIT.
           IF CRCT-LIMIT-AMT < 50.00
               MOVE 50.00 TO CRCT-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCT-LIMIT-AMT > 50000.00
               MOVE 50000.00 TO CRCT-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON MIN DUE YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3260-ACCRUE-MIN-DUE.
           IF CRCM-MIN-DUE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-MIN-DUE-YTD * CRCM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CRCM-CASH-ADV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * GP -> UV WHEN NEXT REVIEW DATE IS PAST
      * UV -> YK WHEN NEXT REVIEW DATE IS PAST
      * YK -> GP WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3270-SET-TYPE.
           EVALUATE CRCM-TYPE-CD
               WHEN 'GP'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UV' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UV'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'YK' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YK'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GP' TO CRCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ADV AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3280-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-SOURCE-CD
                   ADD CRCM-CASH-ADV-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-CASH-ADV-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3290-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CRCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CRCT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CRCT-PAYMENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       CRCM-DISCOUNT / 200
               WHEN CRCM-TYPE-CD = 'GP'
                   MOVE 20.00 TO CRCT-PAYMENT-AMT
               WHEN CRCM-TYPE-CD = 'UV'
                   MOVE 5.00 TO CRCT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRCT-PAYMENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCT-PAYMENT-AMT > 0 AND CRCT-PAYMENT-AMT < 1.00
               MOVE 1.00 TO CRCT-PAYMENT-AMT
           END-IF
           IF CRCT-PAYMENT-AMT > 50.00
               MOVE 50.00 TO CRCT-PAYMENT-AMT
           END-IF
           ADD CRCT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FINANCE AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3300-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-BRANCH-CD
                   ADD CRCM-FINANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-FINANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE FEE ON THE PAYMENT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3310-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CRCM-MIN-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CRCM-MIN-DUE-AMT ROUNDED = CRCM-PAYMENT-AMT *
                       CRCM-RATE / 200
               WHEN CRCM-BRANCH-CD = 'CTB'
                   MOVE 3.00 TO CRCM-MIN-DUE-AMT
               WHEN CRCM-BRANCH-CD = 'LNX'
                   MOVE 7.00 TO CRCM-MIN-DUE-AMT
               WHEN OTHER
                   COMPUTE CRCM-MIN-DUE-AMT ROUNDED = CRCM-PAYMENT-AMT *
                       CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-MIN-DUE-AMT > 0 AND CRCM-MIN-DUE-AMT < 1.00
               MOVE 1.00 TO CRCM-MIN-DUE-AMT
           END-IF
           IF CRCM-MIN-DUE-AMT > 25.00
               MOVE 25.00 TO CRCM-MIN-DUE-AMT
           END-IF
           ADD CRCM-MIN-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE YTD FEE ON THE PAYMENT AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3320-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRCM-MIN-DUE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE CRCM-MIN-DUE-YTD ROUNDED = CRCT-PAYMENT-AMT *
                       CRCM-RATE / 200
               WHEN CRCT-TRAN-CD = 'PU'
                   MOVE 12.00 TO CRCM-MIN-DUE-YTD
               WHEN CRCT-TRAN-CD = 'CA'
                   MOVE 5.00 TO CRCM-MIN-DUE-YTD
               WHEN OTHER
                   COMPUTE CRCM-MIN-DUE-YTD ROUNDED = CRCT-PAYMENT-AMT *
                       CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-MIN-DUE-YTD > 0 AND CRCM-MIN-DUE-YTD < 1.00
               MOVE 1.00 TO CRCM-MIN-DUE-YTD
           END-IF
           IF CRCM-MIN-DUE-YTD > 35.00
               MOVE 35.00 TO CRCM-MIN-DUE-YTD
           END-IF
           ADD CRCM-MIN-DUE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE MIN DUE FEE ON THE WORK AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3330-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CRCT-MIN-DUE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CRCT-MIN-DUE-AMT ROUNDED = WS-WORK-AMT *
                       CRCM-DISCOUNT / 200
               WHEN CRCM-SOURCE-CD = 'C'
                   MOVE 12.00 TO CRCT-MIN-DUE-AMT
               WHEN CRCM-SOURCE-CD = 'H'
                   MOVE 7.00 TO CRCT-MIN-DUE-AMT
               WHEN OTHER
                   COMPUTE CRCT-MIN-DUE-AMT ROUNDED = WS-WORK-AMT *
                       CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCT-MIN-DUE-AMT > 0 AND CRCT-MIN-DUE-AMT < 2.00
               MOVE 2.00 TO CRCT-MIN-DUE-AMT
           END-IF
           IF CRCT-MIN-DUE-AMT > 25.00
               MOVE 25.00 TO CRCT-MIN-DUE-AMT
           END-IF
           ADD CRCT-MIN-DUE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * C -> H WHEN NEXT REVIEW DATE IS PAST
      * H -> M WHEN NEXT REVIEW DATE IS PAST
      * M -> R WHEN NEXT REVIEW DATE IS PAST
      * R -> S WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3340-SET-SOURCE.
           EVALUATE CRCM-SOURCE-CD
               WHEN 'C'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'M' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF CRCM-NEXT-REVIEW-DATE > 0 AND
                       CRCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'S' TO CRCM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3350-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 10
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3360-LIMIT-LATE-FEE.
           IF CRCM-LATE-FEE-AMT < 0.00
               MOVE 0.00 TO CRCM-LATE-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-LATE-FEE-AMT > 5000.00
               MOVE 5000.00 TO CRCM-LATE-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PURCHASE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3370-SPLIT-PURCHASE.
           COMPUTE WS-WORK-AMT = CRCM-PURCHASE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CRCM-PURCHASE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE CRCM-LATE-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3380-ACCRUE-PAYMENT.
           IF CRCM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PAYMENT-AMT * CRCM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3390-AGE-EFFECTIVE.
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
                   ADD CRCM-MIN-DUE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-MIN-DUE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-MIN-DUE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-MIN-DUE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-MIN-DUE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-MATURITY.
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
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-FINANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * LIMIT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3410-FLAG-LIMIT.
           IF CRCM-LIMIT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CRCM-LIMIT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3420-FLAG-PURCHASE.
           IF CRCM-PURCHASE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-PURCHASE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-LAST-ACT.
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
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE LATE FEE YTD FEE ON THE MIN DUE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3440-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CRCM-LATE-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   COMPUTE CRCM-LATE-FEE-YTD ROUNDED = CRCT-MIN-DUE-AMT
                       * CRCM-DISCOUNT / 200
               WHEN CRCT-TRAN-CD = 'PU'
                   MOVE 10.00 TO CRCM-LATE-FEE-YTD
               WHEN CRCT-TRAN-CD = 'CA'
                   MOVE 5.00 TO CRCM-LATE-FEE-YTD
               WHEN OTHER
                   COMPUTE CRCM-LATE-FEE-YTD ROUNDED = CRCT-MIN-DUE-AMT
                       * CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCM-LATE-FEE-YTD > 0 AND CRCM-LATE-FEE-YTD < 10.00
               MOVE 10.00 TO CRCM-LATE-FEE-YTD
           END-IF
           IF CRCM-LATE-FEE-YTD > 150.00
               MOVE 150.00 TO CRCM-LATE-FEE-YTD
           END-IF
           ADD CRCM-LATE-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASH ADV YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3450-LIMIT-CASH-ADV.
           IF CRCM-CASH-ADV-YTD < 100.00
               MOVE 100.00 TO CRCM-CASH-ADV-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-CASH-ADV-YTD > 500.00
               MOVE 500.00 TO CRCM-CASH-ADV-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CASH ADV SCHED TIER FOR THE MIN DUE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3460-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CASH-ADV-SCHED-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF CRCT-MIN-DUE-AMT NOT < WS-CASH-ADV-SCHED-LOW(WS-TX)
                   MOVE WS-CASH-ADV-SCHED-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE FINANCE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3470-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO CRCT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-EXEMPT-FLAG = 'Y'
                   COMPUTE CRCT-PAYMENT-AMT ROUNDED = CRCM-FINANCE-AMT *
                       CRCM-DISCOUNT / 200
               WHEN CRCM-BRANCH-CD = 'CTB'
                   MOVE 5.00 TO CRCT-PAYMENT-AMT
               WHEN CRCM-BRANCH-CD = 'LNX'
                   MOVE 10.00 TO CRCT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE CRCT-PAYMENT-AMT ROUNDED = CRCM-FINANCE-AMT *
                       CRCM-DISCOUNT / 100
           END-EVALUATE
           IF CRCT-PAYMENT-AMT > 0 AND CRCT-PAYMENT-AMT < 10.00
               MOVE 10.00 TO CRCT-PAYMENT-AMT
           END-IF
           IF CRCT-PAYMENT-AMT > 75.00
               MOVE 75.00 TO CRCT-PAYMENT-AMT
           END-IF
           ADD CRCT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE CASH ADV AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3480-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CRCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CRCM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE CRCM-PAYMENT-YTD ROUNDED = CRCM-CASH-ADV-AMT
                       * CRCM-RATE / 200
               WHEN CRCT-TRAN-CD = 'PU'
                   MOVE 3.00 TO CRCM-PAYMENT-YTD
               WHEN CRCT-TRAN-CD = 'CA'
                   MOVE 5.00 TO CRCM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE CRCM-PAYMENT-YTD ROUNDED = CRCM-CASH-ADV-AMT
                       * CRCM-RATE / 100
           END-EVALUATE
           IF CRCM-PAYMENT-YTD > 0 AND CRCM-PAYMENT-YTD < 5.00
               MOVE 5.00 TO CRCM-PAYMENT-YTD
           END-IF
           IF CRCM-PAYMENT-YTD > 100.00
               MOVE 100.00 TO CRCM-PAYMENT-YTD
           END-IF
           ADD CRCM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE BRANCH CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-BRANCH.
           EVALUATE CRCM-BRANCH-CD
               WHEN 'CTB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'LNX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MGS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YNJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3500-LIMIT-PAYMENT.
           IF CRCT-PAYMENT-AMT < 10.00
               MOVE 10.00 TO CRCT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCT-PAYMENT-AMT > 2500.00
               MOVE 2500.00 TO CRCT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3510-COUNT-NOTICE.
           IF CRCM-NOTICE-CNT < 9999999
               ADD 1 TO CRCM-NOTICE-CNT
           END-IF
           IF CRCM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E690 ADDR REQUIRED
      * E381 WORK AMT NEGATIVE
      * E866 WORK AMT OVER 250000
      * E612 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3520-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-ADDR = SPACES
                   MOVE 'E690' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E381' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E866' TO WS-REJECT-CODE
               WHEN CRCM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E612' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3530-LIMIT-PAYMENT.
           IF CRCM-PAYMENT-AMT < 50.00
               MOVE 50.00 TO CRCM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-PAYMENT-AMT > 5000.00
               MOVE 5000.00 TO CRCM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3540-AGE-LAST-ACT.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO LATE FEE AMT.
      *----------------------------------------------------------------*
       3550-PRORATE-LATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CRCM-LATE-FEE-AMT ROUNDED =
                   CRCT-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CRCM-LATE-FEE-AMT
           END-IF
           ADD CRCM-LATE-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-PAYMENT.
           IF CRCM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PAYMENT-AMT * CRCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CRCM-FINANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARD TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3570-DESCRIBE-CARD-TYPE.
           EVALUATE CRCM-CARD-TYPE-CD
               WHEN 'PGU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TRN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WHR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ZWB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3580-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 6
               MOVE 'Y' TO CRCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-MATURITY.
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
               WHEN WS-DAYS NOT > 120
                   ADD CRCT-LIMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CRCT-LIMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCT-LIMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FINANCE GRID TIER FOR THE LIMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3600-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FINANCE-GRID-RATE(1) TO CRCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CRCM-LIMIT-AMT NOT < WS-FINANCE-GRID-LOW(WS-TX)
                   MOVE WS-FINANCE-GRID-RATE(WS-TX) TO CRCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3610-CHECK-CLOSE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LATE FEE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3620-FLAG-LATE-FEE.
           IF CRCM-LATE-FEE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-LATE-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FINANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3630-PRORATE-FINANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CRCM-FINANCE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LIMIT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3640-ACCRUE-LIMIT.
           IF CRCM-LIMIT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-LIMIT-YTD * CRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CRCT-LIMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E802 ADDR REQUIRED
      * E464 CASH ADV YTD NEGATIVE
      * E267 CASH ADV YTD OVER 25000
      * E692 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-ADDR = SPACES
                   MOVE 'E802' TO WS-REJECT-CODE
               WHEN CRCM-CASH-ADV-YTD < 0
                   MOVE 'E464' TO WS-REJECT-CODE
               WHEN CRCM-CASH-ADV-YTD > 25000.00
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN CRCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E692' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3660-CHECK-NEXT-REVIEW.
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
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-ITEM.
           IF CRCM-ITEM-CNT < 9999999
               ADD 1 TO CRCM-ITEM-CNT
           END-IF
           IF CRCM-ITEM-CNT NOT < 5
               MOVE 'Y' TO CRCM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3680-ACCRUE-LATE-FEE.
           IF CRCM-LATE-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-LATE-FEE-YTD * CRCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO CRCT-CASH-ADV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * PU -> CA WHEN EXPIRY DATE IS PAST
      * CA -> PY WHEN EXPIRY DATE IS PAST
      * PY -> FC WHEN EXPIRY DATE IS PAST
      * FC -> LF WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-TRAN.
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E816 CUST ID REQUIRED
      * E334 LIMIT YTD NEGATIVE
      * E519 LIMIT YTD OVER 99999
      * E107 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CUST-ID = SPACES
                   MOVE 'E816' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-YTD < 0
                   MOVE 'E334' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-YTD > 99999.00
                   MOVE 'E519' TO WS-REJECT-CODE
               WHEN CRCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E107' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3710-CHECK-NEXT-REVIEW.
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
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E531 OPERATOR REQUIRED
      * E075 LIMIT YTD NEGATIVE
      * E901 LIMIT YTD OVER 99999
      * E235 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCT-OPERATOR = SPACES
                   MOVE 'E531' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-YTD < 0
                   MOVE 'E075' TO WS-REJECT-CODE
               WHEN CRCM-LIMIT-YTD > 99999.00
                   MOVE 'E901' TO WS-REJECT-CODE
               WHEN CRCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E235' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E189 CUST ID REQUIRED
      * E330 CASH ADV YTD NEGATIVE
      * E017 CASH ADV YTD OVER 50000
      * E686 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3730-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CUST-ID = SPACES
                   MOVE 'E189' TO WS-REJECT-CODE
               WHEN CRCM-CASH-ADV-YTD < 0
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN CRCM-CASH-ADV-YTD > 50000.00
                   MOVE 'E017' TO WS-REJECT-CODE
               WHEN CRCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E686' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3740-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-STATUS-CD
                   ADD CRCM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-LATE-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3750-COUNT-SEQ.
           IF CRCT-SEQ < 9999999
               ADD 1 TO CRCT-SEQ
           END-IF
           IF CRCT-SEQ NOT < 10
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-LAST-PAY.
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
      * DAILY INTEREST ON CASH ADV YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3770-ACCRUE-CASH-ADV.
           IF CRCM-CASH-ADV-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-CASH-ADV-YTD * CRCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CRCM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON LATE FEE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3780-ACCRUE-LATE-FEE.
           IF CRCM-LATE-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-LATE-FEE-AMT * CRCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CRCT-LIMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FINANCE GRID TIER FOR THE MIN DUE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3790-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FINANCE-GRID-RATE(1) TO CRCM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CRCM-MIN-DUE-YTD NOT < WS-FINANCE-GRID-LOW(WS-TX)
                   MOVE WS-FINANCE-GRID-RATE(WS-TX) TO CRCM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CRCM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CRCM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE LATE FEE YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3800-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-SOURCE-CD
                   ADD CRCM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-LATE-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PURCHASE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-PURCHASE.
           IF CRCM-PURCHASE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CRCM-PURCHASE-AMT * CRCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO CRCT-CASH-ADV-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN DUE YTD BY BILL CYCLE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3820-ACCUM-BILL-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-BILL-CYCLE-CD
                   ADD CRCM-MIN-DUE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-BILL-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CRCM-MIN-DUE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3830-LIMIT-PURCHASE.
           IF CRCM-PURCHASE-AMT < 100.00
               MOVE 100.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-PURCHASE-AMT > 500.00
               MOVE 500.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * MIN DUE YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3840-LIMIT-MIN-DUE.
           IF CRCM-MIN-DUE-YTD < 50.00
               MOVE 50.00 TO CRCM-MIN-DUE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-MIN-DUE-YTD > 50000.00
               MOVE 50000.00 TO CRCM-MIN-DUE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3850-COUNT-CYCLE.
           IF CRCM-CYCLE-CNT < 9999999
               ADD 1 TO CRCM-CYCLE-CNT
           END-IF
           IF CRCM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO CRCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3860-COUNT-CYCLE.
           IF CRCM-CYCLE-CNT < 9999999
               ADD 1 TO CRCM-CYCLE-CNT
           END-IF
           IF CRCM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO CRCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MIN DUE AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3870-LIMIT-MIN-DUE.
           IF CRCM-MIN-DUE-AMT < 0.00
               MOVE 0.00 TO CRCM-MIN-DUE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-MIN-DUE-AMT > 2500.00
               MOVE 2500.00 TO CRCM-MIN-DUE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH ADV AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3880-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = CRCM-CASH-ADV-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CRCM-CASH-ADV-AMT - (WS-WORK-AMT * 2)
           COMPUTE CRCM-FINANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-EFFECTIVE.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E964 CUST ID REQUIRED
      * E230 LIMIT AMT NEGATIVE
      * E117 LIMIT AMT OVER 1000000
      * E640 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3900-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-CUST-ID = SPACES
                   MOVE 'E964' TO WS-REJECT-CODE
               WHEN CRCT-LIMIT-AMT < 0
                   MOVE 'E230' TO WS-REJECT-CODE
               WHEN CRCT-LIMIT-AMT > 1000000.00
                   MOVE 'E117' TO WS-REJECT-CODE
               WHEN CRCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E640' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT LIMIT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3910-SPLIT-LIMIT.
           COMPUTE WS-WORK-AMT = CRCM-LIMIT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CRCM-LIMIT-AMT - (WS-WORK-AMT * 2)
           COMPUTE CRCM-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3920-AGE-NEXT-REVIEW.
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
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-LIMIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE MIN DUE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3930-ACCUM-CHANNEL.
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
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3940-CHECK-EFFECTIVE.
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
               MOVE 'Y' TO CRCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3950-DESCRIBE-STATUS.
           EVALUATE CRCM-STATUS-CD
               WHEN 'BAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DDY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DPB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UHF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WLL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XAW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3960-COUNT-SEQ.
           IF CRCT-SEQ < 9999999
               ADD 1 TO CRCT-SEQ
           END-IF
           IF CRCT-SEQ NOT < 3
               MOVE 'Y' TO CRCM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE MIN DUE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3970-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN CRCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CRCM-STOP-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CRCT-MIN-DUE-AMT *
                       CRCM-PENALTY-PCT / 200
               WHEN CRCT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN CRCT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CRCT-MIN-DUE-AMT *
                       CRCM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT CASH ADV AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3980-SPLIT-CASH-ADV.
           COMPUTE WS-WORK-AMT = CRCM-CASH-ADV-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CRCM-CASH-ADV-AMT - (WS-WORK-AMT * 4)
           COMPUTE CRCM-FINANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3990-FLAG-PURCHASE.
           IF CRCM-PURCHASE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-PURCHASE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4000-LIMIT-PAYMENT.
           IF CRCT-PAYMENT-AMT < 25.00
               MOVE 25.00 TO CRCT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCT-PAYMENT-AMT > 1000.00
               MOVE 1000.00 TO CRCT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BILL CYCLE CD:
      * B -> D WHEN OPEN DATE IS PAST
      * D -> E WHEN OPEN DATE IS PAST
      * E -> F WHEN OPEN DATE IS PAST
      * F -> H WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4010-SET-BILL-CYCLE.
           EVALUATE CRCM-BILL-CYCLE-CD
               WHEN 'B'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF CRCM-OPEN-DATE > 0 AND CRCM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO CRCM-BILL-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4020-CHECK-EFF.
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
               MOVE 'Y' TO CRCM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BRANCH CD:
      * CTB -> LNX WHEN EFF DATE IS PAST
      * LNX -> MGS WHEN EFF DATE IS PAST
      * MGS -> YNJ WHEN EFF DATE IS PAST
      * YNJ -> CTB WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4030-SET-BRANCH.
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
      * PURCHASE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4040-LIMIT-PURCHASE.
           IF CRCM-PURCHASE-AMT < 10.00
               MOVE 10.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCM-PURCHASE-AMT > 50000.00
               MOVE 50000.00 TO CRCM-PURCHASE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-CLOSE.
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
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CRCM-FINANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E362 ADDR REQUIRED
      * E354 FINANCE AMT NEGATIVE
      * E513 FINANCE AMT OVER 10000
      * E150 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4060-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CRCM-ADDR = SPACES
                   MOVE 'E362' TO WS-REJECT-CODE
               WHEN CRCM-FINANCE-AMT < 0
                   MOVE 'E354' TO WS-REJECT-CODE
               WHEN CRCM-FINANCE-AMT > 10000.00
                   MOVE 'E513' TO WS-REJECT-CODE
               WHEN CRCM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E150' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ADV YTD BY CARD TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4070-ACCUM-CARD-TYPE.
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
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4080-COUNT-RETRY.
           IF CRCM-RETRY-CNT < 9999999
               ADD 1 TO CRCM-RETRY-CNT
           END-IF
           IF CRCM-RETRY-CNT NOT < 3
               MOVE 'Y' TO CRCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * MIN DUE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4090-FLAG-MIN-DUE.
           IF CRCM-MIN-DUE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-MIN-DUE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PURCHASE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4100-FLAG-PURCHASE.
           IF CRCM-PURCHASE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCM-PURCHASE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4110-COUNT-ITEM.
           IF CRCM-ITEM-CNT < 9999999
               ADD 1 TO CRCM-ITEM-CNT
           END-IF
           IF CRCM-ITEM-CNT NOT < 6
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4120-LIMIT-LIMIT.
           IF CRCT-LIMIT-AMT < 50.00
               MOVE 50.00 TO CRCT-LIMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CRCT-LIMIT-AMT > 5000.00
               MOVE 5000.00 TO CRCT-LIMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * BAL -> DDY WHEN CLOSE DATE IS PAST
      * DDY -> DPB WHEN CLOSE DATE IS PAST
      * DPB -> UHF WHEN CLOSE DATE IS PAST
      * UHF -> WLL WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4130-SET-STATUS.
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
      * ACCUMULATE WORK AMT BY BRANCH CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4140-ACCUM-BRANCH.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CRCM-BRANCH-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CRCM-BRANCH-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4150-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO CRCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * LIMIT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4160-FLAG-LIMIT.
           IF CRCT-LIMIT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CRCT-LIMIT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
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
                   MOVE CRCAL-MIN-DUE-YTD
                     TO CRCM-MIN-DUE-YTD
                   MOVE CRCAL-FINANCE-YTD
                     TO CRCM-FINANCE-YTD
                   MOVE CRCAL-EXEMPT-FLAG
                     TO CRCM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7040-CALL-CRCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

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
                   MOVE CRELG-PURCHASE-AMT
                     TO CRCM-PURCHASE-AMT
                   MOVE CRELG-MIN-DUE-YTD
                     TO CRCM-MIN-DUE-YTD
                   MOVE CRELG-LIMIT-AMT
                     TO CRCM-LIMIT-AMT
                   MOVE CRELG-EXEMPT-FLAG
                     TO CRCM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-CRELG' TO ABND-PARAGRAPH
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
           DISPLAY 'CRCPURG READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CRCPURG' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
