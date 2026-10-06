      *================================================================*
      * PROGRAM  : DRPYREN                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DIVIDEND REINVESTMENT - YEAR-END. READS THE DRP     *
      *            TRANSACTIONS, UPDATES THE DRP MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. BANK HOLDING COMPANY STOCK      *
      *            DIVIDEND REINVESTMENT PLAN                          *
      * INPUT    : DRPMST (KSDS), DRPTRN, BANKCTL                      *
      * OUTPUT   : DRPOUT, DRPYRRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, DRFEE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1984-12-10 PQR  ORIGINAL                                       *
      * 1992-09-26 SKN  NEW REINVEST REPORT COLUMNS REQ3623            *
      * 1999-09-15 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-08-10 TWH  NEW CASH-PAY REPORT COLUMNS REQ3142            *
      * 2001-09-11 DPS  NEW CASH-PAY REPORT COLUMNS INC2465            *
      * 2006-04-19 TWH  WITHHOLD CALCULATION CORRECTED REQ5448         *
      * 2010-10-02 DPS  FEE LIMIT REVISED CHG4546                      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DRPYREN.
       AUTHOR. C R PATEL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "DRPMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS DRPM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "DRPTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "DRPOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "DRPYRRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY DRPMST.
       FD  TRAN-FILE.
       COPY DRPTRN.
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
      * WS-FEE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000013000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000052000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000422000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016000000000677500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016500000000196000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016600000000628500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026600000000257500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026700000000856500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026800000000596000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031800000000834500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032800000000025000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00042800000000621000'.
       01  WS-FEE-TIER-TBL REDEFINES WS-FEE-TIER-VALUES.
           05  WS-FEE-TIER-ROW OCCURS 12 TIMES.
               10  WS-FEE-TIER-LOW         PIC 9(09)V99.
               10  WS-FEE-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000551500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000476000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000057000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002700000000052000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003200000000713000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003450000000722500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005950000000100000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000589000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006100000000456500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006600000000071000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011600000000253000'.
       01  WS-FEE-BAND-TBL REDEFINES WS-FEE-BAND-VALUES.
           05  WS-FEE-BAND-ROW OCCURS 11 TIMES.
               10  WS-FEE-BAND-LOW         PIC 9(09)V99.
               10  WS-FEE-BAND-RATE        PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  DRPYREN  DIVIDEND REINVESTMENT'.
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
       COPY DRFEEP.

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
               MOVE 'DRPMST' TO ABND-FILE-NAME
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
           MOVE DRPT-KEY TO DRPM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-DESCRIBE-HOLDER-TYPE
               PERFORM 3010-ACCRUE-FEE
               PERFORM 3020-SET-SOURCE
               PERFORM 3030-COUNT-PAYMENT
               PERFORM 3040-DESCRIBE-CHANNEL
               PERFORM 3050-EDIT-CUST-ID
               PERFORM 3060-COUNT-PAYMENT
               PERFORM 3070-SPLIT-CASH-PAY
               PERFORM 3080-DESCRIBE-PLAN-OPTION
               PERFORM 3090-EDIT-ADDR
               PERFORM 3100-ACCUM-CYCLE
               PERFORM 3110-PRORATE-REINVEST
               PERFORM 3120-ACCUM-CYCLE
               PERFORM 3130-LIMIT-CASH-PAY
               PERFORM 3140-COUNT-ITEM
               PERFORM 3150-DESCRIBE-CHANNEL
               PERFORM 3160-LIMIT-FEE
               PERFORM 3170-SET-REASON
               PERFORM 3180-SELECT-DISCOUNT
               PERFORM 3190-SPLIT-REINVEST
               PERFORM 3200-ASSESS-OVERRIDE
               PERFORM 3210-SPLIT-SHARES-AMT
               PERFORM 3220-SPLIT-SHARES-AMT
               PERFORM 3230-LIMIT-FEE
               PERFORM 3240-COUNT-ITEM
               PERFORM 3250-SPLIT-DIVIDEND
               PERFORM 3260-SELECT-DISCOUNT
               PERFORM 3270-LIMIT-DIVIDEND
               PERFORM 3280-SELECT-DISCOUNT
               PERFORM 3290-CHECK-NEXT-REVIEW
               PERFORM 3300-PRORATE-SHARES-AMT
               PERFORM 3310-DESCRIBE-TRAN
               PERFORM 3320-SPLIT-WITHHOLD
               PERFORM 3330-CHECK-NOTICE
               PERFORM 3340-COUNT-PAYMENT
               PERFORM 3350-ASSESS-STOP
               PERFORM 3360-ASSESS-STOP
               PERFORM 3370-ASSESS-STOP
               PERFORM 3380-PRORATE-SHARES-AMT
               PERFORM 3390-LIMIT-WITHHOLD
               PERFORM 3400-SELECT-DISCOUNT
               PERFORM 3410-CHECK-MATURITY
               PERFORM 3420-EDIT-REJECT
               PERFORM 3430-FLAG-REINVEST
               PERFORM 3440-ASSESS-AUTO-RENEW
               PERFORM 3450-ACCUM-CHANNEL
               PERFORM 3460-FLAG-WITHHOLD
               PERFORM 3470-ASSESS-REVERSAL
               PERFORM 3480-DESCRIBE-REASON
               PERFORM 3490-AGE-OPEN
               PERFORM 3500-LIMIT-CASH-PAY
               PERFORM 3510-SELECT-RATE
               PERFORM 3520-FLAG-REINVEST
               PERFORM 3530-COUNT-EXCEPT
               PERFORM 3540-FLAG-WITHHOLD
               PERFORM 3550-COUNT-ITEM
               PERFORM 3560-COUNT-ITEM
               PERFORM 3570-SELECT-PENALTY-PCT
               PERFORM 3580-SPLIT-SHARES-AMT
               PERFORM 3590-CHECK-MATURITY
               PERFORM 3600-ACCRUE-REINVEST
               PERFORM 3610-ACCUM-CHANNEL
               PERFORM 3620-SELECT-PENALTY-PCT
               PERFORM 3630-LIMIT-REINVEST
               PERFORM 3640-EDIT-DESC
               PERFORM 3650-SPLIT-SHARES-AMT
               PERFORM 3660-ASSESS-SUSPEND
               PERFORM 3670-ACCUM-HOLDER-TYPE
               PERFORM 3680-PRORATE-FEE
               PERFORM 3690-EDIT-REJECT
               PERFORM 3700-LIMIT-FEE
               PERFORM 3710-EDIT-REF-NO
               PERFORM 3720-AGE-LAST-ACT
               PERFORM 3730-AGE-LAST-MAINT
               PERFORM 3740-SET-TRAN
               PERFORM 3750-FLAG-DIVIDEND
               PERFORM 3760-COUNT-EXCEPT
               PERFORM 3770-LIMIT-FEE
               PERFORM 3780-FLAG-CASH-PAY
               PERFORM 3790-LIMIT-CASH-PAY
               PERFORM 3800-DESCRIBE-SOURCE
               PERFORM 3810-DESCRIBE-TRAN
               PERFORM 3820-SELECT-DISCOUNT
               PERFORM 3830-EDIT-OPERATOR
               PERFORM 3840-AGE-OPEN
               PERFORM 3850-ACCRUE-REINVEST
               PERFORM 3860-AGE-LAST-MAINT
               PERFORM 3870-SET-PLAN-OPTION
               PERFORM 3880-ACCRUE-FEE
               PERFORM 3890-ASSESS-OVERRIDE
               PERFORM 3900-ASSESS-SUSPEND
               PERFORM 3910-SET-TRAN
               PERFORM 3920-FLAG-ITEM
               PERFORM 3930-SELECT-PENALTY-PCT
               PERFORM 3940-LIMIT-SHARES-AMT
               PERFORM 3950-SET-CHANNEL
               PERFORM 3960-LIMIT-DIVIDEND
               PERFORM 3970-SET-CYCLE
               PERFORM 3980-ACCRUE-PENALTY-PCT
               PERFORM 3990-SPLIT-REINVEST
               PERFORM 4000-SELECT-PENALTY-PCT
               PERFORM 4010-EDIT-CONTACT
               PERFORM 4020-ASSESS-STOP
               PERFORM 4030-ACCUM-REASON
               PERFORM 4040-SPLIT-SHARES-AMT
               PERFORM 4050-ACCRUE-DIVIDEND
               PERFORM 4060-FLAG-REINVEST
               PERFORM 4070-AGE-LAST-PAY
               PERFORM 4080-SPLIT-REINVEST
               PERFORM 4090-PRORATE-CASH-PAY
               PERFORM 4100-SELECT-PENALTY-PCT
               PERFORM 4110-FLAG-REINVEST
               PERFORM 4120-FLAG-DIVIDEND
               PERFORM 4130-SELECT-DISCOUNT
               PERFORM 4140-COUNT-ITEM
               PERFORM 4150-FLAG-WITHHOLD
               PERFORM 4160-ACCUM-PLAN-OPTION
               PERFORM 4170-FLAG-FEE
               PERFORM 4180-ACCUM-HOLDER-TYPE
               PERFORM 4190-LIMIT-ITEM
               PERFORM 4200-DESCRIBE-SOURCE
               PERFORM 4210-SPLIT-WITHHOLD
               PERFORM 4220-LIMIT-REINVEST
               PERFORM 4230-SPLIT-REINVEST
               PERFORM 4240-DESCRIBE-SOURCE
               PERFORM 4250-SELECT-RATE
               PERFORM 4260-EDIT-REJECT
               PERFORM 4270-ACCRUE-REINVEST
               PERFORM 4280-ASSESS-AUTO-RENEW
               PERFORM 4290-FLAG-DIVIDEND
               PERFORM 4300-FLAG-CASH-PAY
               PERFORM 4310-ACCRUE-DIVIDEND
               PERFORM 4320-EDIT-OPERATOR
               PERFORM 4330-LIMIT-ITEM
               PERFORM 4340-ASSESS-STOP
               PERFORM 4350-DESCRIBE-TRAN
               PERFORM 4360-SELECT-DISCOUNT
               PERFORM 4370-SPLIT-REINVEST
               PERFORM 4380-SELECT-RATE
               PERFORM 4390-PRORATE-SHARES-AMT
               PERFORM 4400-ACCRUE-REINVEST
               PERFORM 4410-COUNT-ITEM
               PERFORM 4420-ASSESS-SUSPEND
               PERFORM 4430-DESCRIBE-PLAN-OPTION
               PERFORM 4440-ACCRUE-SHARES-AMT
               PERFORM 4450-SELECT-PENALTY-PCT
               PERFORM 4460-CHECK-NEXT-DUE
               PERFORM 4470-EDIT-NAME
               PERFORM 4480-ACCUM-TRAN
               PERFORM 4490-DESCRIBE-REASON
               PERFORM 4500-SET-CHANNEL
               PERFORM 4510-ACCUM-PLAN-OPTION
               PERFORM 4520-FLAG-FEE
               PERFORM 4530-ASSESS-REVERSAL
               PERFORM 4540-SPLIT-DIVIDEND
               PERFORM 4550-LIMIT-REINVEST
               PERFORM 4560-DESCRIBE-CHANNEL
               PERFORM 4570-ACCRUE-WITHHOLD
               PERFORM 4580-SELECT-RATE
               PERFORM 4590-LIMIT-DIVIDEND
               PERFORM 4600-FLAG-FEE
               PERFORM 4610-COUNT-ITEM
               PERFORM 4620-ASSESS-OVERRIDE
               PERFORM 4630-SET-CHANNEL
               PERFORM 4640-EDIT-NAME
               PERFORM 4650-ASSESS-EXEMPT
               PERFORM 7030-CALL-DRFEE
               REWRITE DRP-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'DRPMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM DRP-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * TRANSLATE HOLDER TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3000-DESCRIBE-HOLDER-TYPE.
           EVALUATE DRPM-HOLDER-TYPE-CD
               WHEN 'C'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'L'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'M'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'X'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-FEE.
           IF DRPM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-FEE-AMT * DRPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-WITHHOLD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * BT -> FL WHEN LAST ACT DATE IS PAST
      * FL -> NK WHEN LAST ACT DATE IS PAST
      * NK -> YC WHEN LAST ACT DATE IS PAST
      * YC -> BT WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3020-SET-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FL' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FL'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NK' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NK'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YC' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YC'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BT' TO DRPM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-PAYMENT.
           IF DRPM-PAYMENT-CNT < 9999999
               ADD 1 TO DRPM-PAYMENT-CNT
           END-IF
           IF DRPM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E637 CUST ID REQUIRED
      * E360 DIVIDEND AMT NEGATIVE
      * E710 DIVIDEND AMT OVER 50000
      * E885 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3050-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CUST-ID = SPACES
                   MOVE 'E637' TO WS-REJECT-CODE
               WHEN DRPT-DIVIDEND-AMT < 0
                   MOVE 'E360' TO WS-REJECT-CODE
               WHEN DRPT-DIVIDEND-AMT > 50000.00
                   MOVE 'E710' TO WS-REJECT-CODE
               WHEN DRPM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E885' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3060-COUNT-PAYMENT.
           IF DRPM-PAYMENT-CNT < 9999999
               ADD 1 TO DRPM-PAYMENT-CNT
           END-IF
           IF DRPM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CASH PAY YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3070-SPLIT-CASH-PAY.
           COMPUTE WS-WORK-AMT = DRPM-CASH-PAY-YTD / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-CASH-PAY-YTD - (WS-WORK-AMT * 4)
           COMPUTE DRPM-DIVIDEND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN OPTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3080-DESCRIBE-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E579 ADDR REQUIRED
      * E900 DIVIDEND AMT NEGATIVE
      * E296 DIVIDEND AMT OVER 1000000
      * E490 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-ADDR = SPACES
                   MOVE 'E579' TO WS-REJECT-CODE
               WHEN DRPT-DIVIDEND-AMT < 0
                   MOVE 'E900' TO WS-REJECT-CODE
               WHEN DRPT-DIVIDEND-AMT > 1000000.00
                   MOVE 'E296' TO WS-REJECT-CODE
               WHEN DRPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E490' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CYCLE-CD
                   ADD DRPM-REINVEST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-REINVEST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH PAY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REINVEST YTD.
      *----------------------------------------------------------------*
       3110-PRORATE-REINVEST.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-REINVEST-YTD ROUNDED =
                   DRPM-CASH-PAY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-REINVEST-YTD
           END-IF
           ADD DRPM-REINVEST-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH PAY AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CYCLE-CD
                   ADD DRPM-CASH-PAY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-CASH-PAY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CASH PAY AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3130-LIMIT-CASH-PAY.
           IF DRPM-CASH-PAY-AMT < 50.00
               MOVE 50.00 TO DRPM-CASH-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-CASH-PAY-AMT > 5000.00
               MOVE 5000.00 TO DRPM-CASH-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3140-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 5
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3150-DESCRIBE-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3160-LIMIT-FEE.
           IF DRPT-FEE-AMT < 100.00
               MOVE 100.00 TO DRPT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPT-FEE-AMT > 2500.00
               MOVE 2500.00 TO DRPT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * B -> G WHEN LAST ACT DATE IS PAST
      * G -> J WHEN LAST ACT DATE IS PAST
      * J -> K WHEN LAST ACT DATE IS PAST
      * K -> L WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3170-SET-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO DRPM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE WITHHOLD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3180-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPM-WITHHOLD-AMT NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3190-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPM-REINVEST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-REINVEST-AMT - (WS-WORK-AMT *
               12)
           COMPUTE DRPT-DIVIDEND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE CASH PAY AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3200-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = DRPM-CASH-PAY-AMT *
                       DRPM-PENALTY-PCT / 200
               WHEN DRPT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN DRPT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = DRPM-CASH-PAY-AMT *
                       DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3210-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPT-SHARES-AMT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = DRPT-SHARES-AMT-AMT - (WS-WORK-AMT *
               2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3220-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-AMT - (WS-WORK-AMT *
               4)
           COMPUTE DRPM-WITHHOLD-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3230-LIMIT-FEE.
           IF DRPM-FEE-AMT < 0.00
               MOVE 0.00 TO DRPM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-FEE-AMT > 500.00
               MOVE 500.00 TO DRPM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3240-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 3
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3250-SPLIT-DIVIDEND.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE DRPM-DIVIDEND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3260-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DIVIDEND YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3270-LIMIT-DIVIDEND.
           IF DRPM-DIVIDEND-YTD < 25.00
               MOVE 25.00 TO DRPM-DIVIDEND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-DIVIDEND-YTD > 5000.00
               MOVE 5000.00 TO DRPM-DIVIDEND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE WITHHOLD YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPM-WITHHOLD-YTD NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3290-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO DRPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHARES AMT AMT.
      *----------------------------------------------------------------*
       3300-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPT-SHARES-AMT-AMT ROUNDED =
                   DRPT-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPT-SHARES-AMT-AMT
           END-IF
           ADD DRPT-SHARES-AMT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3310-DESCRIBE-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RI'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3320-SPLIT-WITHHOLD.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE DRPM-WITHHOLD-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3340-COUNT-PAYMENT.
           IF DRPM-PAYMENT-CNT < 9999999
               ADD 1 TO DRPM-PAYMENT-CNT
           END-IF
           IF DRPM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WITHHOLD YTD FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3350-ASSESS-STOP.
           EVALUATE TRUE
               WHEN DRPM-STOP-FLAG = 'Y'
                   MOVE ZERO TO DRPM-WITHHOLD-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   COMPUTE DRPM-WITHHOLD-YTD ROUNDED = WS-WORK-AMT *
                       DRPM-PENALTY-PCT / 200
               WHEN DRPM-PLAN-OPTION-CD = 'E'
                   MOVE 5.00 TO DRPM-WITHHOLD-YTD
               WHEN DRPM-PLAN-OPTION-CD = 'R'
                   MOVE 3.00 TO DRPM-WITHHOLD-YTD
               WHEN OTHER
                   COMPUTE DRPM-WITHHOLD-YTD ROUNDED = WS-WORK-AMT *
                       DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-WITHHOLD-YTD > 0 AND DRPM-WITHHOLD-YTD < 2.00
               MOVE 2.00 TO DRPM-WITHHOLD-YTD
           END-IF
           IF DRPM-WITHHOLD-YTD > 75.00
               MOVE 75.00 TO DRPM-WITHHOLD-YTD
           END-IF
           ADD DRPM-WITHHOLD-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE SHARES FEE ON THE WITHHOLD AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3360-ASSESS-STOP.
           EVALUATE TRUE
               WHEN DRPM-STOP-FLAG = 'Y'
                   MOVE ZERO TO DRPM-SHARES-AMT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   COMPUTE DRPM-SHARES-AMT-AMT ROUNDED =
                       DRPM-WITHHOLD-AMT * DRPM-RATE / 200
               WHEN DRPT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO DRPM-SHARES-AMT-AMT
               WHEN DRPT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO DRPM-SHARES-AMT-AMT
               WHEN OTHER
                   COMPUTE DRPM-SHARES-AMT-AMT ROUNDED =
                       DRPM-WITHHOLD-AMT * DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-SHARES-AMT-AMT > 0 AND DRPM-SHARES-AMT-AMT < 2.00
               MOVE 2.00 TO DRPM-SHARES-AMT-AMT
           END-IF
           IF DRPM-SHARES-AMT-AMT > 50.00
               MOVE 50.00 TO DRPM-SHARES-AMT-AMT
           END-IF
           ADD DRPM-SHARES-AMT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CASH PAY YTD FEE ON THE FEE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3370-ASSESS-STOP.
           EVALUATE TRUE
               WHEN DRPM-STOP-FLAG = 'Y'
                   MOVE ZERO TO DRPM-CASH-PAY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE DRPM-CASH-PAY-YTD ROUNDED = DRPT-FEE-AMT *
                       DRPM-DISCOUNT / 200
               WHEN DRPM-PLAN-OPTION-CD = 'E'
                   MOVE 3.00 TO DRPM-CASH-PAY-YTD
               WHEN DRPM-PLAN-OPTION-CD = 'R'
                   MOVE 10.00 TO DRPM-CASH-PAY-YTD
               WHEN OTHER
                   COMPUTE DRPM-CASH-PAY-YTD ROUNDED = DRPT-FEE-AMT *
                       DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-CASH-PAY-YTD > 0 AND DRPM-CASH-PAY-YTD < 10.00
               MOVE 10.00 TO DRPM-CASH-PAY-YTD
           END-IF
           IF DRPM-CASH-PAY-YTD > 250.00
               MOVE 250.00 TO DRPM-CASH-PAY-YTD
           END-IF
           ADD DRPM-CASH-PAY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHARES AMT YTD.
      *----------------------------------------------------------------*
       3380-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-SHARES-AMT-YTD
           END-IF
           ADD DRPM-SHARES-AMT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WITHHOLD AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3390-LIMIT-WITHHOLD.
           IF DRPM-WITHHOLD-AMT < 10.00
               MOVE 10.00 TO DRPM-WITHHOLD-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-WITHHOLD-AMT > 10000.00
               MOVE 10000.00 TO DRPM-WITHHOLD-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3410-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO DRPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E670 REJECT CD REQUIRED
      * E264 WITHHOLD AMT NEGATIVE
      * E257 WITHHOLD AMT OVER 99999
      * E360 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-REJECT-CD = SPACES
                   MOVE 'E670' TO WS-REJECT-CODE
               WHEN DRPM-WITHHOLD-AMT < 0
                   MOVE 'E264' TO WS-REJECT-CODE
               WHEN DRPM-WITHHOLD-AMT > 99999.00
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN DRPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E360' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3430-FLAG-REINVEST.
           IF DRPM-REINVEST-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE CASH PAY AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3440-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO DRPM-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED = DRPM-CASH-PAY-AMT
                       * DRPM-DISCOUNT / 200
               WHEN DRPT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO DRPM-DIVIDEND-AMT
               WHEN DRPT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO DRPM-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED = DRPM-CASH-PAY-AMT
                       * DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-DIVIDEND-AMT > 0 AND DRPM-DIVIDEND-AMT < 1.00
               MOVE 1.00 TO DRPM-DIVIDEND-AMT
           END-IF
           IF DRPM-DIVIDEND-AMT > 25.00
               MOVE 25.00 TO DRPM-DIVIDEND-AMT
           END-IF
           ADD DRPM-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3460-FLAG-WITHHOLD.
           IF DRPM-WITHHOLD-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRPM-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE CASH PAY AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3470-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO DRPM-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-STOP-FLAG = 'Y'
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED = DRPM-CASH-PAY-AMT
                       * DRPM-RATE / 200
               WHEN DRPT-TRAN-CD = 'DV'
                   MOVE 12.00 TO DRPM-DIVIDEND-AMT
               WHEN DRPT-TRAN-CD = 'RI'
                   MOVE 12.00 TO DRPM-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED = DRPM-CASH-PAY-AMT
                       * DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-DIVIDEND-AMT > 0 AND DRPM-DIVIDEND-AMT < 5.00
               MOVE 5.00 TO DRPM-DIVIDEND-AMT
           END-IF
           IF DRPM-DIVIDEND-AMT > 35.00
               MOVE 35.00 TO DRPM-DIVIDEND-AMT
           END-IF
           ADD DRPM-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3480-DESCRIBE-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'G'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'J'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPT-REINVEST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPT-REINVEST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD DRPT-REINVEST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPT-REINVEST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPT-REINVEST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CASH PAY AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3500-LIMIT-CASH-PAY.
           IF DRPM-CASH-PAY-AMT < 50.00
               MOVE 50.00 TO DRPM-CASH-PAY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-CASH-PAY-AMT > 10000.00
               MOVE 10000.00 TO DRPM-CASH-PAY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE CASH PAY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3510-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPM-CASH-PAY-AMT NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3520-FLAG-REINVEST.
           IF DRPM-REINVEST-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3530-COUNT-EXCEPT.
           IF DRPM-EXCEPT-CNT < 9999999
               ADD 1 TO DRPM-EXCEPT-CNT
           END-IF
           IF DRPM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3540-FLAG-WITHHOLD.
           IF DRPM-WITHHOLD-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3550-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 12
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3560-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 3
               MOVE 'Y' TO DRPM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE REINVEST YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3570-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPM-REINVEST-YTD NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3580-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPM-SHARES-AMT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-SHARES-AMT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO DRPM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3600-ACCRUE-REINVEST.
           IF DRPM-REINVEST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-REINVEST-AMT * DRPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO DRPT-REINVEST-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3610-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-CHANNEL-CD
                   ADD DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE SHARES AMT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPT-SHARES-AMT-AMT NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3630-LIMIT-REINVEST.
           IF DRPM-REINVEST-AMT < 25.00
               MOVE 25.00 TO DRPM-REINVEST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-REINVEST-AMT > 10000.00
               MOVE 10000.00 TO DRPM-REINVEST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E591 DESC REQUIRED
      * E468 DIVIDEND AMT NEGATIVE
      * E555 DIVIDEND AMT OVER 1000000
      * E037 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-DESC = SPACES
                   MOVE 'E591' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-AMT < 0
                   MOVE 'E468' TO WS-REJECT-CODE
               WHEN DRPM-DIVIDEND-AMT > 1000000.00
                   MOVE 'E555' TO WS-REJECT-CODE
               WHEN DRPT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E037' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3650-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPT-SHARES-AMT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = DRPT-SHARES-AMT-AMT - (WS-WORK-AMT *
               4)
           COMPUTE DRPM-DIVIDEND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REINVEST FEE ON THE WITHHOLD YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3660-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPM-REINVEST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE DRPM-REINVEST-AMT ROUNDED = DRPM-WITHHOLD-YTD
                       * DRPM-DISCOUNT / 200
               WHEN DRPM-PLAN-OPTION-CD = 'E'
                   MOVE 15.00 TO DRPM-REINVEST-AMT
               WHEN DRPM-PLAN-OPTION-CD = 'R'
                   MOVE 3.00 TO DRPM-REINVEST-AMT
               WHEN OTHER
                   COMPUTE DRPM-REINVEST-AMT ROUNDED = DRPM-WITHHOLD-YTD
                       * DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-REINVEST-AMT > 0 AND DRPM-REINVEST-AMT < 10.00
               MOVE 10.00 TO DRPM-REINVEST-AMT
           END-IF
           IF DRPM-REINVEST-AMT > 100.00
               MOVE 100.00 TO DRPM-REINVEST-AMT
           END-IF
           ADD DRPM-REINVEST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST YTD BY HOLDER TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3670-ACCUM-HOLDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-HOLDER-TYPE-CD
                   ADD DRPM-REINVEST-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-HOLDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-REINVEST-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHARES AMT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3680-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPT-FEE-AMT ROUNDED =
                   DRPM-SHARES-AMT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPT-FEE-AMT
           END-IF
           ADD DRPT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E753 REJECT CD REQUIRED
      * E019 REINVEST AMT NEGATIVE
      * E102 REINVEST AMT OVER 25000
      * E794 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3690-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-REJECT-CD = SPACES
                   MOVE 'E753' TO WS-REJECT-CODE
               WHEN DRPT-REINVEST-AMT < 0
                   MOVE 'E019' TO WS-REJECT-CODE
               WHEN DRPT-REINVEST-AMT > 25000.00
                   MOVE 'E102' TO WS-REJECT-CODE
               WHEN DRPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E794' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3700-LIMIT-FEE.
           IF DRPT-FEE-AMT < 0.00
               MOVE 0.00 TO DRPT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPT-FEE-AMT > 10000.00
               MOVE 10000.00 TO DRPT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E689 REF NO REQUIRED
      * E434 WITHHOLD YTD NEGATIVE
      * E358 WITHHOLD YTD OVER 250000
      * E357 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3710-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-REF-NO = SPACES
                   MOVE 'E689' TO WS-REJECT-CODE
               WHEN DRPM-WITHHOLD-YTD < 0
                   MOVE 'E434' TO WS-REJECT-CODE
               WHEN DRPM-WITHHOLD-YTD > 250000.00
                   MOVE 'E358' TO WS-REJECT-CODE
               WHEN DRPM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E357' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3720-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-ACT-DATE > 0 AND DRPM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-WITHHOLD-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-WITHHOLD-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-WITHHOLD-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-WITHHOLD-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-WITHHOLD-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3730-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-SHARES-AMT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DV -> RI WHEN EFFECTIVE DATE IS PAST
      * RI -> CP WHEN EFFECTIVE DATE IS PAST
      * CP -> WH WHEN EFFECTIVE DATE IS PAST
      * WH -> SL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RI' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RI'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WH'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SL' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DIVIDEND AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3750-FLAG-DIVIDEND.
           IF DRPM-DIVIDEND-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-DIVIDEND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3760-COUNT-EXCEPT.
           IF DRPM-EXCEPT-CNT < 9999999
               ADD 1 TO DRPM-EXCEPT-CNT
           END-IF
           IF DRPM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO DRPM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3770-LIMIT-FEE.
           IF DRPM-FEE-YTD < 10.00
               MOVE 10.00 TO DRPM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-FEE-YTD > 5000.00
               MOVE 5000.00 TO DRPM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CASH PAY YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3780-FLAG-CASH-PAY.
           IF DRPM-CASH-PAY-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-CASH-PAY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CASH PAY YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3790-LIMIT-CASH-PAY.
           IF DRPM-CASH-PAY-YTD < 10.00
               MOVE 10.00 TO DRPM-CASH-PAY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-CASH-PAY-YTD > 50000.00
               MOVE 50000.00 TO DRPM-CASH-PAY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3800-DESCRIBE-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RI'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'CP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE SHARES AMT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3820-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF DRPT-SHARES-AMT-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E986 OPERATOR REQUIRED
      * E402 FEE YTD NEGATIVE
      * E768 FEE YTD OVER 10000
      * E328 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3830-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-OPERATOR = SPACES
                   MOVE 'E986' TO WS-REJECT-CODE
               WHEN DRPM-FEE-YTD < 0
                   MOVE 'E402' TO WS-REJECT-CODE
               WHEN DRPM-FEE-YTD > 10000.00
                   MOVE 'E768' TO WS-REJECT-CODE
               WHEN DRPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E328' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3840-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPT-DIVIDEND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3850-ACCRUE-REINVEST.
           IF DRPT-REINVEST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPT-REINVEST-AMT * DRPM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-MAINT-DATE > 0 AND DRPM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PLAN OPTION CD:
      * E -> R WHEN POST DATE IS PAST
      * R -> T WHEN POST DATE IS PAST
      * T -> Z WHEN POST DATE IS PAST
      * Z -> E WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF DRPT-POST-DATE > 0 AND DRPT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO DRPM-PLAN-OPTION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3880-ACCRUE-FEE.
           IF DRPM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-FEE-AMT * DRPM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO DRPT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REINVEST FEE ON THE FEE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3890-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRPT-REINVEST-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-STOP-FLAG = 'Y'
                   COMPUTE DRPT-REINVEST-AMT ROUNDED = DRPM-FEE-AMT *
                       DRPM-PENALTY-PCT / 200
               WHEN DRPT-TRAN-CD = 'DV'
                   MOVE 7.00 TO DRPT-REINVEST-AMT
               WHEN DRPT-TRAN-CD = 'RI'
                   MOVE 20.00 TO DRPT-REINVEST-AMT
               WHEN OTHER
                   COMPUTE DRPT-REINVEST-AMT ROUNDED = DRPM-FEE-AMT *
                       DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPT-REINVEST-AMT > 0 AND DRPT-REINVEST-AMT < 10.00
               MOVE 10.00 TO DRPT-REINVEST-AMT
           END-IF
           IF DRPT-REINVEST-AMT > 50.00
               MOVE 50.00 TO DRPT-REINVEST-AMT
           END-IF
           ADD DRPT-REINVEST-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CASH PAY FEE ON THE DIVIDEND YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3900-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPM-CASH-PAY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-PREFERRED-FLAG = 'Y'
                   COMPUTE DRPM-CASH-PAY-AMT ROUNDED = DRPM-DIVIDEND-YTD
                       * DRPM-DISCOUNT / 200
               WHEN DRPM-PLAN-OPTION-CD = 'E'
                   MOVE 7.00 TO DRPM-CASH-PAY-AMT
               WHEN DRPM-PLAN-OPTION-CD = 'R'
                   MOVE 15.00 TO DRPM-CASH-PAY-AMT
               WHEN OTHER
                   COMPUTE DRPM-CASH-PAY-AMT ROUNDED = DRPM-DIVIDEND-YTD
                       * DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPM-CASH-PAY-AMT > 0 AND DRPM-CASH-PAY-AMT < 5.00
               MOVE 5.00 TO DRPM-CASH-PAY-AMT
           END-IF
           IF DRPM-CASH-PAY-AMT > 250.00
               MOVE 250.00 TO DRPM-CASH-PAY-AMT
           END-IF
           ADD DRPM-CASH-PAY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DV -> RI WHEN OPEN DATE IS PAST
      * RI -> CP WHEN OPEN DATE IS PAST
      * CP -> WH WHEN OPEN DATE IS PAST
      * WH -> SL WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RI' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RI'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WH' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WH'
                   IF DRPM-OPEN-DATE > 0 AND DRPM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SL' TO DRPT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3920-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE SHARES AMT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF DRPM-SHARES-AMT-YTD NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHARES AMT AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3940-LIMIT-SHARES-AMT.
           IF DRPT-SHARES-AMT-AMT < 0.00
               MOVE 0.00 TO DRPT-SHARES-AMT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPT-SHARES-AMT-AMT > 1000.00
               MOVE 1000.00 TO DRPT-SHARES-AMT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NOTICE DATE IS PAST
      * BAT -> MAL WHEN NOTICE DATE IS PAST
      * MAL -> TEL WHEN NOTICE DATE IS PAST
      * TEL -> ONL WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3950-SET-CHANNEL.
           EVALUATE DRPT-CHANNEL-CD
               WHEN 'BRN'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF DRPM-NOTICE-DATE > 0 AND DRPM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO DRPT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DIVIDEND AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3960-LIMIT-DIVIDEND.
           IF DRPT-DIVIDEND-AMT < 10.00
               MOVE 10.00 TO DRPT-DIVIDEND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPT-DIVIDEND-AMT > 2500.00
               MOVE 2500.00 TO DRPT-DIVIDEND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DE -> FB WHEN EFFECTIVE DATE IS PAST
      * FB -> PC WHEN EFFECTIVE DATE IS PAST
      * PC -> WY WHEN EFFECTIVE DATE IS PAST
      * WY -> XD WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-CYCLE.
           EVALUATE DRPM-CYCLE-CD
               WHEN 'DE'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FB'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PC' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PC'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'WY'
                   IF DRPM-EFFECTIVE-DATE > 0 AND DRPM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XD' TO DRPM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3980-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * DRPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-REINVEST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPT-REINVEST-AMT / 12
           COMPUTE WS-WORK-AMT-2 = DRPT-REINVEST-AMT - (WS-WORK-AMT *
               12)
           COMPUTE DRPM-DIVIDEND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE DIVIDEND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4000-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF DRPM-DIVIDEND-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E789 CONTACT REQUIRED
      * E735 REINVEST AMT NEGATIVE
      * E284 REINVEST AMT OVER 50000
      * E238 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-CONTACT = SPACES
                   MOVE 'E789' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-AMT < 0
                   MOVE 'E735' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-AMT > 50000.00
                   MOVE 'E284' TO WS-REJECT-CODE
               WHEN DRPM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E238' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE DIVIDEND AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4020-ASSESS-STOP.
           EVALUATE TRUE
               WHEN DRPM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = DRPT-DIVIDEND-AMT *
                       DRPM-PENALTY-PCT / 200
               WHEN DRPM-HOLDER-TYPE-CD = 'C'
                   MOVE 10.00 TO WS-WORK-AMT-2
               WHEN DRPM-HOLDER-TYPE-CD = 'H'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = DRPT-DIVIDEND-AMT *
                       DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE SHARES AMT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-REASON-CD
                   ADD DRPM-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-SHARES-AMT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHARES AMT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4040-SPLIT-SHARES-AMT.
           COMPUTE WS-WORK-AMT = DRPT-SHARES-AMT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = DRPT-SHARES-AMT-AMT - (WS-WORK-AMT *
               2)
           COMPUTE DRPM-DIVIDEND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DIVIDEND AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4050-ACCRUE-DIVIDEND.
           IF DRPT-DIVIDEND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPT-DIVIDEND-AMT * DRPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-WITHHOLD-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4060-FLAG-REINVEST.
           IF DRPM-REINVEST-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4070-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF DRPM-LAST-PAY-DATE > 0 AND DRPM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD DRPM-REINVEST-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REINVEST YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4080-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPM-REINVEST-YTD / 3
           COMPUTE WS-WORK-AMT-2 = DRPM-REINVEST-YTD - (WS-WORK-AMT * 3)
           COMPUTE DRPM-SHARES-AMT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DIVIDEND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH PAY YTD.
      *----------------------------------------------------------------*
       4090-PRORATE-CASH-PAY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-CASH-PAY-YTD ROUNDED =
                   DRPT-DIVIDEND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO DRPM-CASH-PAY-YTD
           END-IF
           ADD DRPM-CASH-PAY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE CASH PAY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4100-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF DRPM-CASH-PAY-YTD NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4110-FLAG-REINVEST.
           IF DRPM-REINVEST-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-REINVEST-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DIVIDEND AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4120-FLAG-DIVIDEND.
           IF DRPM-DIVIDEND-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE DRPM-DIVIDEND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE SHARES AMT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4130-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPM-SHARES-AMT-YTD NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4140-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 3
               MOVE 'Y' TO DRPM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WITHHOLD AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4150-FLAG-WITHHOLD.
           IF DRPM-WITHHOLD-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-WITHHOLD-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WITHHOLD AMT BY PLAN OPTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4160-ACCUM-PLAN-OPTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-PLAN-OPTION-CD
                   ADD DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-PLAN-OPTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-WITHHOLD-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4170-FLAG-FEE.
           IF DRPT-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DIVIDEND YTD BY HOLDER TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4180-ACCUM-HOLDER-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-HOLDER-TYPE-CD
                   ADD DRPM-DIVIDEND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-HOLDER-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-DIVIDEND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4190-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 500.00
               MOVE 500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4200-DESCRIBE-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT WITHHOLD AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4210-SPLIT-WITHHOLD.
           COMPUTE WS-WORK-AMT = DRPM-WITHHOLD-AMT / 2
           COMPUTE WS-WORK-AMT-2 = DRPM-WITHHOLD-AMT - (WS-WORK-AMT * 2)
           COMPUTE DRPM-CASH-PAY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4220-LIMIT-REINVEST.
           IF DRPM-REINVEST-YTD < 10.00
               MOVE 10.00 TO DRPM-REINVEST-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-REINVEST-YTD > 10000.00
               MOVE 10000.00 TO DRPM-REINVEST-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4230-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPM-REINVEST-YTD / 12
           COMPUTE WS-WORK-AMT-2 = DRPM-REINVEST-YTD - (WS-WORK-AMT *
               12)
           COMPUTE DRPM-DIVIDEND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4240-DESCRIBE-SOURCE.
           EVALUATE DRPM-SOURCE-CD
               WHEN 'BT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NK'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE WITHHOLD AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4250-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF DRPM-WITHHOLD-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E099 REJECT CD REQUIRED
      * E149 REINVEST AMT NEGATIVE
      * E249 REINVEST AMT OVER 25000
      * E297 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4260-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-REJECT-CD = SPACES
                   MOVE 'E099' TO WS-REJECT-CODE
               WHEN DRPT-REINVEST-AMT < 0
                   MOVE 'E149' TO WS-REJECT-CODE
               WHEN DRPT-REINVEST-AMT > 25000.00
                   MOVE 'E249' TO WS-REJECT-CODE
               WHEN DRPM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E297' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4270-ACCRUE-REINVEST.
           IF DRPT-REINVEST-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPT-REINVEST-AMT * DRPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-WITHHOLD-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE REINVEST AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4280-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN DRPM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = DRPM-REINVEST-AMT *
                       DRPM-DISCOUNT / 200
               WHEN DRPM-CYCLE-CD = 'DE'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN DRPM-CYCLE-CD = 'FB'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = DRPM-REINVEST-AMT *
                       DRPM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DIVIDEND YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4290-FLAG-DIVIDEND.
           IF DRPM-DIVIDEND-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-DIVIDEND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CASH PAY YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4300-FLAG-CASH-PAY.
           IF DRPM-CASH-PAY-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE DRPM-CASH-PAY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DIVIDEND YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4310-ACCRUE-DIVIDEND.
           IF DRPM-DIVIDEND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-DIVIDEND-YTD * DRPM-RATE / 100 / 360
               ADD WS-WORK-AMT TO DRPM-REINVEST-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E291 OPERATOR REQUIRED
      * E991 REINVEST AMT NEGATIVE
      * E393 REINVEST AMT OVER 99999
      * E413 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4320-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPT-OPERATOR = SPACES
                   MOVE 'E291' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-AMT < 0
                   MOVE 'E991' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-AMT > 99999.00
                   MOVE 'E393' TO WS-REJECT-CODE
               WHEN DRPM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E413' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4330-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE CASH PAY AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4340-ASSESS-STOP.
           EVALUATE TRUE
               WHEN DRPM-STOP-FLAG = 'Y'
                   MOVE ZERO TO DRPT-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPM-CASH-PAY-AMT
                       * DRPM-PENALTY-PCT / 200
               WHEN DRPT-TRAN-CD = 'DV'
                   MOVE 5.00 TO DRPT-DIVIDEND-AMT
               WHEN DRPT-TRAN-CD = 'RI'
                   MOVE 10.00 TO DRPT-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPT-DIVIDEND-AMT ROUNDED = DRPM-CASH-PAY-AMT
                       * DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPT-DIVIDEND-AMT > 0 AND DRPT-DIVIDEND-AMT < 5.00
               MOVE 5.00 TO DRPT-DIVIDEND-AMT
           END-IF
           IF DRPT-DIVIDEND-AMT > 75.00
               MOVE 75.00 TO DRPT-DIVIDEND-AMT
           END-IF
           ADD DRPT-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4350-DESCRIBE-TRAN.
           EVALUATE DRPT-TRAN-CD
               WHEN 'DV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RI'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'CP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE DIVIDEND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4360-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO DRPM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF DRPT-DIVIDEND-AMT NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO DRPM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REINVEST AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4370-SPLIT-REINVEST.
           COMPUTE WS-WORK-AMT = DRPM-REINVEST-AMT / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-REINVEST-AMT - (WS-WORK-AMT * 4)
           COMPUTE DRPM-DIVIDEND-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE REINVEST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4380-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF DRPM-REINVEST-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REINVEST YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHARES AMT YTD.
      *----------------------------------------------------------------*
       4390-PRORATE-SHARES-AMT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE DRPM-SHARES-AMT-YTD ROUNDED =
                   DRPM-REINVEST-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO DRPM-SHARES-AMT-YTD
           END-IF
           ADD DRPM-SHARES-AMT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REINVEST YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4400-ACCRUE-REINVEST.
           IF DRPM-REINVEST-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-REINVEST-YTD * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-SHARES-AMT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4410-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 10
               MOVE 'Y' TO DRPM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE FEE YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4420-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN DRPM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO DRPT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   COMPUTE DRPT-FEE-AMT ROUNDED = DRPM-FEE-YTD *
                       DRPM-DISCOUNT / 200
               WHEN DRPM-SOURCE-CD = 'BT'
                   MOVE 3.00 TO DRPT-FEE-AMT
               WHEN DRPM-SOURCE-CD = 'FL'
                   MOVE 7.00 TO DRPT-FEE-AMT
               WHEN OTHER
                   COMPUTE DRPT-FEE-AMT ROUNDED = DRPM-FEE-YTD *
                       DRPM-DISCOUNT / 100
           END-EVALUATE
           IF DRPT-FEE-AMT > 0 AND DRPT-FEE-AMT < 5.00
               MOVE 5.00 TO DRPT-FEE-AMT
           END-IF
           IF DRPT-FEE-AMT > 35.00
               MOVE 35.00 TO DRPT-FEE-AMT
           END-IF
           ADD DRPT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE PLAN OPTION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4430-DESCRIBE-PLAN-OPTION.
           EVALUATE DRPM-PLAN-OPTION-CD
               WHEN 'E'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHARES AMT AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4440-ACCRUE-SHARES-AMT.
           IF DRPT-SHARES-AMT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPT-SHARES-AMT-AMT * DRPM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO DRPM-CASH-PAY-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE REINVEST AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4450-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO DRPM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF DRPT-REINVEST-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO DRPM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4460-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF DRPM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE DRPM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO DRPM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E209 NAME REQUIRED
      * E083 REINVEST AMT NEGATIVE
      * E042 REINVEST AMT OVER 5000
      * E878 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4470-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-NAME = SPACES
                   MOVE 'E209' TO WS-REJECT-CODE
               WHEN DRPT-REINVEST-AMT < 0
                   MOVE 'E083' TO WS-REJECT-CODE
               WHEN DRPT-REINVEST-AMT > 5000.00
                   MOVE 'E042' TO WS-REJECT-CODE
               WHEN DRPM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E878' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REINVEST AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4480-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPT-TRAN-CD
                   ADD DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPT-REINVEST-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4490-DESCRIBE-REASON.
           EVALUATE DRPM-REASON-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'G'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'J'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'K'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * RM -> VP WHEN MATURITY DATE IS PAST
      * VP -> YS WHEN MATURITY DATE IS PAST
      * YS -> RM WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4500-SET-CHANNEL.
           EVALUATE DRPM-CHANNEL-CD
               WHEN 'RM'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VP' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VP'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YS' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YS'
                   IF DRPM-MATURITY-DATE > 0 AND DRPM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SHARES AMT YTD BY PLAN OPTION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4510-ACCUM-PLAN-OPTION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = DRPM-PLAN-OPTION-CD
                   ADD DRPM-SHARES-AMT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE DRPM-PLAN-OPTION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE DRPM-SHARES-AMT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4520-FLAG-FEE.
           IF DRPM-FEE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DIVIDEND FEE ON THE SHARES AMT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4530-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN DRPT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO DRPM-DIVIDEND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-STOP-FLAG = 'Y'
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED =
                       DRPM-SHARES-AMT-AMT * DRPM-PENALTY-PCT / 200
               WHEN DRPT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO DRPM-DIVIDEND-AMT
               WHEN DRPT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO DRPM-DIVIDEND-AMT
               WHEN OTHER
                   COMPUTE DRPM-DIVIDEND-AMT ROUNDED =
                       DRPM-SHARES-AMT-AMT * DRPM-PENALTY-PCT / 100
           END-EVALUATE
           IF DRPM-DIVIDEND-AMT > 0 AND DRPM-DIVIDEND-AMT < 5.00
               MOVE 5.00 TO DRPM-DIVIDEND-AMT
           END-IF
           IF DRPM-DIVIDEND-AMT > 50.00
               MOVE 50.00 TO DRPM-DIVIDEND-AMT
           END-IF
           ADD DRPM-DIVIDEND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT DIVIDEND YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4540-SPLIT-DIVIDEND.
           COMPUTE WS-WORK-AMT = DRPM-DIVIDEND-YTD / 4
           COMPUTE WS-WORK-AMT-2 = DRPM-DIVIDEND-YTD - (WS-WORK-AMT * 4)
           COMPUTE DRPM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REINVEST AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4550-LIMIT-REINVEST.
           IF DRPM-REINVEST-AMT < 50.00
               MOVE 50.00 TO DRPM-REINVEST-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-REINVEST-AMT > 1000.00
               MOVE 1000.00 TO DRPM-REINVEST-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4560-DESCRIBE-CHANNEL.
           EVALUATE DRPM-CHANNEL-CD
               WHEN 'RM'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YS'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WITHHOLD YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4570-ACCRUE-WITHHOLD.
           IF DRPM-WITHHOLD-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   DRPM-WITHHOLD-YTD * DRPM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO DRPM-SHARES-AMT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4580-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-TIER-RATE(1) TO DRPM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF DRPT-FEE-AMT NOT < WS-FEE-TIER-LOW(WS-TX)
                   MOVE WS-FEE-TIER-RATE(WS-TX) TO DRPM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF DRPM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO DRPM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DIVIDEND YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4590-LIMIT-DIVIDEND.
           IF DRPM-DIVIDEND-YTD < 50.00
               MOVE 50.00 TO DRPM-DIVIDEND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF DRPM-DIVIDEND-YTD > 10000.00
               MOVE 10000.00 TO DRPM-DIVIDEND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4600-FLAG-FEE.
           IF DRPT-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE DRPT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4610-COUNT-ITEM.
           IF DRPM-ITEM-CNT < 9999999
               ADD 1 TO DRPM-ITEM-CNT
           END-IF
           IF DRPM-ITEM-CNT NOT < 5
               MOVE 'Y' TO DRPM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE REINVEST AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4620-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN DRPM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO DRPM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-PREFERRED-FLAG = 'Y'
                   COMPUTE DRPM-FEE-AMT ROUNDED = DRPT-REINVEST-AMT *
                       DRPM-RATE / 200
               WHEN DRPT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO DRPM-FEE-AMT
               WHEN DRPT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO DRPM-FEE-AMT
               WHEN OTHER
                   COMPUTE DRPM-FEE-AMT ROUNDED = DRPT-REINVEST-AMT *
                       DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-FEE-AMT > 0 AND DRPM-FEE-AMT < 2.00
               MOVE 2.00 TO DRPM-FEE-AMT
           END-IF
           IF DRPM-FEE-AMT > 35.00
               MOVE 35.00 TO DRPM-FEE-AMT
           END-IF
           ADD DRPM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * RM -> VP WHEN NEXT REVIEW DATE IS PAST
      * VP -> YS WHEN NEXT REVIEW DATE IS PAST
      * YS -> RM WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4630-SET-CHANNEL.
           EVALUATE DRPM-CHANNEL-CD
               WHEN 'RM'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'VP' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VP'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'YS' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'YS'
                   IF DRPM-NEXT-REVIEW-DATE > 0 AND
                       DRPM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'RM' TO DRPM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E605 NAME REQUIRED
      * E429 REINVEST AMT NEGATIVE
      * E146 REINVEST AMT OVER 50000
      * E444 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4640-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN DRPM-NAME = SPACES
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-AMT < 0
                   MOVE 'E429' TO WS-REJECT-CODE
               WHEN DRPM-REINVEST-AMT > 50000.00
                   MOVE 'E146' TO WS-REJECT-CODE
               WHEN DRPM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E444' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REINVEST YTD FEE ON THE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4650-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN DRPM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO DRPM-REINVEST-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN DRPM-PREFERRED-FLAG = 'Y'
                   COMPUTE DRPM-REINVEST-YTD ROUNDED = DRPM-FEE-AMT *
                       DRPM-RATE / 200
               WHEN DRPM-HOLDER-TYPE-CD = 'C'
                   MOVE 20.00 TO DRPM-REINVEST-YTD
               WHEN DRPM-HOLDER-TYPE-CD = 'H'
                   MOVE 15.00 TO DRPM-REINVEST-YTD
               WHEN OTHER
                   COMPUTE DRPM-REINVEST-YTD ROUNDED = DRPM-FEE-AMT *
                       DRPM-RATE / 100
           END-EVALUATE
           IF DRPM-REINVEST-YTD > 0 AND DRPM-REINVEST-YTD < 1.00
               MOVE 1.00 TO DRPM-REINVEST-YTD
           END-IF
           IF DRPM-REINVEST-YTD > 50.00
               MOVE 50.00 TO DRPM-REINVEST-YTD
           END-IF
           ADD DRPM-REINVEST-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PASS THE DRP FIELDS TO DRFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-DRFEE.
           INITIALIZE DRFEE-PARMS
           MOVE DRPM-CASH-PAY-YTD
             TO DRFEE-CASH-PAY-YTD
           MOVE DRPM-WITHHOLD-YTD
             TO DRFEE-WITHHOLD-YTD
           MOVE DRPM-SHARES-AMT-AMT
             TO DRFEE-SHARES-AMT-AMT
           MOVE DRPM-FEE-AMT
             TO DRFEE-FEE-AMT
           MOVE DRPM-REINVEST-YTD
             TO DRFEE-REINVEST-YTD
           MOVE DRPM-SHARES-AMT-YTD
             TO DRFEE-SHARES-AMT-YTD
           MOVE DRPM-NOTICE-DATE
             TO DRFEE-NOTICE-DATE
           MOVE DRPM-LAST-PAY-DATE
             TO DRFEE-LAST-PAY-DATE
           MOVE DRPM-NEXT-REVIEW-DATE
             TO DRFEE-NEXT-REVIEW-DATE
           MOVE DRPM-OPEN-DATE
             TO DRFEE-OPEN-DATE
           MOVE DRPM-REASON-CD
             TO DRFEE-REASON-CD
           MOVE DRPM-CYCLE-CD
             TO DRFEE-CYCLE-CD
           MOVE DRPM-PLAN-OPTION-CD
             TO DRFEE-PLAN-OPTION-CD
           MOVE DRPM-HOLDER-TYPE-CD
             TO DRFEE-HOLDER-TYPE-CD
           MOVE DRPM-OVERRIDE-FLAG
             TO DRFEE-OVERRIDE-FLAG
           MOVE DRPM-SUSPEND-FLAG
             TO DRFEE-SUSPEND-FLAG
           MOVE DRPM-PREFERRED-FLAG
             TO DRFEE-PREFERRED-FLAG
           MOVE DRPM-PAYMENT-CNT
             TO DRFEE-PAYMENT-CNT
           MOVE DRPM-NOTICE-CNT
             TO DRFEE-NOTICE-CNT
           MOVE DRPM-DISCOUNT
             TO DRFEE-DISCOUNT
           MOVE DRPM-RATE
             TO DRFEE-RATE
           MOVE DRPM-DESC
             TO DRFEE-DESC
           MOVE DRPM-ADDR
             TO DRFEE-ADDR
           CALL 'DRFEE' USING DRFEE-PARMS
           EVALUATE DRFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE DRFEE-CASH-PAY-YTD
                     TO DRPM-CASH-PAY-YTD
                   MOVE DRFEE-WITHHOLD-YTD
                     TO DRPM-WITHHOLD-YTD
                   MOVE DRFEE-FEE-AMT
                     TO DRPM-FEE-AMT
                   MOVE DRFEE-SHARES-AMT-YTD
                     TO DRPM-SHARES-AMT-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-DRFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE DRPT-KEY TO RD-KEY
           MOVE DRPT-TRAN-CD TO RD-CODE
           MOVE DRPT-FEE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE DRPT-KEY TO RD-KEY
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
           DISPLAY 'DRPYREN READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'DRPYREN' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
