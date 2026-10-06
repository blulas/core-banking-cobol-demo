      *================================================================*
      * PROGRAM  : ESCACCR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MORTGAGE ESCROW - DAILY ACCRUAL. READS THE ESC      *
      *            TRANSACTIONS, UPDATES THE ESC MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. ESCROW ANALYSIS, TAX AND        *
      *            INSURANCE DISBURSEMENT AND SHORTAGE                 *
      * INPUT    : ESCMST (KSDS), ESCTRN, BANKCTL                      *
      * OUTPUT   : ESCOUT, ESCACRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ESFEE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-06-01 DPS  ORIGINAL                                       *
      * 1994-07-07 GAF  PAYMENT ROUNDING - HALF UP INC2752             *
      * 2000-01-19 GAF  SHORTAGE CUTOFF MOVED CHG1819                  *
      * 2001-12-01 BHS  PERFORMANCE - SHORTAGE READ AHEAD CHG9243      *
      * 2007-07-21 DPS  NEW INSURANCE REPORT COLUMNS INC8229           *
      * 2007-10-08 JLM  SHORTAGE EXCEPTIONS TO OPS QUEUE INC4899       *
      * 2008-10-15 TWH  SURPLUS LIMIT REVISED INC5416                  *
      * 2012-06-25 LMB  CUSHION EXCEPTIONS TO OPS QUEUE PRB8951        *
      * 2013-12-16 TWH  NEW INSURANCE REPORT COLUMNS CHG5384           *
      * 2013-01-26 RJT  CUSHION CALCULATION CORRECTED CHG0872          *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESCACCR.
       AUTHOR. B H SANDERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "ESCMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ESCM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "ESCTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "ESCOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "ESCACRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY ESCMST.
       FD  TRAN-FILE.
       COPY ESCTRN.
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
                                           VALUE 12.000000.
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
      * WS-INSURANC-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INSURANC-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000554500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000381000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000546500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001600000000619000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000637500'.
       01  WS-INSURANC-BAND-TBL REDEFINES WS-INSURANC-BAND-VALUES.
           05  WS-INSURANC-BAND-ROW OCCURS 5 TIMES.
               10  WS-INSURANC-BAND-LOW    PIC 9(09)V99.
               10  WS-INSURANC-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-TAX-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TAX-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000759000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000677500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000530500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010750000000684500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020750000000153000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020800000000692000'.
       01  WS-TAX-TIER-TBL REDEFINES WS-TAX-TIER-VALUES.
           05  WS-TAX-TIER-ROW OCCURS 6 TIMES.
               10  WS-TAX-TIER-LOW         PIC 9(09)V99.
               10  WS-TAX-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INSURANC-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INSURANC-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000362000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000785000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000611000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000264000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006750000000787500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006800000000312500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009300000000868500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009400000000889500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014400000000426500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019400000000229500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019900000000707000'.
       01  WS-INSURANC-GRID-TBL REDEFINES WS-INSURANC-GRID-VALUES.
           05  WS-INSURANC-GRID-ROW OCCURS 11 TIMES.
               10  WS-INSURANC-GRID-LOW    PIC 9(09)V99.
               10  WS-INSURANC-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISBURSE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISBURSE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000667000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000559500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001100000000854000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000309500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008600000000133500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008650000000596000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008750000000411000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008800000000362500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009800000000709000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010300000000191000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015300000000720000'.
       01  WS-DISBURSE-BAND-TBL REDEFINES WS-DISBURSE-BAND-VALUES.
           05  WS-DISBURSE-BAND-ROW OCCURS 11 TIMES.
               10  WS-DISBURSE-BAND-LOW    PIC 9(09)V99.
               10  WS-DISBURSE-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ESCACCR  MORTGAGE ESCROW'.
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
       COPY ESFEEP.

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
               MOVE 'ESCMST' TO ABND-FILE-NAME
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
           MOVE ESCT-KEY TO ESCM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ACCUM-PAYEE-TYPE
               PERFORM 3010-SET-PAYEE-TYPE
               PERFORM 3020-SPLIT-SHORTAGE
               PERFORM 3030-AGE-NEXT-REVIEW
               PERFORM 3040-ACCRUE-SURPLUS
               PERFORM 3050-LIMIT-SURPLUS
               PERFORM 3060-LIMIT-SURPLUS
               PERFORM 3070-CHECK-EXPIRY
               PERFORM 3080-AGE-CLOSE
               PERFORM 3090-SPLIT-SHORTAGE
               PERFORM 3100-ACCUM-CHANNEL
               PERFORM 3110-CHECK-EXPIRY
               PERFORM 3120-LIMIT-TAX
               PERFORM 3130-AGE-LAST-ACT
               PERFORM 3140-AGE-NOTICE
               PERFORM 3150-ACCUM-TRAN
               PERFORM 3160-EDIT-MEMO
               PERFORM 3170-EDIT-MEMO
               PERFORM 3180-ACCRUE-CUSHION
               PERFORM 3190-SET-ANALYSIS
               PERFORM 3200-COUNT-RETRY
               PERFORM 3210-CHECK-POST
               PERFORM 3220-ACCUM-TYPE
               PERFORM 3230-DESCRIBE-STATUS
               PERFORM 3240-EDIT-OPERATOR
               PERFORM 3250-SPLIT-CUSHION
               PERFORM 3260-ACCUM-REASON
               PERFORM 3270-SPLIT-SURPLUS
               PERFORM 3280-ACCUM-REASON
               PERFORM 3290-PRORATE-CUSHION
               PERFORM 3300-EDIT-CUST-ID
               PERFORM 3310-EDIT-CONTACT
               PERFORM 3320-COUNT-CYCLE
               PERFORM 3330-DESCRIBE-CHANNEL
               PERFORM 3340-FLAG-ITEM
               PERFORM 3350-EDIT-ADDR
               PERFORM 3360-FLAG-ITEM
               PERFORM 3370-COUNT-EXCEPT
               PERFORM 3380-SELECT-FEE-PCT
               PERFORM 3390-FLAG-PAYMENT
               PERFORM 3400-DESCRIBE-TRAN
               PERFORM 3410-PRORATE-TAX
               PERFORM 3420-COUNT-NOTICE
               PERFORM 3430-SET-REASON
               PERFORM 3440-ASSESS-NOTICE-SENT
               PERFORM 3450-PRORATE-INSURANCE
               PERFORM 3460-ASSESS-NOTICE-SENT
               PERFORM 3470-ASSESS-NOTICE-SENT
               PERFORM 3480-CHECK-LAST-PAY
               PERFORM 3490-FLAG-SHORTAGE
               PERFORM 3500-LIMIT-PAYMENT
               PERFORM 3510-SELECT-FEE-PCT
               PERFORM 3520-SELECT-DISCOUNT
               PERFORM 3530-ACCUM-TYPE
               PERFORM 3540-DESCRIBE-ANALYSIS
               PERFORM 3550-SELECT-DISCOUNT
               PERFORM 3560-PRORATE-PAYMENT
               PERFORM 3570-AGE-LAST-STMT
               PERFORM 3580-LIMIT-TAX
               PERFORM 3590-SET-STATUS
               PERFORM 3600-AGE-EXPIRY
               PERFORM 3610-LIMIT-INSURANCE
               PERFORM 3620-COUNT-EXCEPT
               PERFORM 3630-ASSESS-SUSPEND
               PERFORM 3640-EDIT-REJECT
               PERFORM 3650-ACCUM-REASON
               PERFORM 3660-COUNT-NOTICE
               PERFORM 3670-ACCUM-REASON
               PERFORM 3680-DESCRIBE-CHANNEL
               PERFORM 3690-CHECK-NOTICE
               PERFORM 3700-COUNT-EXCEPT
               PERFORM 3710-DESCRIBE-REASON
               PERFORM 3720-FLAG-PAYMENT
               PERFORM 3730-PRORATE-SURPLUS
               PERFORM 3740-LIMIT-INSURANCE
               PERFORM 3750-FLAG-TAX
               PERFORM 3760-LIMIT-CUSHION
               PERFORM 3770-LIMIT-INSURANCE
               PERFORM 3780-FLAG-ITEM
               PERFORM 3790-EDIT-DESC
               PERFORM 3800-SET-TYPE
               PERFORM 3810-COUNT-RETRY
               PERFORM 3820-EDIT-OPERATOR
               PERFORM 3830-AGE-NEXT-DUE
               PERFORM 3840-SET-TRAN
               PERFORM 3850-SET-REGION
               PERFORM 3860-LIMIT-SURPLUS
               PERFORM 3870-COUNT-RETRY
               PERFORM 3880-LIMIT-TAX
               PERFORM 3890-EDIT-ADDR
               PERFORM 3900-SPLIT-TAX
               PERFORM 3910-PRORATE-SURPLUS
               PERFORM 3920-ACCUM-STATUS
               PERFORM 3930-CHECK-POST
               PERFORM 3940-AGE-CLOSE
               PERFORM 3950-EDIT-MEMO
               PERFORM 3960-FLAG-DISBURSE
               PERFORM 3970-EDIT-OPERATOR
               PERFORM 3980-SELECT-DISCOUNT
               PERFORM 3990-CHECK-EXPIRY
               PERFORM 4000-ACCUM-PAYEE-TYPE
               PERFORM 4010-DESCRIBE-STATUS
               PERFORM 4020-LIMIT-DISBURSE
               PERFORM 4030-SET-REGION
               PERFORM 4040-LIMIT-SURPLUS
               PERFORM 4050-ASSESS-HOLD
               PERFORM 4060-PRORATE-INSURANCE
               PERFORM 4070-SPLIT-INSURANCE
               PERFORM 4080-FLAG-ITEM
               PERFORM 4090-ACCUM-ANALYSIS
               PERFORM 4100-ACCRUE-SURPLUS
               PERFORM 4110-SELECT-DISCOUNT
               PERFORM 4120-SPLIT-CUSHION
               PERFORM 4130-COUNT-NOTICE
               PERFORM 4140-PRORATE-TAX
               PERFORM 4150-CHECK-EFFECTIVE
               PERFORM 4160-AGE-CLOSE
               PERFORM 4170-AGE-NOTICE
               PERFORM 4180-PRORATE-DISBURSE
               PERFORM 7000-CALL-ESFEE
               REWRITE ESC-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'ESCMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM ESC-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE AMT BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-PAYEE-TYPE-CD
                   ADD ESCM-INSURANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * AM -> UE WHEN NEXT DUE DATE IS PAST
      * UE -> UP WHEN NEXT DUE DATE IS PAST
      * UP -> AM WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UE' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UE'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UP' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UP'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AM' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SHORTAGE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = ESCT-SHORTAGE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ESCT-SHORTAGE-AMT - (WS-WORK-AMT * 3)
           COMPUTE ESCT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NEXT-REVIEW-DATE > 0 AND ESCM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURPLUS AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3040-ACCRUE-SURPLUS.
           IF ESCM-SURPLUS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SURPLUS-AMT * ESCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3050-LIMIT-SURPLUS.
           IF ESCM-SURPLUS-AMT < 0.00
               MOVE 0.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SURPLUS-AMT > 10000.00
               MOVE 10000.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3060-LIMIT-SURPLUS.
           IF ESCM-SURPLUS-AMT < 0.00
               MOVE 0.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SURPLUS-AMT > 10000.00
               MOVE 10000.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ESCM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ESCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3080-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SHORTAGE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3090-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = ESCM-SHORTAGE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-SHORTAGE-AMT - (WS-WORK-AMT * 3)
           COMPUTE ESCM-SURPLUS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SURPLUS YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCT-CHANNEL-CD
                   ADD ESCM-SURPLUS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-SURPLUS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3110-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ESCM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAX AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3120-LIMIT-TAX.
           IF ESCM-TAX-AMT < 0.00
               MOVE 0.00 TO ESCM-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-TAX-AMT > 500.00
               MOVE 500.00 TO ESCM-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3130-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ESCM-LAST-ACT-DATE > 0 AND ESCM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-SURPLUS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-SURPLUS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-SURPLUS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-SURPLUS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SURPLUS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3150-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCT-TRAN-CD
                   ADD ESCM-DISBURSE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-DISBURSE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E138 MEMO REQUIRED
      * E372 PAYMENT AMT NEGATIVE
      * E150 PAYMENT AMT OVER 99999
      * E615 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3160-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-MEMO = SPACES
                   MOVE 'E138' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-AMT < 0
                   MOVE 'E372' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-AMT > 99999.00
                   MOVE 'E150' TO WS-REJECT-CODE
               WHEN ESCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E615' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E587 MEMO REQUIRED
      * E562 DISBURSE AMT NEGATIVE
      * E765 DISBURSE AMT OVER 1000000
      * E460 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3170-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-MEMO = SPACES
                   MOVE 'E587' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-AMT < 0
                   MOVE 'E562' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-AMT > 1000000.00
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN ESCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E460' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CUSHION YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3180-ACCRUE-CUSHION.
           IF ESCM-CUSHION-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-CUSHION-YTD * ESCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ANALYSIS CD:
      * GD -> LY WHEN CLOSE DATE IS PAST
      * LY -> XJ WHEN CLOSE DATE IS PAST
      * XJ -> GD WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3190-SET-ANALYSIS.
           EVALUATE ESCM-ANALYSIS-CD
               WHEN 'GD'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LY' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LY'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XJ'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GD' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3200-COUNT-RETRY.
           IF ESCM-RETRY-CNT < 9999999
               ADD 1 TO ESCM-RETRY-CNT
           END-IF
           IF ESCM-RETRY-CNT NOT < 12
               MOVE 'Y' TO ESCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF ESCT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SURPLUS YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-TYPE-CD
                   ADD ESCM-SURPLUS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-SURPLUS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3230-DESCRIBE-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'E'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E059 OPERATOR REQUIRED
      * E259 CUSHION AMT NEGATIVE
      * E938 CUSHION AMT OVER 50000
      * E687 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-OPERATOR = SPACES
                   MOVE 'E059' TO WS-REJECT-CODE
               WHEN ESCT-CUSHION-AMT < 0
                   MOVE 'E259' TO WS-REJECT-CODE
               WHEN ESCT-CUSHION-AMT > 50000.00
                   MOVE 'E938' TO WS-REJECT-CODE
               WHEN ESCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E687' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CUSHION YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3250-SPLIT-CUSHION.
           COMPUTE WS-WORK-AMT = ESCM-CUSHION-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ESCM-CUSHION-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3260-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REASON-CD
                   ADD ESCM-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SURPLUS AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3270-SPLIT-SURPLUS.
           COMPUTE WS-WORK-AMT = ESCM-SURPLUS-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ESCM-SURPLUS-AMT - (WS-WORK-AMT * 2)
           COMPUTE ESCT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3280-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REASON-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CUSHION AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3290-PRORATE-CUSHION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ESCT-CUSHION-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E495 CUST ID REQUIRED
      * E285 SHORTAGE YTD NEGATIVE
      * E115 SHORTAGE YTD OVER 50000
      * E330 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3300-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CUST-ID = SPACES
                   MOVE 'E495' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-YTD < 0
                   MOVE 'E285' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-YTD > 50000.00
                   MOVE 'E115' TO WS-REJECT-CODE
               WHEN ESCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E330' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E154 CONTACT REQUIRED
      * E445 SHORTAGE AMT NEGATIVE
      * E487 SHORTAGE AMT OVER 50000
      * E024 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3310-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CONTACT = SPACES
                   MOVE 'E154' TO WS-REJECT-CODE
               WHEN ESCT-SHORTAGE-AMT < 0
                   MOVE 'E445' TO WS-REJECT-CODE
               WHEN ESCT-SHORTAGE-AMT > 50000.00
                   MOVE 'E487' TO WS-REJECT-CODE
               WHEN ESCM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E024' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3320-COUNT-CYCLE.
           IF ESCM-CYCLE-CNT < 9999999
               ADD 1 TO ESCM-CYCLE-CNT
           END-IF
           IF ESCM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3330-DESCRIBE-CHANNEL.
           EVALUATE ESCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3340-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E505 ADDR REQUIRED
      * E239 SURPLUS AMT NEGATIVE
      * E599 SURPLUS AMT OVER 5000
      * E994 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-ADDR = SPACES
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN ESCM-SURPLUS-AMT < 0
                   MOVE 'E239' TO WS-REJECT-CODE
               WHEN ESCM-SURPLUS-AMT > 5000.00
                   MOVE 'E599' TO WS-REJECT-CODE
               WHEN ESCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E994' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3360-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3370-COUNT-EXCEPT.
           IF ESCM-EXCEPT-CNT < 9999999
               ADD 1 TO ESCM-EXCEPT-CNT
           END-IF
           IF ESCM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE BAND TIER FOR THE INSURANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-BAND-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ESCT-INSURANCE-AMT NOT < WS-DISBURSE-BAND-LOW(WS-TX)
                   MOVE WS-DISBURSE-BAND-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3390-FLAG-PAYMENT.
           IF ESCT-PAYMENT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3400-DESCRIBE-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'AN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAX YTD.
      *----------------------------------------------------------------*
       3410-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-TAX-YTD ROUNDED =
                   ESCM-DISBURSE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-TAX-YTD
           END-IF
           ADD ESCM-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3420-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * C -> F WHEN CLOSE DATE IS PAST
      * F -> K WHEN CLOSE DATE IS PAST
      * K -> L WHEN CLOSE DATE IS PAST
      * L -> S WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3430-SET-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE TAX FEE ON THE INSURANCE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3440-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ESCM-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   COMPUTE ESCM-TAX-AMT ROUNDED = ESCM-INSURANCE-YTD *
                       ESCM-RATE / 200
               WHEN ESCM-ANALYSIS-CD = 'GD'
                   MOVE 12.00 TO ESCM-TAX-AMT
               WHEN ESCM-ANALYSIS-CD = 'LY'
                   MOVE 7.00 TO ESCM-TAX-AMT
               WHEN OTHER
                   COMPUTE ESCM-TAX-AMT ROUNDED = ESCM-INSURANCE-YTD *
                       ESCM-RATE / 100
           END-EVALUATE
           IF ESCM-TAX-AMT > 0 AND ESCM-TAX-AMT < 10.00
               MOVE 10.00 TO ESCM-TAX-AMT
           END-IF
           IF ESCM-TAX-AMT > 50.00
               MOVE 50.00 TO ESCM-TAX-AMT
           END-IF
           ADD ESCM-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INSURANCE AMT.
      *----------------------------------------------------------------*
       3450-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCT-INSURANCE-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCT-INSURANCE-AMT
           END-IF
           ADD ESCT-INSURANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE DISBURSE FEE ON THE PAYMENT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3460-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ESCM-DISBURSE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-STOP-FLAG = 'Y'
                   COMPUTE ESCM-DISBURSE-AMT ROUNDED = ESCT-PAYMENT-AMT
                       * ESCM-RATE / 200
               WHEN ESCM-ANALYSIS-CD = 'GD'
                   MOVE 12.00 TO ESCM-DISBURSE-AMT
               WHEN ESCM-ANALYSIS-CD = 'LY'
                   MOVE 12.00 TO ESCM-DISBURSE-AMT
               WHEN OTHER
                   COMPUTE ESCM-DISBURSE-AMT ROUNDED = ESCT-PAYMENT-AMT
                       * ESCM-RATE / 100
           END-EVALUATE
           IF ESCM-DISBURSE-AMT > 0 AND ESCM-DISBURSE-AMT < 1.00
               MOVE 1.00 TO ESCM-DISBURSE-AMT
           END-IF
           IF ESCM-DISBURSE-AMT > 75.00
               MOVE 75.00 TO ESCM-DISBURSE-AMT
           END-IF
           ADD ESCM-DISBURSE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3470-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ESCM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-STOP-FLAG = 'Y'
                   COMPUTE ESCM-PAYMENT-AMT ROUNDED = ESCM-SHORTAGE-YTD
                       * ESCM-RATE / 200
               WHEN ESCM-ANALYSIS-CD = 'GD'
                   MOVE 15.00 TO ESCM-PAYMENT-AMT
               WHEN ESCM-ANALYSIS-CD = 'LY'
                   MOVE 12.00 TO ESCM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE ESCM-PAYMENT-AMT ROUNDED = ESCM-SHORTAGE-YTD
                       * ESCM-RATE / 100
           END-EVALUATE
           IF ESCM-PAYMENT-AMT > 0 AND ESCM-PAYMENT-AMT < 1.00
               MOVE 1.00 TO ESCM-PAYMENT-AMT
           END-IF
           IF ESCM-PAYMENT-AMT > 75.00
               MOVE 75.00 TO ESCM-PAYMENT-AMT
           END-IF
           ADD ESCM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3480-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ESCM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ESCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3490-FLAG-SHORTAGE.
           IF ESCT-SHORTAGE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCT-SHORTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3500-LIMIT-PAYMENT.
           IF ESCT-PAYMENT-AMT < 50.00
               MOVE 50.00 TO ESCT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCT-PAYMENT-AMT > 5000.00
               MOVE 5000.00 TO ESCT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INSURANC BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3510-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INSURANC-BAND-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-INSURANC-BAND-LOW(WS-TX)
                   MOVE WS-INSURANC-BAND-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INSURANC BAND TIER FOR THE CUSHION AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3520-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INSURANC-BAND-RATE(1) TO ESCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ESCM-CUSHION-AMT NOT < WS-INSURANC-BAND-LOW(WS-TX)
                   MOVE WS-INSURANC-BAND-RATE(WS-TX) TO ESCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3530-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-TYPE-CD
                   ADD ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ANALYSIS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-ANALYSIS.
           EVALUATE ESCM-ANALYSIS-CD
               WHEN 'GD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE TAX TIER TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-TIER-RATE(1) TO ESCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF ESCM-PAYMENT-AMT NOT < WS-TAX-TIER-LOW(WS-TX)
                   MOVE WS-TAX-TIER-RATE(WS-TX) TO ESCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       3560-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-PAYMENT-YTD ROUNDED =
                   ESCT-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-PAYMENT-YTD
           END-IF
           ADD ESCM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3570-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-CUSHION-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-CUSHION-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-CUSHION-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-CUSHION-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-CUSHION-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TAX YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3580-LIMIT-TAX.
           IF ESCM-TAX-YTD < 100.00
               MOVE 100.00 TO ESCM-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-TAX-YTD > 1000.00
               MOVE 1000.00 TO ESCM-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * A -> E WHEN NEXT DUE DATE IS PAST
      * E -> R WHEN NEXT DUE DATE IS PAST
      * R -> U WHEN NEXT DUE DATE IS PAST
      * U -> Z WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3590-SET-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3600-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ESCM-EXPIRY-DATE > 0 AND ESCM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INSURANCE AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3610-LIMIT-INSURANCE.
           IF ESCT-INSURANCE-AMT < 100.00
               MOVE 100.00 TO ESCT-INSURANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCT-INSURANCE-AMT > 10000.00
               MOVE 10000.00 TO ESCT-INSURANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-EXCEPT.
           IF ESCM-EXCEPT-CNT < 9999999
               ADD 1 TO ESCM-EXCEPT-CNT
           END-IF
           IF ESCM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO ESCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE SURPLUS YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3630-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ESCM-SURPLUS-YTD *
                       ESCM-FEE-PCT / 200
               WHEN ESCM-STATUS-CD = 'A'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN ESCM-STATUS-CD = 'E'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ESCM-SURPLUS-YTD *
                       ESCM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 35.00
               MOVE 35.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E126 REJECT CD REQUIRED
      * E531 WORK AMT 2 NEGATIVE
      * E947 WORK AMT 2 OVER 50000
      * E248 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-REJECT-CD = SPACES
                   MOVE 'E126' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E531' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E947' TO WS-REJECT-CODE
               WHEN ESCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E248' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3650-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REASON-CD
                   ADD ESCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3670-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3680-DESCRIBE-CHANNEL.
           EVALUATE ESCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3690-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ESCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-EXCEPT.
           IF ESCM-EXCEPT-CNT < 9999999
               ADD 1 TO ESCM-EXCEPT-CNT
           END-IF
           IF ESCM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3710-DESCRIBE-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'F'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3720-FLAG-PAYMENT.
           IF ESCM-PAYMENT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SURPLUS YTD.
      *----------------------------------------------------------------*
       3730-PRORATE-SURPLUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SURPLUS-YTD ROUNDED =
                   ESCM-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-SURPLUS-YTD
           END-IF
           ADD ESCM-SURPLUS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * INSURANCE AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3740-LIMIT-INSURANCE.
           IF ESCM-INSURANCE-AMT < 50.00
               MOVE 50.00 TO ESCM-INSURANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-INSURANCE-AMT > 50000.00
               MOVE 50000.00 TO ESCM-INSURANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAX AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3750-FLAG-TAX.
           IF ESCM-TAX-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CUSHION AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3760-LIMIT-CUSHION.
           IF ESCM-CUSHION-AMT < 25.00
               MOVE 25.00 TO ESCM-CUSHION-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-CUSHION-AMT > 500.00
               MOVE 500.00 TO ESCM-CUSHION-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3770-LIMIT-INSURANCE.
           IF ESCM-INSURANCE-YTD < 100.00
               MOVE 100.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-INSURANCE-YTD > 10000.00
               MOVE 10000.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3780-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E633 DESC REQUIRED
      * E515 INSURANCE YTD NEGATIVE
      * E323 INSURANCE YTD OVER 10000
      * E195 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-DESC = SPACES
                   MOVE 'E633' TO WS-REJECT-CODE
               WHEN ESCM-INSURANCE-YTD < 0
                   MOVE 'E515' TO WS-REJECT-CODE
               WHEN ESCM-INSURANCE-YTD > 10000.00
                   MOVE 'E323' TO WS-REJECT-CODE
               WHEN ESCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E195' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AKX -> REP WHEN CLOSE DATE IS PAST
      * REP -> SMX WHEN CLOSE DATE IS PAST
      * SMX -> VJL WHEN CLOSE DATE IS PAST
      * VJL -> WKN WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3800-SET-TYPE.
           EVALUATE ESCM-TYPE-CD
               WHEN 'AKX'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'REP' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'REP'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMX' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMX'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJL' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJL'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKN' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3810-COUNT-RETRY.
           IF ESCM-RETRY-CNT < 9999999
               ADD 1 TO ESCM-RETRY-CNT
           END-IF
           IF ESCM-RETRY-CNT NOT < 10
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E528 OPERATOR REQUIRED
      * E117 TAX AMT NEGATIVE
      * E149 TAX AMT OVER 250000
      * E889 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-OPERATOR = SPACES
                   MOVE 'E528' TO WS-REJECT-CODE
               WHEN ESCM-TAX-AMT < 0
                   MOVE 'E117' TO WS-REJECT-CODE
               WHEN ESCM-TAX-AMT > 250000.00
                   MOVE 'E149' TO WS-REJECT-CODE
               WHEN ESCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E889' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3830-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DS -> PY WHEN POST DATE IS PAST
      * PY -> AN WHEN POST DATE IS PAST
      * AN -> RF WHEN POST DATE IS PAST
      * RF -> SH WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AN' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AN'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RF'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * B -> G WHEN LAST PAY DATE IS PAST
      * G -> Y WHEN LAST PAY DATE IS PAST
      * Y -> B WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3850-SET-REGION.
           EVALUATE ESCM-REGION-CD
               WHEN 'B'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ESCM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO ESCM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO ESCM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SURPLUS YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3860-LIMIT-SURPLUS.
           IF ESCM-SURPLUS-YTD < 100.00
               MOVE 100.00 TO ESCM-SURPLUS-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SURPLUS-YTD > 2500.00
               MOVE 2500.00 TO ESCM-SURPLUS-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3870-COUNT-RETRY.
           IF ESCM-RETRY-CNT < 9999999
               ADD 1 TO ESCM-RETRY-CNT
           END-IF
           IF ESCM-RETRY-CNT NOT < 10
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TAX YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3880-LIMIT-TAX.
           IF ESCM-TAX-YTD < 50.00
               MOVE 50.00 TO ESCM-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-TAX-YTD > 500.00
               MOVE 500.00 TO ESCM-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E082 ADDR REQUIRED
      * E781 PAYMENT AMT NEGATIVE
      * E033 PAYMENT AMT OVER 25000
      * E101 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-ADDR = SPACES
                   MOVE 'E082' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-AMT < 0
                   MOVE 'E781' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-AMT > 25000.00
                   MOVE 'E033' TO WS-REJECT-CODE
               WHEN ESCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E101' TO WS-REJECT-CODE
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
       3900-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 12)
           COMPUTE ESCM-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INSURANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SURPLUS AMT.
      *----------------------------------------------------------------*
       3910-PRORATE-SURPLUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SURPLUS-AMT ROUNDED =
                   ESCM-INSURANCE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-SURPLUS-AMT
           END-IF
           ADD ESCM-SURPLUS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE SURPLUS YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3920-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-STATUS-CD
                   ADD ESCM-SURPLUS-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-SURPLUS-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3930-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF ESCT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3940-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E984 MEMO REQUIRED
      * E045 SHORTAGE YTD NEGATIVE
      * E044 SHORTAGE YTD OVER 5000
      * E124 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3950-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-MEMO = SPACES
                   MOVE 'E984' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-YTD < 0
                   MOVE 'E045' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-YTD > 5000.00
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN ESCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E124' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3960-FLAG-DISBURSE.
           IF ESCM-DISBURSE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCM-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E207 OPERATOR REQUIRED
      * E069 TAX AMT NEGATIVE
      * E224 TAX AMT OVER 10000
      * E430 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-OPERATOR = SPACES
                   MOVE 'E207' TO WS-REJECT-CODE
               WHEN ESCM-TAX-AMT < 0
                   MOVE 'E069' TO WS-REJECT-CODE
               WHEN ESCM-TAX-AMT > 10000.00
                   MOVE 'E224' TO WS-REJECT-CODE
               WHEN ESCM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E430' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INSURANC GRID TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3980-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INSURANC-GRID-RATE(1) TO ESCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ESCT-PAYMENT-AMT NOT < WS-INSURANC-GRID-LOW(WS-TX)
                   MOVE WS-INSURANC-GRID-RATE(WS-TX) TO ESCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3990-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ESCM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO ESCM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX YTD BY PAYEE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4000-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-PAYEE-TYPE-CD
                   ADD ESCM-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4010-DESCRIBE-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'E'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'U'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DISBURSE AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4020-LIMIT-DISBURSE.
           IF ESCM-DISBURSE-AMT < 25.00
               MOVE 25.00 TO ESCM-DISBURSE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-DISBURSE-AMT > 10000.00
               MOVE 10000.00 TO ESCM-DISBURSE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * B -> G WHEN LAST PAY DATE IS PAST
      * G -> Y WHEN LAST PAY DATE IS PAST
      * Y -> B WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4030-SET-REGION.
           EVALUATE ESCM-REGION-CD
               WHEN 'B'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ESCM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO ESCM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO ESCM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SURPLUS AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4040-LIMIT-SURPLUS.
           IF ESCM-SURPLUS-AMT < 10.00
               MOVE 10.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SURPLUS-AMT > 5000.00
               MOVE 5000.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAX YTD FEE ON THE INSURANCE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4050-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ESCM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ESCM-TAX-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ESCM-TAX-YTD ROUNDED = ESCM-INSURANCE-AMT *
                       ESCM-DISCOUNT / 200
               WHEN ESCM-PAYEE-TYPE-CD = 'AM'
                   MOVE 20.00 TO ESCM-TAX-YTD
               WHEN ESCM-PAYEE-TYPE-CD = 'UE'
                   MOVE 3.00 TO ESCM-TAX-YTD
               WHEN OTHER
                   COMPUTE ESCM-TAX-YTD ROUNDED = ESCM-INSURANCE-AMT *
                       ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-TAX-YTD > 0 AND ESCM-TAX-YTD < 2.00
               MOVE 2.00 TO ESCM-TAX-YTD
           END-IF
           IF ESCM-TAX-YTD > 25.00
               MOVE 25.00 TO ESCM-TAX-YTD
           END-IF
           ADD ESCM-TAX-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE CUSHION YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INSURANCE YTD.
      *----------------------------------------------------------------*
       4060-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-INSURANCE-YTD ROUNDED =
                   ESCM-CUSHION-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-INSURANCE-YTD
           END-IF
           ADD ESCM-INSURANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4070-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESCM-INSURANCE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-INSURANCE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE ESCM-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4080-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX YTD BY ANALYSIS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-ANALYSIS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-ANALYSIS-CD
                   ADD ESCM-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-ANALYSIS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURPLUS YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4100-ACCRUE-SURPLUS.
           IF ESCM-SURPLUS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SURPLUS-YTD * ESCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ESCT-CUSHION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE BAND TIER FOR THE INSURANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4110-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-BAND-RATE(1) TO ESCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ESCM-INSURANCE-AMT NOT < WS-DISBURSE-BAND-LOW(WS-TX)
                   MOVE WS-DISBURSE-BAND-RATE(WS-TX) TO ESCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CUSHION AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4120-SPLIT-CUSHION.
           COMPUTE WS-WORK-AMT = ESCT-CUSHION-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ESCT-CUSHION-AMT - (WS-WORK-AMT * 2)
           COMPUTE ESCM-CUSHION-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4130-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO ESCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CUSHION AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAX YTD.
      *----------------------------------------------------------------*
       4140-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-TAX-YTD ROUNDED =
                   ESCT-CUSHION-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-TAX-YTD
           END-IF
           ADD ESCM-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4150-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ESCM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4160-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SURPLUS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE YTD.
      *----------------------------------------------------------------*
       4180-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-DISBURSE-YTD ROUNDED =
                   ESCM-SURPLUS-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-DISBURSE-YTD
           END-IF
           ADD ESCM-DISBURSE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PASS THE ESC FIELDS TO ESFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-ESFEE.
           INITIALIZE ESFEE-PARMS
           MOVE ESCM-INSURANCE-AMT
             TO ESFEE-INSURANCE-AMT
           MOVE ESCM-TAX-AMT
             TO ESFEE-TAX-AMT
           MOVE ESCM-DISBURSE-AMT
             TO ESFEE-DISBURSE-AMT
           MOVE ESCM-SURPLUS-YTD
             TO ESFEE-SURPLUS-YTD
           MOVE ESCM-CUSHION-YTD
             TO ESFEE-CUSHION-YTD
           MOVE ESCM-TAX-YTD
             TO ESFEE-TAX-YTD
           MOVE ESCM-LAST-STMT-DATE
             TO ESFEE-LAST-STMT-DATE
           MOVE ESCM-CLOSE-DATE
             TO ESFEE-CLOSE-DATE
           MOVE ESCM-EFFECTIVE-DATE
             TO ESFEE-EFFECTIVE-DATE
           MOVE ESCM-NEXT-REVIEW-DATE
             TO ESFEE-NEXT-REVIEW-DATE
           MOVE ESCM-REGION-CD
             TO ESFEE-REGION-CD
           MOVE ESCM-ANALYSIS-CD
             TO ESFEE-ANALYSIS-CD
           MOVE ESCM-REASON-CD
             TO ESFEE-REASON-CD
           MOVE ESCM-TYPE-CD
             TO ESFEE-TYPE-CD
           MOVE ESCM-PREFERRED-FLAG
             TO ESFEE-PREFERRED-FLAG
           MOVE ESCM-OVERRIDE-FLAG
             TO ESFEE-OVERRIDE-FLAG
           MOVE ESCM-SUSPEND-FLAG
             TO ESFEE-SUSPEND-FLAG
           MOVE ESCM-NOTICE-CNT
             TO ESFEE-NOTICE-CNT
           MOVE ESCM-EXCEPT-CNT
             TO ESFEE-EXCEPT-CNT
           MOVE ESCM-FEE-PCT
             TO ESFEE-FEE-PCT
           MOVE ESCM-DISCOUNT
             TO ESFEE-DISCOUNT
           MOVE ESCM-ADDR
             TO ESFEE-ADDR
           MOVE ESCM-CONTACT
             TO ESFEE-CONTACT
           CALL 'ESFEE' USING ESFEE-PARMS
           EVALUATE ESFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ESFEE-TAX-AMT
                     TO ESCM-TAX-AMT
                   MOVE ESFEE-DISBURSE-AMT
                     TO ESCM-DISBURSE-AMT
                   MOVE ESFEE-OVERRIDE-FLAG
                     TO ESCM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-ESFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE ESCT-KEY TO RD-KEY
           MOVE ESCT-TRAN-CD TO RD-CODE
           MOVE ESCT-CUSHION-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE ESCT-KEY TO RD-KEY
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
           DISPLAY 'ESCACCR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ESCACCR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
