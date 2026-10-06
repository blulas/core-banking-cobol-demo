      *================================================================*
      * PROGRAM  : ESCEXTR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MORTGAGE ESCROW - NIGHTLY EXTRACT. READS THE ESC    *
      *            TRANSACTIONS, UPDATES THE ESC MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. ESCROW ANALYSIS, TAX AND        *
      *            INSURANCE DISBURSEMENT AND SHORTAGE                 *
      * INPUT    : ESCMST (KSDS), ESCTRN, BANKCTL                      *
      * OUTPUT   : ESCOUT, ESCEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ESVAL                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-09-18 DPS  ORIGINAL                                       *
      * 1996-03-08 EWS  SHORTAGE LIMIT REVISED REQ8550                 *
      * 2004-03-11 LMB  TAX CALCULATION CORRECTED PRB3833              *
      * 2005-05-04 MKL  NEW SURPLUS REPORT COLUMNS REQ6251             *
      * 2006-09-04 MKL  SURPLUS CALCULATION CORRECTED INC3777          *
      * 2006-02-16 GAF  TAX TABLE EXPANDED INC2437                     *
      * 2008-07-13 LMB  PAYMENT CALCULATION CORRECTED CHG4275          *
      * 2010-01-09 BHS  DISBURSE FEE SCHEDULE UPDATE INC4228           *
      * 2011-08-11 JLM  CUSHION CUTOFF MOVED INC6687                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESCEXTR.
       AUTHOR. D P SHAW.

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
           SELECT REPORT-FILE ASSIGN TO "ESCEXRPT"
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
                                           VALUE 16.000000.
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
      * WS-SURPLUS-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SURPLUS-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000435000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000036000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000035000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000370000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000471000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023250000000704500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028250000000434000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028350000000444500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029350000000253000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029850000000663500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00029900000000833500'.
       01  WS-SURPLUS-GRID-TBL REDEFINES WS-SURPLUS-GRID-VALUES.
           05  WS-SURPLUS-GRID-ROW OCCURS 11 TIMES.
               10  WS-SURPLUS-GRID-LOW     PIC 9(09)V99.
               10  WS-SURPLUS-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-SURPLUS-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SURPLUS-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000766000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000462500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000449000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000148500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011500000000445000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012500000000217000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017500000000806000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022500000000823000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025000000000490000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025050000000392000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025150000000620500'.
       01  WS-SURPLUS-TIER-TBL REDEFINES WS-SURPLUS-TIER-VALUES.
           05  WS-SURPLUS-TIER-ROW OCCURS 11 TIMES.
               10  WS-SURPLUS-TIER-LOW     PIC 9(09)V99.
               10  WS-SURPLUS-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INSURANC-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INSURANC-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000384500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000452000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002750000000568000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002800000000389000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002850000000108000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002950000000239000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012950000000021000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022950000000152000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023200000000464500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023250000000790000'.
       01  WS-INSURANC-GRID-TBL REDEFINES WS-INSURANC-GRID-VALUES.
           05  WS-INSURANC-GRID-ROW OCCURS 10 TIMES.
               10  WS-INSURANC-GRID-LOW    PIC 9(09)V99.
               10  WS-INSURANC-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ESCEXTR  MORTGAGE ESCROW'.
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
       COPY ESVALP.

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
               PERFORM 3000-COUNT-NOTICE
               PERFORM 3010-SET-REASON
               PERFORM 3020-PRORATE-INSURANCE
               PERFORM 3030-FLAG-SHORTAGE
               PERFORM 3040-SPLIT-TAX
               PERFORM 3050-CHECK-NOTICE
               PERFORM 3060-DESCRIBE-REGION
               PERFORM 3070-SET-PAYEE-TYPE
               PERFORM 3080-ACCRUE-CUSHION
               PERFORM 3090-ASSESS-NOTICE-SENT
               PERFORM 3100-ACCUM-ANALYSIS
               PERFORM 3110-EDIT-CONTACT
               PERFORM 3120-EDIT-CONTACT
               PERFORM 3130-LIMIT-SURPLUS
               PERFORM 3140-ASSESS-SUSPEND
               PERFORM 3150-COUNT-RETRY
               PERFORM 3160-DESCRIBE-PAYEE-TYPE
               PERFORM 3170-SPLIT-SURPLUS
               PERFORM 3180-DESCRIBE-PAYEE-TYPE
               PERFORM 3190-CHECK-EXPIRY
               PERFORM 3200-SELECT-FEE-PCT
               PERFORM 3210-LIMIT-PAYMENT
               PERFORM 3220-SELECT-FEE-PCT
               PERFORM 3230-ACCRUE-DISBURSE
               PERFORM 3240-AGE-NEXT-REVIEW
               PERFORM 3250-AGE-CLOSE
               PERFORM 3260-AGE-LAST-ACT
               PERFORM 3270-FLAG-PAYMENT
               PERFORM 3280-SELECT-FEE-PCT
               PERFORM 3290-FLAG-DISBURSE
               PERFORM 3300-ACCRUE-INSURANCE
               PERFORM 3310-EDIT-DESC
               PERFORM 3320-ACCRUE-CUSHION
               PERFORM 3330-SET-REASON
               PERFORM 3340-FLAG-ITEM
               PERFORM 3350-SELECT-DISCOUNT
               PERFORM 3360-LIMIT-INSURANCE
               PERFORM 3370-ACCRUE-TAX
               PERFORM 3380-LIMIT-INSURANCE
               PERFORM 3390-SET-PAYEE-TYPE
               PERFORM 3400-ACCUM-TYPE
               PERFORM 3410-EDIT-CUST-ID
               PERFORM 3420-ASSESS-STOP
               PERFORM 3430-SPLIT-SHORTAGE
               PERFORM 3440-ASSESS-NOTICE-SENT
               PERFORM 3450-PRORATE-SHORTAGE
               PERFORM 3460-PRORATE-CUSHION
               PERFORM 3470-LIMIT-PAYMENT
               PERFORM 3480-SELECT-FEE-PCT
               PERFORM 3490-COUNT-EXCEPT
               PERFORM 3500-LIMIT-SHORTAGE
               PERFORM 3510-LIMIT-INSURANCE
               PERFORM 3520-FLAG-SHORTAGE
               PERFORM 3530-COUNT-SEQ
               PERFORM 3540-DESCRIBE-STATUS
               PERFORM 3550-ASSESS-PREFERRED
               PERFORM 3560-ASSESS-OVERRIDE
               PERFORM 3570-PRORATE-INSURANCE
               PERFORM 3580-ACCRUE-PAYMENT
               PERFORM 3590-SPLIT-CUSHION
               PERFORM 3600-ASSESS-SUSPEND
               PERFORM 3610-FLAG-SURPLUS
               PERFORM 3620-SELECT-RATE
               PERFORM 3630-LIMIT-PAYMENT
               PERFORM 3640-EDIT-CUST-ID
               PERFORM 3650-EDIT-DESC
               PERFORM 3660-DESCRIBE-ANALYSIS
               PERFORM 3670-SPLIT-SHORTAGE
               PERFORM 3680-EDIT-NAME
               PERFORM 3690-ASSESS-STOP
               PERFORM 3700-AGE-EXPIRY
               PERFORM 3710-ACCRUE-TAX
               PERFORM 3720-COUNT-SEQ
               PERFORM 3730-EDIT-CONTACT
               PERFORM 3740-LIMIT-SHORTAGE
               PERFORM 3750-LIMIT-SHORTAGE
               PERFORM 3760-COUNT-CYCLE
               PERFORM 3770-DESCRIBE-REASON
               PERFORM 3780-SELECT-DISCOUNT
               PERFORM 3790-DESCRIBE-TRAN
               PERFORM 3800-AGE-LAST-STMT
               PERFORM 3810-ACCRUE-PAYMENT
               PERFORM 3820-EDIT-NAME
               PERFORM 3830-DESCRIBE-ANALYSIS
               PERFORM 3840-ACCUM-STATUS
               PERFORM 3850-ASSESS-SUSPEND
               PERFORM 3860-SET-CHANNEL
               PERFORM 3870-LIMIT-SURPLUS
               PERFORM 3880-CHECK-LAST-STMT
               PERFORM 3890-ACCRUE-DISCOUNT
               PERFORM 3900-SET-ANALYSIS
               PERFORM 3910-EDIT-REJECT
               PERFORM 3920-SPLIT-TAX
               PERFORM 3930-PRORATE-PAYMENT
               PERFORM 3940-SET-STATUS
               PERFORM 3950-ASSESS-HOLD
               PERFORM 3960-SPLIT-PAYMENT
               PERFORM 3970-FLAG-ITEM
               PERFORM 3980-ACCRUE-SHORTAGE
               PERFORM 3990-PRORATE-PAYMENT
               PERFORM 4000-FLAG-DISBURSE
               PERFORM 4010-ASSESS-STOP
               PERFORM 4020-LIMIT-PAYMENT
               PERFORM 4030-DESCRIBE-TRAN
               PERFORM 4040-AGE-EFFECTIVE
               PERFORM 4050-ACCUM-ANALYSIS
               PERFORM 4060-AGE-LAST-PAY
               PERFORM 4070-DESCRIBE-STATUS
               PERFORM 4080-EDIT-CUST-ID
               PERFORM 4090-CHECK-LAST-PAY
               PERFORM 4100-PRORATE-DISBURSE
               PERFORM 4110-AGE-LAST-PAY
               PERFORM 4120-DESCRIBE-REGION
               PERFORM 4130-EDIT-REJECT
               PERFORM 4140-ASSESS-OVERRIDE
               PERFORM 4150-PRORATE-SURPLUS
               PERFORM 4160-PRORATE-CUSHION
               PERFORM 4170-SPLIT-CUSHION
               PERFORM 4180-SPLIT-INSURANCE
               PERFORM 4190-PRORATE-SHORTAGE
               PERFORM 4200-SET-TRAN
               PERFORM 4210-LIMIT-INSURANCE
               PERFORM 4220-PRORATE-SURPLUS
               PERFORM 4230-AGE-NEXT-DUE
               PERFORM 4240-CHECK-LAST-ACT
               PERFORM 4250-DESCRIBE-TRAN
               PERFORM 4260-SELECT-RATE
               PERFORM 4270-EDIT-CONTACT
               PERFORM 4280-ASSESS-NOTICE-SENT
               PERFORM 4290-ACCUM-REASON
               PERFORM 4300-COUNT-NOTICE
               PERFORM 4310-FLAG-INSURANCE
               PERFORM 4320-SELECT-RATE
               PERFORM 4330-SELECT-RATE
               PERFORM 4340-ACCUM-STATUS
               PERFORM 4350-CHECK-NOTICE
               PERFORM 4360-ACCRUE-PAYMENT
               PERFORM 4370-AGE-LAST-STMT
               PERFORM 4380-ACCRUE-SHORTAGE
               PERFORM 4390-PRORATE-INSURANCE
               PERFORM 4400-ACCRUE-SURPLUS
               PERFORM 4410-ACCUM-STATUS
               PERFORM 4420-LIMIT-PAYMENT
               PERFORM 4430-COUNT-RETRY
               PERFORM 7020-CALL-ESVAL
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
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3000-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO ESCT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * C -> F WHEN EXPIRY DATE IS PAST
      * F -> K WHEN EXPIRY DATE IS PAST
      * K -> L WHEN EXPIRY DATE IS PAST
      * L -> S WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3010-SET-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   IF ESCM-EXPIRY-DATE > 0 AND ESCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF ESCM-EXPIRY-DATE > 0 AND ESCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF ESCM-EXPIRY-DATE > 0 AND ESCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF ESCM-EXPIRY-DATE > 0 AND ESCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INSURANCE AMT.
      *----------------------------------------------------------------*
       3020-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-INSURANCE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-INSURANCE-AMT
           END-IF
           ADD ESCM-INSURANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SHORTAGE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3030-FLAG-SHORTAGE.
           IF ESCT-SHORTAGE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCT-SHORTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAX AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = ESCM-TAX-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ESCM-TAX-AMT - (WS-WORK-AMT * 2)
           COMPUTE ESCM-DISBURSE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3050-CHECK-NOTICE.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3060-DESCRIBE-REGION.
           EVALUATE ESCM-REGION-CD
               WHEN 'B'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'G'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * AM -> UE WHEN NOTICE DATE IS PAST
      * UE -> UP WHEN NOTICE DATE IS PAST
      * UP -> AM WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UE' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UE'
                   IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UP' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UP'
                   IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AM' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CUSHION YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-CUSHION.
           IF ESCM-CUSHION-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-CUSHION-YTD * ESCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ESCM-CUSHION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3090-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SHORTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-HOLD-FLAG = 'Y'
                   COMPUTE ESCM-SHORTAGE-YTD ROUNDED = WS-WORK-AMT *
                       ESCM-DISCOUNT / 200
               WHEN ESCM-REGION-CD = 'B'
                   MOVE 7.00 TO ESCM-SHORTAGE-YTD
               WHEN ESCM-REGION-CD = 'G'
                   MOVE 12.00 TO ESCM-SHORTAGE-YTD
               WHEN OTHER
                   COMPUTE ESCM-SHORTAGE-YTD ROUNDED = WS-WORK-AMT *
                       ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-SHORTAGE-YTD > 0 AND ESCM-SHORTAGE-YTD < 5.00
               MOVE 5.00 TO ESCM-SHORTAGE-YTD
           END-IF
           IF ESCM-SHORTAGE-YTD > 35.00
               MOVE 35.00 TO ESCM-SHORTAGE-YTD
           END-IF
           ADD ESCM-SHORTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY ANALYSIS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3100-ACCUM-ANALYSIS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-ANALYSIS-CD
                   ADD ESCM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-ANALYSIS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E745 CONTACT REQUIRED
      * E198 WORK AMT 2 NEGATIVE
      * E964 WORK AMT 2 OVER 10000
      * E492 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3110-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CONTACT = SPACES
                   MOVE 'E745' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E198' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 10000.00
                   MOVE 'E964' TO WS-REJECT-CODE
               WHEN ESCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E492' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E885 CONTACT REQUIRED
      * E355 PAYMENT YTD NEGATIVE
      * E086 PAYMENT YTD OVER 99999
      * E457 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3120-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CONTACT = SPACES
                   MOVE 'E885' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-YTD < 0
                   MOVE 'E355' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-YTD > 99999.00
                   MOVE 'E086' TO WS-REJECT-CODE
               WHEN ESCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E457' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3130-LIMIT-SURPLUS.
           IF ESCM-SURPLUS-AMT < 25.00
               MOVE 25.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SURPLUS-AMT > 2500.00
               MOVE 2500.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAX FEE ON THE CUSHION AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3140-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ESCM-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCT-REVERSAL-FLAG = 'Y'
                   COMPUTE ESCM-TAX-AMT ROUNDED = ESCM-CUSHION-AMT *
                       ESCM-RATE / 200
               WHEN ESCM-PAYEE-TYPE-CD = 'AM'
                   MOVE 10.00 TO ESCM-TAX-AMT
               WHEN ESCM-PAYEE-TYPE-CD = 'UE'
                   MOVE 3.00 TO ESCM-TAX-AMT
               WHEN OTHER
                   COMPUTE ESCM-TAX-AMT ROUNDED = ESCM-CUSHION-AMT *
                       ESCM-RATE / 100
           END-EVALUATE
           IF ESCM-TAX-AMT > 0 AND ESCM-TAX-AMT < 2.00
               MOVE 2.00 TO ESCM-TAX-AMT
           END-IF
           IF ESCM-TAX-AMT > 250.00
               MOVE 250.00 TO ESCM-TAX-AMT
           END-IF
           ADD ESCM-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3150-COUNT-RETRY.
           IF ESCM-RETRY-CNT < 9999999
               ADD 1 TO ESCM-RETRY-CNT
           END-IF
           IF ESCM-RETRY-CNT NOT < 3
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3160-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SURPLUS YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-SURPLUS.
           COMPUTE WS-WORK-AMT = ESCM-SURPLUS-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-SURPLUS-YTD - (WS-WORK-AMT * 3)
           COMPUTE ESCM-DISBURSE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-EXPIRY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO ESCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURPLUS GRID TIER FOR THE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3200-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SURPLUS-GRID-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ESCM-TAX-YTD NOT < WS-SURPLUS-GRID-LOW(WS-TX)
                   MOVE WS-SURPLUS-GRID-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3210-LIMIT-PAYMENT.
           IF ESCT-PAYMENT-AMT < 100.00
               MOVE 100.00 TO ESCT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCT-PAYMENT-AMT > 500.00
               MOVE 500.00 TO ESCT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURPLUS GRID TIER FOR THE SHORTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3220-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SURPLUS-GRID-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ESCT-SHORTAGE-AMT NOT < WS-SURPLUS-GRID-LOW(WS-TX)
                   MOVE WS-SURPLUS-GRID-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-DISBURSE.
           IF ESCM-DISBURSE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-DISBURSE-AMT * ESCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3240-AGE-NEXT-REVIEW.
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
                   ADD ESCM-SHORTAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-SHORTAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-SHORTAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-SHORTAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SHORTAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3250-AGE-CLOSE.
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
                   ADD ESCM-INSURANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-INSURANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-INSURANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-INSURANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-INSURANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-LAST-ACT.
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
                   ADD ESCT-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCT-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ESCT-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ESCT-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCT-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3270-FLAG-PAYMENT.
           IF ESCM-PAYMENT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURPLUS GRID TIER FOR THE INSURANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SURPLUS-GRID-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ESCT-INSURANCE-AMT NOT < WS-SURPLUS-GRID-LOW(WS-TX)
                   MOVE WS-SURPLUS-GRID-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3290-FLAG-DISBURSE.
           IF ESCM-DISBURSE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-DISBURSE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INSURANCE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-INSURANCE.
           IF ESCM-INSURANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-INSURANCE-YTD * ESCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ESCT-SHORTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E806 DESC REQUIRED
      * E440 DISBURSE AMT NEGATIVE
      * E086 DISBURSE AMT OVER 5000
      * E175 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3310-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-DESC = SPACES
                   MOVE 'E806' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-AMT < 0
                   MOVE 'E440' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-AMT > 5000.00
                   MOVE 'E086' TO WS-REJECT-CODE
               WHEN ESCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E175' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CUSHION AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3320-ACCRUE-CUSHION.
           IF ESCM-CUSHION-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-CUSHION-AMT * ESCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ESCM-SURPLUS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * C -> F WHEN NEXT DUE DATE IS PAST
      * F -> K WHEN NEXT DUE DATE IS PAST
      * K -> L WHEN NEXT DUE DATE IS PAST
      * L -> S WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3330-SET-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3340-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INSURANC GRID TIER FOR THE CUSHION YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INSURANC-GRID-RATE(1) TO ESCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ESCM-CUSHION-YTD NOT < WS-INSURANC-GRID-LOW(WS-TX)
                   MOVE WS-INSURANC-GRID-RATE(WS-TX) TO ESCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3360-LIMIT-INSURANCE.
           IF ESCT-INSURANCE-AMT < 25.00
               MOVE 25.00 TO ESCT-INSURANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCT-INSURANCE-AMT > 5000.00
               MOVE 5000.00 TO ESCT-INSURANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-TAX.
           IF ESCM-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-TAX-YTD * ESCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-DISBURSE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3380-LIMIT-INSURANCE.
           IF ESCM-INSURANCE-YTD < 50.00
               MOVE 50.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-INSURANCE-YTD > 2500.00
               MOVE 2500.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * AM -> UE WHEN LAST STMT DATE IS PAST
      * UE -> UP WHEN LAST STMT DATE IS PAST
      * UP -> AM WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3390-SET-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UE' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UE'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UP' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UP'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AM' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3400-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-TYPE-CD
                   ADD ESCM-INSURANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E802 CUST ID REQUIRED
      * E943 PAYMENT AMT NEGATIVE
      * E508 PAYMENT AMT OVER 50000
      * E408 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CUST-ID = SPACES
                   MOVE 'E802' TO WS-REJECT-CODE
               WHEN ESCT-PAYMENT-AMT < 0
                   MOVE 'E943' TO WS-REJECT-CODE
               WHEN ESCT-PAYMENT-AMT > 50000.00
                   MOVE 'E508' TO WS-REJECT-CODE
               WHEN ESCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E408' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CUSHION YTD FEE ON THE SURPLUS YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3420-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ESCM-CUSHION-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ESCM-CUSHION-YTD ROUNDED = ESCM-SURPLUS-YTD *
                       ESCM-RATE / 200
               WHEN ESCM-REGION-CD = 'B'
                   MOVE 3.00 TO ESCM-CUSHION-YTD
               WHEN ESCM-REGION-CD = 'G'
                   MOVE 20.00 TO ESCM-CUSHION-YTD
               WHEN OTHER
                   COMPUTE ESCM-CUSHION-YTD ROUNDED = ESCM-SURPLUS-YTD *
                       ESCM-RATE / 100
           END-EVALUATE
           IF ESCM-CUSHION-YTD > 0 AND ESCM-CUSHION-YTD < 2.00
               MOVE 2.00 TO ESCM-CUSHION-YTD
           END-IF
           IF ESCM-CUSHION-YTD > 50.00
               MOVE 50.00 TO ESCM-CUSHION-YTD
           END-IF
           ADD ESCM-CUSHION-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT SHORTAGE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = ESCM-SHORTAGE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ESCM-SHORTAGE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE ESCM-CUSHION-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURPLUS FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3440-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SURPLUS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   COMPUTE ESCM-SURPLUS-AMT ROUNDED = ESCM-SHORTAGE-YTD
                       * ESCM-DISCOUNT / 200
               WHEN ESCM-PAYEE-TYPE-CD = 'AM'
                   MOVE 10.00 TO ESCM-SURPLUS-AMT
               WHEN ESCM-PAYEE-TYPE-CD = 'UE'
                   MOVE 15.00 TO ESCM-SURPLUS-AMT
               WHEN OTHER
                   COMPUTE ESCM-SURPLUS-AMT ROUNDED = ESCM-SHORTAGE-YTD
                       * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-SURPLUS-AMT > 0 AND ESCM-SURPLUS-AMT < 10.00
               MOVE 10.00 TO ESCM-SURPLUS-AMT
           END-IF
           IF ESCM-SURPLUS-AMT > 75.00
               MOVE 75.00 TO ESCM-SURPLUS-AMT
           END-IF
           ADD ESCM-SURPLUS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHORTAGE YTD.
      *----------------------------------------------------------------*
       3450-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SHORTAGE-YTD ROUNDED =
                   ESCM-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-SHORTAGE-YTD
           END-IF
           ADD ESCM-SHORTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE SURPLUS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CUSHION YTD.
      *----------------------------------------------------------------*
       3460-PRORATE-CUSHION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-CUSHION-YTD ROUNDED =
                   ESCM-SURPLUS-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-CUSHION-YTD
           END-IF
           ADD ESCM-CUSHION-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3470-LIMIT-PAYMENT.
           IF ESCM-PAYMENT-AMT < 50.00
               MOVE 50.00 TO ESCM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-PAYMENT-AMT > 500.00
               MOVE 500.00 TO ESCM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE INSURANC GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3480-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-INSURANC-GRID-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-INSURANC-GRID-LOW(WS-TX)
                   MOVE WS-INSURANC-GRID-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3490-COUNT-EXCEPT.
           IF ESCM-EXCEPT-CNT < 9999999
               ADD 1 TO ESCM-EXCEPT-CNT
           END-IF
           IF ESCM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO ESCM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3500-LIMIT-SHORTAGE.
           IF ESCM-SHORTAGE-AMT < 10.00
               MOVE 10.00 TO ESCM-SHORTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SHORTAGE-AMT > 2500.00
               MOVE 2500.00 TO ESCM-SHORTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3510-LIMIT-INSURANCE.
           IF ESCT-INSURANCE-AMT < 25.00
               MOVE 25.00 TO ESCT-INSURANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCT-INSURANCE-AMT > 1000.00
               MOVE 1000.00 TO ESCT-INSURANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3520-FLAG-SHORTAGE.
           IF ESCT-SHORTAGE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCT-SHORTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3530-COUNT-SEQ.
           IF ESCT-SEQ < 9999999
               ADD 1 TO ESCT-SEQ
           END-IF
           IF ESCT-SEQ NOT < 5
               MOVE 'Y' TO ESCM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3540-DESCRIBE-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'E'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE TAX FEE ON THE DISBURSE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3550-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ESCM-TAX-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCT-REVERSAL-FLAG = 'Y'
                   COMPUTE ESCM-TAX-AMT ROUNDED = ESCM-DISBURSE-AMT *
                       ESCM-DISCOUNT / 200
               WHEN ESCT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO ESCM-TAX-AMT
               WHEN ESCT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO ESCM-TAX-AMT
               WHEN OTHER
                   COMPUTE ESCM-TAX-AMT ROUNDED = ESCM-DISBURSE-AMT *
                       ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-TAX-AMT > 0 AND ESCM-TAX-AMT < 1.00
               MOVE 1.00 TO ESCM-TAX-AMT
           END-IF
           IF ESCM-TAX-AMT > 150.00
               MOVE 150.00 TO ESCM-TAX-AMT
           END-IF
           ADD ESCM-TAX-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CUSHION FEE ON THE SHORTAGE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3560-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ESCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ESCM-CUSHION-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   COMPUTE ESCM-CUSHION-AMT ROUNDED = ESCT-SHORTAGE-AMT
                       * ESCM-RATE / 200
               WHEN ESCM-TYPE-CD = 'AKX'
                   MOVE 20.00 TO ESCM-CUSHION-AMT
               WHEN ESCM-TYPE-CD = 'REP'
                   MOVE 5.00 TO ESCM-CUSHION-AMT
               WHEN OTHER
                   COMPUTE ESCM-CUSHION-AMT ROUNDED = ESCT-SHORTAGE-AMT
                       * ESCM-RATE / 100
           END-EVALUATE
           IF ESCM-CUSHION-AMT > 0 AND ESCM-CUSHION-AMT < 10.00
               MOVE 10.00 TO ESCM-CUSHION-AMT
           END-IF
           IF ESCM-CUSHION-AMT > 35.00
               MOVE 35.00 TO ESCM-CUSHION-AMT
           END-IF
           ADD ESCM-CUSHION-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE SURPLUS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INSURANCE AMT.
      *----------------------------------------------------------------*
       3570-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-INSURANCE-AMT ROUNDED =
                   ESCM-SURPLUS-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-INSURANCE-AMT
           END-IF
           ADD ESCM-INSURANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3580-ACCRUE-PAYMENT.
           IF ESCM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-PAYMENT-AMT * ESCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ESCT-INSURANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CUSHION YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-CUSHION.
           COMPUTE WS-WORK-AMT = ESCM-CUSHION-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ESCM-CUSHION-YTD - (WS-WORK-AMT * 4)
           COMPUTE ESCM-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURPLUS YTD FEE ON THE WORK AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3600-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SURPLUS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ESCM-SURPLUS-YTD ROUNDED = WS-WORK-AMT *
                       ESCM-FEE-PCT / 200
               WHEN ESCM-REASON-CD = 'C'
                   MOVE 3.00 TO ESCM-SURPLUS-YTD
               WHEN ESCM-REASON-CD = 'F'
                   MOVE 5.00 TO ESCM-SURPLUS-YTD
               WHEN OTHER
                   COMPUTE ESCM-SURPLUS-YTD ROUNDED = WS-WORK-AMT *
                       ESCM-FEE-PCT / 100
           END-EVALUATE
           IF ESCM-SURPLUS-YTD > 0 AND ESCM-SURPLUS-YTD < 5.00
               MOVE 5.00 TO ESCM-SURPLUS-YTD
           END-IF
           IF ESCM-SURPLUS-YTD > 50.00
               MOVE 50.00 TO ESCM-SURPLUS-YTD
           END-IF
           ADD ESCM-SURPLUS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SURPLUS YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3610-FLAG-SURPLUS.
           IF ESCM-SURPLUS-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCM-SURPLUS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURPLUS GRID TIER FOR THE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURPLUS-GRID-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ESCM-TAX-AMT NOT < WS-SURPLUS-GRID-LOW(WS-TX)
                   MOVE WS-SURPLUS-GRID-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3630-LIMIT-PAYMENT.
           IF ESCM-PAYMENT-AMT < 50.00
               MOVE 50.00 TO ESCM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-PAYMENT-AMT > 50000.00
               MOVE 50000.00 TO ESCM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E729 CUST ID REQUIRED
      * E595 INSURANCE AMT NEGATIVE
      * E139 INSURANCE AMT OVER 25000
      * E500 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CUST-ID = SPACES
                   MOVE 'E729' TO WS-REJECT-CODE
               WHEN ESCM-INSURANCE-AMT < 0
                   MOVE 'E595' TO WS-REJECT-CODE
               WHEN ESCM-INSURANCE-AMT > 25000.00
                   MOVE 'E139' TO WS-REJECT-CODE
               WHEN ESCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E500' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E715 DESC REQUIRED
      * E677 TAX YTD NEGATIVE
      * E852 TAX YTD OVER 50000
      * E152 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-DESC = SPACES
                   MOVE 'E715' TO WS-REJECT-CODE
               WHEN ESCM-TAX-YTD < 0
                   MOVE 'E677' TO WS-REJECT-CODE
               WHEN ESCM-TAX-YTD > 50000.00
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN ESCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E152' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ANALYSIS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3660-DESCRIBE-ANALYSIS.
           EVALUATE ESCM-ANALYSIS-CD
               WHEN 'GD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SHORTAGE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3670-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = ESCM-SHORTAGE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ESCM-SHORTAGE-AMT - (WS-WORK-AMT * 4)
           COMPUTE ESCT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E949 NAME REQUIRED
      * E294 SHORTAGE AMT NEGATIVE
      * E592 SHORTAGE AMT OVER 25000
      * E704 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-NAME = SPACES
                   MOVE 'E949' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-AMT < 0
                   MOVE 'E294' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-AMT > 25000.00
                   MOVE 'E592' TO WS-REJECT-CODE
               WHEN ESCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E704' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE FEE ON THE CUSHION AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3690-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SHORTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   COMPUTE ESCM-SHORTAGE-AMT ROUNDED = ESCT-CUSHION-AMT
                       * ESCM-RATE / 200
               WHEN ESCM-REASON-CD = 'C'
                   MOVE 7.00 TO ESCM-SHORTAGE-AMT
               WHEN ESCM-REASON-CD = 'F'
                   MOVE 5.00 TO ESCM-SHORTAGE-AMT
               WHEN OTHER
                   COMPUTE ESCM-SHORTAGE-AMT ROUNDED = ESCT-CUSHION-AMT
                       * ESCM-RATE / 100
           END-EVALUATE
           IF ESCM-SHORTAGE-AMT > 0 AND ESCM-SHORTAGE-AMT < 5.00
               MOVE 5.00 TO ESCM-SHORTAGE-AMT
           END-IF
           IF ESCM-SHORTAGE-AMT > 150.00
               MOVE 150.00 TO ESCM-SHORTAGE-AMT
           END-IF
           ADD ESCM-SHORTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-EXPIRY.
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
                   ADD ESCM-SHORTAGE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-SHORTAGE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-SHORTAGE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-SHORTAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SHORTAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-TAX.
           IF ESCM-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-TAX-AMT * ESCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3720-COUNT-SEQ.
           IF ESCT-SEQ < 9999999
               ADD 1 TO ESCT-SEQ
           END-IF
           IF ESCT-SEQ NOT < 6
               MOVE 'Y' TO ESCM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E431 CONTACT REQUIRED
      * E790 CUSHION YTD NEGATIVE
      * E102 CUSHION YTD OVER 99999
      * E822 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3730-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CONTACT = SPACES
                   MOVE 'E431' TO WS-REJECT-CODE
               WHEN ESCM-CUSHION-YTD < 0
                   MOVE 'E790' TO WS-REJECT-CODE
               WHEN ESCM-CUSHION-YTD > 99999.00
                   MOVE 'E102' TO WS-REJECT-CODE
               WHEN ESCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E822' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3740-LIMIT-SHORTAGE.
           IF ESCM-SHORTAGE-AMT < 10.00
               MOVE 10.00 TO ESCM-SHORTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SHORTAGE-AMT > 2500.00
               MOVE 2500.00 TO ESCM-SHORTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3750-LIMIT-SHORTAGE.
           IF ESCM-SHORTAGE-AMT < 0.00
               MOVE 0.00 TO ESCM-SHORTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SHORTAGE-AMT > 1000.00
               MOVE 1000.00 TO ESCM-SHORTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3760-COUNT-CYCLE.
           IF ESCM-CYCLE-CNT < 9999999
               ADD 1 TO ESCM-CYCLE-CNT
           END-IF
           IF ESCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO ESCM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'F'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SURPLUS GRID TIER FOR THE DISBURSE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3780-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SURPLUS-GRID-RATE(1) TO ESCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ESCM-DISBURSE-AMT NOT < WS-SURPLUS-GRID-LOW(WS-TX)
                   MOVE WS-SURPLUS-GRID-RATE(WS-TX) TO ESCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3790-DESCRIBE-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'AN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3800-AGE-LAST-STMT.
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
                   ADD ESCM-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-PAYMENT.
           IF ESCM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-PAYMENT-YTD * ESCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ESCM-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E464 NAME REQUIRED
      * E962 SHORTAGE AMT NEGATIVE
      * E482 SHORTAGE AMT OVER 25000
      * E326 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-NAME = SPACES
                   MOVE 'E464' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-AMT < 0
                   MOVE 'E962' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-AMT > 25000.00
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN ESCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E326' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ANALYSIS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3830-DESCRIBE-ANALYSIS.
           EVALUATE ESCM-ANALYSIS-CD
               WHEN 'GD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'LY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3840-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-STATUS-CD
                   ADD ESCM-DISBURSE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-DISBURSE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INSURANCE FEE ON THE SHORTAGE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3850-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ESCT-INSURANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ESCT-INSURANCE-AMT ROUNDED =
                       ESCM-SHORTAGE-AMT * ESCM-DISCOUNT / 200
               WHEN ESCM-REASON-CD = 'C'
                   MOVE 12.00 TO ESCT-INSURANCE-AMT
               WHEN ESCM-REASON-CD = 'F'
                   MOVE 3.00 TO ESCT-INSURANCE-AMT
               WHEN OTHER
                   COMPUTE ESCT-INSURANCE-AMT ROUNDED =
                       ESCM-SHORTAGE-AMT * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCT-INSURANCE-AMT > 0 AND ESCT-INSURANCE-AMT < 5.00
               MOVE 5.00 TO ESCT-INSURANCE-AMT
           END-IF
           IF ESCT-INSURANCE-AMT > 35.00
               MOVE 35.00 TO ESCT-INSURANCE-AMT
           END-IF
           ADD ESCT-INSURANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3860-SET-CHANNEL.
           EVALUATE ESCT-CHANNEL-CD
               WHEN 'BRN'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SURPLUS AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3870-LIMIT-SURPLUS.
           IF ESCM-SURPLUS-AMT < 0.00
               MOVE 0.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SURPLUS-AMT > 50000.00
               MOVE 50000.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3880-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ESCM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO ESCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3890-ACCRUE-DISCOUNT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ESCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-INSURANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ANALYSIS CD:
      * GD -> LY WHEN LAST PAY DATE IS PAST
      * LY -> XJ WHEN LAST PAY DATE IS PAST
      * XJ -> GD WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3900-SET-ANALYSIS.
           EVALUATE ESCM-ANALYSIS-CD
               WHEN 'GD'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LY' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LY'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XJ'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GD' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E989 REJECT CD REQUIRED
      * E795 SURPLUS YTD NEGATIVE
      * E715 SURPLUS YTD OVER 250000
      * E041 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-REJECT-CD = SPACES
                   MOVE 'E989' TO WS-REJECT-CODE
               WHEN ESCM-SURPLUS-YTD < 0
                   MOVE 'E795' TO WS-REJECT-CODE
               WHEN ESCM-SURPLUS-YTD > 250000.00
                   MOVE 'E715' TO WS-REJECT-CODE
               WHEN ESCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E041' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAX AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3920-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = ESCM-TAX-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ESCM-TAX-AMT - (WS-WORK-AMT * 4)
           COMPUTE ESCM-INSURANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3930-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCT-PAYMENT-AMT ROUNDED =
                   ESCM-SHORTAGE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCT-PAYMENT-AMT
           END-IF
           ADD ESCT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * A -> E WHEN NEXT REVIEW DATE IS PAST
      * E -> R WHEN NEXT REVIEW DATE IS PAST
      * R -> U WHEN NEXT REVIEW DATE IS PAST
      * U -> Z WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'E' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'R' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'U' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'Z' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE INSURANCE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3950-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ESCM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ESCM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   COMPUTE ESCM-PAYMENT-AMT ROUNDED = ESCT-INSURANCE-AMT
                       * ESCM-RATE / 200
               WHEN ESCM-TYPE-CD = 'AKX'
                   MOVE 15.00 TO ESCM-PAYMENT-AMT
               WHEN ESCM-TYPE-CD = 'REP'
                   MOVE 12.00 TO ESCM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE ESCM-PAYMENT-AMT ROUNDED = ESCT-INSURANCE-AMT
                       * ESCM-RATE / 100
           END-EVALUATE
           IF ESCM-PAYMENT-AMT > 0 AND ESCM-PAYMENT-AMT < 5.00
               MOVE 5.00 TO ESCM-PAYMENT-AMT
           END-IF
           IF ESCM-PAYMENT-AMT > 75.00
               MOVE 75.00 TO ESCM-PAYMENT-AMT
           END-IF
           ADD ESCM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3960-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = ESCT-PAYMENT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ESCT-PAYMENT-AMT - (WS-WORK-AMT * 4)
           COMPUTE ESCM-DISBURSE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3970-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3980-ACCRUE-SHORTAGE.
           IF ESCM-SHORTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SHORTAGE-AMT * ESCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ESCT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INSURANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3990-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCT-PAYMENT-AMT ROUNDED =
                   ESCT-INSURANCE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCT-PAYMENT-AMT
           END-IF
           ADD ESCT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DISBURSE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4000-FLAG-DISBURSE.
           IF ESCM-DISBURSE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-DISBURSE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TAX YTD FEE ON THE WORK AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4010-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ESCM-TAX-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   COMPUTE ESCM-TAX-YTD ROUNDED = WS-WORK-AMT *
                       ESCM-RATE / 200
               WHEN ESCM-REASON-CD = 'C'
                   MOVE 12.00 TO ESCM-TAX-YTD
               WHEN ESCM-REASON-CD = 'F'
                   MOVE 12.00 TO ESCM-TAX-YTD
               WHEN OTHER
                   COMPUTE ESCM-TAX-YTD ROUNDED = WS-WORK-AMT *
                       ESCM-RATE / 100
           END-EVALUATE
           IF ESCM-TAX-YTD > 0 AND ESCM-TAX-YTD < 2.00
               MOVE 2.00 TO ESCM-TAX-YTD
           END-IF
           IF ESCM-TAX-YTD > 25.00
               MOVE 25.00 TO ESCM-TAX-YTD
           END-IF
           ADD ESCM-TAX-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4020-LIMIT-PAYMENT.
           IF ESCT-PAYMENT-AMT < 0.00
               MOVE 0.00 TO ESCT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCT-PAYMENT-AMT > 500.00
               MOVE 500.00 TO ESCT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4030-DESCRIBE-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'AN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4040-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-EFFECTIVE-DATE > 0 AND ESCM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-CUSHION-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-CUSHION-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-CUSHION-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-CUSHION-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-CUSHION-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY ANALYSIS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4050-ACCUM-ANALYSIS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-ANALYSIS-CD
                   ADD ESCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-ANALYSIS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'E'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E155 CUST ID REQUIRED
      * E218 SHORTAGE AMT NEGATIVE
      * E431 SHORTAGE AMT OVER 99999
      * E098 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4080-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CUST-ID = SPACES
                   MOVE 'E155' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-AMT < 0
                   MOVE 'E218' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-AMT > 99999.00
                   MOVE 'E431' TO WS-REJECT-CODE
               WHEN ESCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E098' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4090-CHECK-LAST-PAY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE YTD.
      *----------------------------------------------------------------*
       4100-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-DISBURSE-YTD ROUNDED =
                   ESCM-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-DISBURSE-YTD
           END-IF
           ADD ESCM-DISBURSE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4110-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCT-CUSHION-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4120-DESCRIBE-REGION.
           EVALUATE ESCM-REGION-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'G'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E652 REJECT CD REQUIRED
      * E765 PAYMENT YTD NEGATIVE
      * E560 PAYMENT YTD OVER 250000
      * E636 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4130-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-REJECT-CD = SPACES
                   MOVE 'E652' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-YTD < 0
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-YTD > 250000.00
                   MOVE 'E560' TO WS-REJECT-CODE
               WHEN ESCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E636' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE FEE ON THE PAYMENT YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4140-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ESCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ESCT-SHORTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ESCT-SHORTAGE-AMT ROUNDED = ESCM-PAYMENT-YTD
                       * ESCM-RATE / 200
               WHEN ESCM-PAYEE-TYPE-CD = 'AM'
                   MOVE 15.00 TO ESCT-SHORTAGE-AMT
               WHEN ESCM-PAYEE-TYPE-CD = 'UE'
                   MOVE 10.00 TO ESCT-SHORTAGE-AMT
               WHEN OTHER
                   COMPUTE ESCT-SHORTAGE-AMT ROUNDED = ESCM-PAYMENT-YTD
                       * ESCM-RATE / 100
           END-EVALUATE
           IF ESCT-SHORTAGE-AMT > 0 AND ESCT-SHORTAGE-AMT < 2.00
               MOVE 2.00 TO ESCT-SHORTAGE-AMT
           END-IF
           IF ESCT-SHORTAGE-AMT > 25.00
               MOVE 25.00 TO ESCT-SHORTAGE-AMT
           END-IF
           ADD ESCT-SHORTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SURPLUS YTD.
      *----------------------------------------------------------------*
       4150-PRORATE-SURPLUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SURPLUS-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-SURPLUS-YTD
           END-IF
           ADD ESCM-SURPLUS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CUSHION YTD.
      *----------------------------------------------------------------*
       4160-PRORATE-CUSHION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-CUSHION-YTD ROUNDED =
                   ESCM-DISBURSE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-CUSHION-YTD
           END-IF
           ADD ESCM-CUSHION-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CUSHION AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4170-SPLIT-CUSHION.
           COMPUTE WS-WORK-AMT = ESCM-CUSHION-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ESCM-CUSHION-AMT - (WS-WORK-AMT * 4)
           COMPUTE ESCM-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4180-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESCM-INSURANCE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ESCM-INSURANCE-YTD - (WS-WORK-AMT *
               2)
           COMPUTE ESCM-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHORTAGE AMT.
      *----------------------------------------------------------------*
       4190-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCT-SHORTAGE-AMT ROUNDED =
                   ESCM-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCT-SHORTAGE-AMT
           END-IF
           ADD ESCT-SHORTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DS -> PY WHEN LAST PAY DATE IS PAST
      * PY -> AN WHEN LAST PAY DATE IS PAST
      * AN -> RF WHEN LAST PAY DATE IS PAST
      * RF -> SH WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4200-SET-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AN' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AN'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RF'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * INSURANCE YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4210-LIMIT-INSURANCE.
           IF ESCM-INSURANCE-YTD < 100.00
               MOVE 100.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-INSURANCE-YTD > 10000.00
               MOVE 10000.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SURPLUS AMT.
      *----------------------------------------------------------------*
       4220-PRORATE-SURPLUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SURPLUS-AMT ROUNDED =
                   ESCM-SHORTAGE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-SURPLUS-AMT
           END-IF
           ADD ESCM-SURPLUS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4230-AGE-NEXT-DUE.
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
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4240-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ESCM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4250-DESCRIBE-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'AN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SH'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SURPLUS TIER TIER FOR THE TAX AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4260-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURPLUS-TIER-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ESCM-TAX-AMT NOT < WS-SURPLUS-TIER-LOW(WS-TX)
                   MOVE WS-SURPLUS-TIER-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E198 CONTACT REQUIRED
      * E019 INSURANCE AMT NEGATIVE
      * E857 INSURANCE AMT OVER 25000
      * E594 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4270-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CONTACT = SPACES
                   MOVE 'E198' TO WS-REJECT-CODE
               WHEN ESCT-INSURANCE-AMT < 0
                   MOVE 'E019' TO WS-REJECT-CODE
               WHEN ESCT-INSURANCE-AMT > 25000.00
                   MOVE 'E857' TO WS-REJECT-CODE
               WHEN ESCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E594' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CUSHION FEE ON THE CUSHION AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4280-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ESCM-CUSHION-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-HOLD-FLAG = 'Y'
                   COMPUTE ESCM-CUSHION-AMT ROUNDED = ESCT-CUSHION-AMT *
                       ESCM-FEE-PCT / 200
               WHEN ESCM-REGION-CD = 'B'
                   MOVE 3.00 TO ESCM-CUSHION-AMT
               WHEN ESCM-REGION-CD = 'G'
                   MOVE 7.00 TO ESCM-CUSHION-AMT
               WHEN OTHER
                   COMPUTE ESCM-CUSHION-AMT ROUNDED = ESCT-CUSHION-AMT *
                       ESCM-FEE-PCT / 100
           END-EVALUATE
           IF ESCM-CUSHION-AMT > 0 AND ESCM-CUSHION-AMT < 1.00
               MOVE 1.00 TO ESCM-CUSHION-AMT
           END-IF
           IF ESCM-CUSHION-AMT > 75.00
               MOVE 75.00 TO ESCM-CUSHION-AMT
           END-IF
           ADD ESCM-CUSHION-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE SHORTAGE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4290-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REASON-CD
                   ADD ESCM-SHORTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-SHORTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4300-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4310-FLAG-INSURANCE.
           IF ESCT-INSURANCE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCT-INSURANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURPLUS GRID TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4320-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURPLUS-GRID-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ESCM-PAYMENT-YTD NOT < WS-SURPLUS-GRID-LOW(WS-TX)
                   MOVE WS-SURPLUS-GRID-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SURPLUS GRID TIER FOR THE INSURANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4330-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SURPLUS-GRID-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF ESCT-INSURANCE-AMT NOT < WS-SURPLUS-GRID-LOW(WS-TX)
                   MOVE WS-SURPLUS-GRID-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4340-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-STATUS-CD
                   ADD ESCM-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4350-CHECK-NOTICE.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4360-ACCRUE-PAYMENT.
           IF ESCM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-PAYMENT-YTD * ESCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4370-AGE-LAST-STMT.
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
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCT-SHORTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4380-ACCRUE-SHORTAGE.
           IF ESCM-SHORTAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SHORTAGE-YTD * ESCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INSURANCE AMT.
      *----------------------------------------------------------------*
       4390-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCT-INSURANCE-AMT ROUNDED =
                   ESCM-SHORTAGE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCT-INSURANCE-AMT
           END-IF
           ADD ESCT-INSURANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURPLUS AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4400-ACCRUE-SURPLUS.
           IF ESCM-SURPLUS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SURPLUS-AMT * ESCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-CUSHION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4410-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-STATUS-CD
                   ADD ESCM-DISBURSE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-DISBURSE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4420-LIMIT-PAYMENT.
           IF ESCT-PAYMENT-AMT < 100.00
               MOVE 100.00 TO ESCT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCT-PAYMENT-AMT > 500.00
               MOVE 500.00 TO ESCT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4430-COUNT-RETRY.
           IF ESCM-RETRY-CNT < 9999999
               ADD 1 TO ESCM-RETRY-CNT
           END-IF
           IF ESCM-RETRY-CNT NOT < 6
               MOVE 'Y' TO ESCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE ESC FIELDS TO ESVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-ESVAL.
           INITIALIZE ESVAL-PARMS
           MOVE ESCM-CUSHION-YTD
             TO ESVAL-CUSHION-YTD
           MOVE ESCM-SHORTAGE-YTD
             TO ESVAL-SHORTAGE-YTD
           MOVE ESCM-SURPLUS-YTD
             TO ESVAL-SURPLUS-YTD
           MOVE ESCM-INSURANCE-AMT
             TO ESVAL-INSURANCE-AMT
           MOVE ESCM-PAYMENT-YTD
             TO ESVAL-PAYMENT-YTD
           MOVE ESCM-DISBURSE-YTD
             TO ESVAL-DISBURSE-YTD
           MOVE ESCM-NEXT-DUE-DATE
             TO ESVAL-NEXT-DUE-DATE
           MOVE ESCM-CLOSE-DATE
             TO ESVAL-CLOSE-DATE
           MOVE ESCM-EFFECTIVE-DATE
             TO ESVAL-EFFECTIVE-DATE
           MOVE ESCM-NOTICE-DATE
             TO ESVAL-NOTICE-DATE
           MOVE ESCM-REGION-CD
             TO ESVAL-REGION-CD
           MOVE ESCM-ANALYSIS-CD
             TO ESVAL-ANALYSIS-CD
           MOVE ESCM-STATUS-CD
             TO ESVAL-STATUS-CD
           MOVE ESCM-REASON-CD
             TO ESVAL-REASON-CD
           MOVE ESCM-SUSPEND-FLAG
             TO ESVAL-SUSPEND-FLAG
           MOVE ESCM-HOLD-FLAG
             TO ESVAL-HOLD-FLAG
           MOVE ESCM-PREFERRED-FLAG
             TO ESVAL-PREFERRED-FLAG
           MOVE ESCM-CYCLE-CNT
             TO ESVAL-CYCLE-CNT
           MOVE ESCM-RETRY-CNT
             TO ESVAL-RETRY-CNT
           MOVE ESCM-DISCOUNT
             TO ESVAL-DISCOUNT
           MOVE ESCM-RATE
             TO ESVAL-RATE
           MOVE ESCM-CONTACT
             TO ESVAL-CONTACT
           MOVE ESCM-CUST-ID
             TO ESVAL-CUST-ID
           CALL 'ESVAL' USING ESVAL-PARMS
           EVALUATE ESVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ESVAL-SURPLUS-YTD
                     TO ESCM-SURPLUS-YTD
                   MOVE ESVAL-INSURANCE-AMT
                     TO ESCM-INSURANCE-AMT
                   MOVE ESVAL-PAYMENT-YTD
                     TO ESCM-PAYMENT-YTD
                   MOVE ESVAL-DISBURSE-YTD
                     TO ESCM-DISBURSE-YTD
                   MOVE ESVAL-HOLD-FLAG
                     TO ESCM-HOLD-FLAG
                   MOVE ESVAL-PREFERRED-FLAG
                     TO ESCM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-ESVAL' TO ABND-PARAGRAPH
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
           DISPLAY 'ESCEXTR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ESCEXTR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
