      *================================================================*
      * PROGRAM  : EURAUDT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : EURO CONVERSION - AUDIT TRAIL. READS THE EUR        *
      *            TRANSACTIONS, UPDATES THE EUR MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. NATIONAL CURRENCY BALANCE       *
      *            CONVERSION TO EURO AND DUAL DISPLAY                 *
      * INPUT    : EURMST (KSDS), EURTRN, BANKCTL                      *
      * OUTPUT   : EUROUT, EURAURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, EUFEE                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-11-26 LMB  ORIGINAL                                       *
      * 1999-07-05 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-02-14 CRP  AUDIT FINDING - RESIDUAL CONTROL TOTALS INC5413*
      * 2000-07-03 SKN  RESIDUAL FEE SCHEDULE UPDATE INC3162           *
      * 2001-04-14 GAF  NATIONAL ROUNDING - HALF UP CHG3103            *
      * 2002-10-25 DPS  CONVERTED NOTICE TEXT PER LEGAL CHG7458        *
      * 2004-03-22 SKN  EURO CALCULATION CORRECTED REQ9722             *
      * 2005-02-09 TWH  PERFORMANCE - EURO READ AHEAD REQ8856          *
      * 2006-03-27 DPS  RESIDUAL EXCEPTIONS TO OPS QUEUE REQ7308       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EURAUDT.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "EURMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS EURM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "EURTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "EUROUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "EURAURPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY EURMST.
       FD  TRAN-FILE.
       COPY EURTRN.
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
      * WS-FEE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000159000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000252000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005100000000530500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000174000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010350000000650000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011350000000592000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021350000000053500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026350000000210000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031350000000464000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031600000000809000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031700000000749500'.
       01  WS-FEE-GRID-TBL REDEFINES WS-FEE-GRID-VALUES.
           05  WS-FEE-GRID-ROW OCCURS 11 TIMES.
               10  WS-FEE-GRID-LOW         PIC 9(09)V99.
               10  WS-FEE-GRID-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-EURO-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-EURO-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000046500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000730000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000428000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000650000000746000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001150000000630000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001650000000311500'.
       01  WS-EURO-BAND-TBL REDEFINES WS-EURO-BAND-VALUES.
           05  WS-EURO-BAND-ROW OCCURS 6 TIMES.
               10  WS-EURO-BAND-LOW        PIC 9(09)V99.
               10  WS-EURO-BAND-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-EURO-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-EURO-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000591000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000131000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000139500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002650000000459500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003150000000641000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003250000000767500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003300000000863500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003350000000782000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003600000000496000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003850000000648000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004850000000813500'.
       01  WS-EURO-TIER-TBL REDEFINES WS-EURO-TIER-VALUES.
           05  WS-EURO-TIER-ROW OCCURS 11 TIMES.
               10  WS-EURO-TIER-LOW        PIC 9(09)V99.
               10  WS-EURO-TIER-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  EURAUDT  EURO CONVERSION'.
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
       COPY EUFEEP.

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
               MOVE 'EURMST' TO ABND-FILE-NAME
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
           MOVE EURT-KEY TO EURM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-AGE-NEXT-REVIEW
               PERFORM 3010-COUNT-PAYMENT
               PERFORM 3020-PRORATE-FEE
               PERFORM 3030-DESCRIBE-REGION
               PERFORM 3040-COUNT-SEQ
               PERFORM 3050-SPLIT-TRIANGULAR
               PERFORM 3060-SPLIT-CONVERTED
               PERFORM 3070-SPLIT-EURO
               PERFORM 3080-ACCUM-CHANNEL
               PERFORM 3090-CHECK-MATURITY
               PERFORM 3100-FLAG-RESIDUAL
               PERFORM 3110-ASSESS-REVIEW
               PERFORM 3120-PRORATE-ROUNDING
               PERFORM 3130-PRORATE-EURO
               PERFORM 3140-COUNT-SEQ
               PERFORM 3150-AGE-MATURITY
               PERFORM 3160-COUNT-ITEM
               PERFORM 3170-ASSESS-PREFERRED
               PERFORM 3180-ASSESS-EXEMPT
               PERFORM 3190-EDIT-OPERATOR
               PERFORM 3200-ACCUM-CHANNEL
               PERFORM 3210-COUNT-ITEM
               PERFORM 3220-AGE-NEXT-REVIEW
               PERFORM 3230-SET-TYPE
               PERFORM 3240-SELECT-PENALTY-PCT
               PERFORM 3250-PRORATE-NATIONAL
               PERFORM 3260-ASSESS-REVIEW
               PERFORM 3270-DESCRIBE-CONV-METHOD
               PERFORM 3280-SELECT-PENALTY-PCT
               PERFORM 3290-CHECK-NEXT-REVIEW
               PERFORM 3300-PRORATE-FEE
               PERFORM 3310-SELECT-RATE
               PERFORM 3320-ACCRUE-TRIANGULAR
               PERFORM 3330-ACCRUE-ROUNDING
               PERFORM 3340-FLAG-CONVERTED
               PERFORM 3350-FLAG-RESIDUAL
               PERFORM 3360-LIMIT-CONVERTED
               PERFORM 3370-PRORATE-EURO
               PERFORM 3380-PRORATE-CONVERTED
               PERFORM 3390-ASSESS-EXEMPT
               PERFORM 3400-CHECK-MATURITY
               PERFORM 3410-SPLIT-EURO
               PERFORM 3420-SELECT-FEE-PCT
               PERFORM 3430-LIMIT-EURO
               PERFORM 3440-SELECT-FEE-PCT
               PERFORM 3450-CHECK-LAST-STMT
               PERFORM 3460-COUNT-CYCLE
               PERFORM 3470-LIMIT-TRIANGULAR
               PERFORM 3480-FLAG-NATIONAL
               PERFORM 3490-AGE-LAST-PAY
               PERFORM 3500-ASSESS-EXEMPT
               PERFORM 3510-CHECK-POST
               PERFORM 3520-ACCRUE-ROUNDING
               PERFORM 3530-COUNT-PAYMENT
               PERFORM 3540-ASSESS-AUTO-RENEW
               PERFORM 3550-LIMIT-EURO
               PERFORM 3560-ACCRUE-CONVERTED
               PERFORM 3570-ACCRUE-ROUNDING
               PERFORM 3580-LIMIT-FEE
               PERFORM 3590-SPLIT-RESIDUAL
               PERFORM 3600-SPLIT-NATIONAL
               PERFORM 3610-LIMIT-TRIANGULAR
               PERFORM 3620-LIMIT-NATIONAL
               PERFORM 3630-LIMIT-NATIONAL
               PERFORM 3640-PRORATE-RESIDUAL
               PERFORM 3650-SELECT-RATE
               PERFORM 3660-SPLIT-EURO
               PERFORM 3670-PRORATE-EURO
               PERFORM 3680-FLAG-RESIDUAL
               PERFORM 3690-PRORATE-NATIONAL
               PERFORM 3700-COUNT-PAYMENT
               PERFORM 3710-COUNT-CYCLE
               PERFORM 3720-PRORATE-CONVERTED
               PERFORM 3730-DESCRIBE-REGION
               PERFORM 3740-ACCRUE-ROUNDING
               PERFORM 3750-DESCRIBE-STATUS
               PERFORM 3760-SPLIT-ROUNDING
               PERFORM 3770-SPLIT-FEE
               PERFORM 3780-ASSESS-STOP
               PERFORM 3790-FLAG-TRIANGULAR
               PERFORM 3800-SPLIT-FEE
               PERFORM 3810-SPLIT-ROUNDING
               PERFORM 3820-SELECT-PENALTY-PCT
               PERFORM 3830-ACCUM-TRAN
               PERFORM 3840-COUNT-ITEM
               PERFORM 3850-AGE-LAST-PAY
               PERFORM 3860-EDIT-ADDR
               PERFORM 3870-SPLIT-ROUNDING
               PERFORM 3880-SELECT-FEE-PCT
               PERFORM 3890-AGE-EFFECTIVE
               PERFORM 3900-COUNT-ITEM
               PERFORM 3910-LIMIT-TRIANGULAR
               PERFORM 3920-SPLIT-RESIDUAL
               PERFORM 3930-COUNT-PAYMENT
               PERFORM 3940-DESCRIBE-STATUS
               PERFORM 3950-CHECK-LAST-MAINT
               PERFORM 3960-SET-CHANNEL
               PERFORM 3970-ACCUM-TRAN
               PERFORM 3980-SPLIT-FEE
               PERFORM 3990-ASSESS-OVERRIDE
               PERFORM 4000-ASSESS-AUTO-RENEW
               PERFORM 4010-DESCRIBE-TYPE
               PERFORM 4020-SELECT-RATE
               PERFORM 4030-PRORATE-ROUNDING
               PERFORM 4040-ACCRUE-NATIONAL
               PERFORM 4050-PRORATE-TRIANGULAR
               PERFORM 4060-PRORATE-EURO
               PERFORM 4070-COUNT-SEQ
               PERFORM 4080-PRORATE-EURO
               PERFORM 4090-COUNT-CYCLE
               PERFORM 4100-DESCRIBE-TRAN
               PERFORM 4110-CHECK-LAST-PAY
               PERFORM 4120-ACCUM-TRAN
               PERFORM 4130-AGE-MATURITY
               PERFORM 4140-SPLIT-RESIDUAL
               PERFORM 4150-ASSESS-AUTO-RENEW
               PERFORM 4160-AGE-OPEN
               PERFORM 4170-SPLIT-RESIDUAL
               PERFORM 4180-ACCUM-STATUS
               PERFORM 4190-ACCUM-TYPE
               PERFORM 4200-ACCUM-CURRENCY
               PERFORM 4210-COUNT-SEQ
               PERFORM 4220-ACCRUE-TRIANGULAR
               PERFORM 4230-SET-TRAN
               PERFORM 4240-SELECT-PENALTY-PCT
               PERFORM 4250-CHECK-LAST-STMT
               PERFORM 4260-ACCUM-TYPE
               PERFORM 4270-FLAG-NATIONAL
               PERFORM 4280-SELECT-FEE-PCT
               PERFORM 4290-ACCRUE-CONVERTED
               PERFORM 4300-CHECK-EFFECTIVE
               PERFORM 4310-COUNT-PAYMENT
               PERFORM 4320-AGE-MATURITY
               PERFORM 4330-SELECT-FEE-PCT
               PERFORM 4340-DESCRIBE-STATUS
               PERFORM 4350-SELECT-FEE-PCT
               PERFORM 4360-SET-CONV-METHOD
               PERFORM 4370-SET-CHANNEL
               PERFORM 4380-DESCRIBE-TYPE
               PERFORM 4390-SPLIT-FEE
               PERFORM 4400-ASSESS-OVERRIDE
               PERFORM 4410-SPLIT-ROUNDING
               PERFORM 4420-FLAG-ROUNDING
               PERFORM 4430-EDIT-CONTACT
               PERFORM 4440-SPLIT-FEE
               PERFORM 4450-ACCRUE-NATIONAL
               PERFORM 4460-SELECT-FEE-PCT
               PERFORM 4470-SELECT-FEE-PCT
               PERFORM 4480-COUNT-PAYMENT
               PERFORM 4490-DESCRIBE-STATUS
               PERFORM 4500-SET-CHANNEL
               PERFORM 4510-SELECT-RATE
               PERFORM 4520-PRORATE-EURO
               PERFORM 4530-ACCUM-CHANNEL
               PERFORM 4540-AGE-LAST-PAY
               PERFORM 4550-LIMIT-TRIANGULAR
               PERFORM 4560-EDIT-CONTACT
               PERFORM 4570-DESCRIBE-CURRENCY
               PERFORM 4580-ACCUM-REGION
               PERFORM 4590-ACCUM-CONV-METHOD
               PERFORM 4600-ACCRUE-NATIONAL
               PERFORM 4610-AGE-OPEN
               PERFORM 4620-SET-TYPE
               PERFORM 4630-COUNT-SEQ
               PERFORM 4640-SPLIT-FEE
               PERFORM 4650-SPLIT-NATIONAL
               PERFORM 4660-ASSESS-REVIEW
               PERFORM 4670-EDIT-CUST-ID
               PERFORM 4680-SPLIT-EURO
               PERFORM 4690-AGE-NOTICE
               PERFORM 4700-SELECT-RATE
               PERFORM 4710-FLAG-ROUNDING
               PERFORM 7020-CALL-EUFEE
               REWRITE EUR-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'EURMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM EUR-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3000-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF EURM-NEXT-REVIEW-DATE > 0 AND EURM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-PAYMENT.
           IF EURM-PAYMENT-CNT < 9999999
               ADD 1 TO EURM-PAYMENT-CNT
           END-IF
           IF EURM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO EURM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3020-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURT-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3030-DESCRIBE-REGION.
           EVALUATE EURM-REGION-CD
               WHEN 'DTL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HYD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JUL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WSE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YAU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'YJB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-SEQ.
           IF EURT-SEQ < 9999999
               ADD 1 TO EURT-SEQ
           END-IF
           IF EURT-SEQ NOT < 5
               MOVE 'Y' TO EURM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TRIANGULAR AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-TRIANGULAR.
           COMPUTE WS-WORK-AMT = EURM-TRIANGULAR-AMT / 2
           COMPUTE WS-WORK-AMT-2 = EURM-TRIANGULAR-AMT - (WS-WORK-AMT *
               2)
           COMPUTE EURT-NATIONAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CONVERTED AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3060-SPLIT-CONVERTED.
           COMPUTE WS-WORK-AMT = EURM-CONVERTED-AMT / 12
           COMPUTE WS-WORK-AMT-2 = EURM-CONVERTED-AMT - (WS-WORK-AMT *
               12)
           COMPUTE EURM-CONVERTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EURO YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3070-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EURM-EURO-YTD / 4
           COMPUTE WS-WORK-AMT-2 = EURM-EURO-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3080-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURT-CHANNEL-CD
                   ADD EURM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3090-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF EURM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO EURM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESIDUAL YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3100-FLAG-RESIDUAL.
           IF EURM-RESIDUAL-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EURM-RESIDUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE NATIONAL AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3110-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EURM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO EURT-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-EXEMPT-FLAG = 'Y'
                   COMPUTE EURT-FEE-AMT ROUNDED = EURT-NATIONAL-AMT *
                       EURM-PENALTY-PCT / 200
               WHEN EURM-REGION-CD = 'DTL'
                   MOVE 12.00 TO EURT-FEE-AMT
               WHEN EURM-REGION-CD = 'HYD'
                   MOVE 12.00 TO EURT-FEE-AMT
               WHEN OTHER
                   COMPUTE EURT-FEE-AMT ROUNDED = EURT-NATIONAL-AMT *
                       EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF EURT-FEE-AMT > 0 AND EURT-FEE-AMT < 10.00
               MOVE 10.00 TO EURT-FEE-AMT
           END-IF
           IF EURT-FEE-AMT > 50.00
               MOVE 50.00 TO EURT-FEE-AMT
           END-IF
           ADD EURT-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ROUNDING AMT.
      *----------------------------------------------------------------*
       3120-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURT-ROUNDING-AMT ROUNDED =
                   EURM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURT-ROUNDING-AMT
           END-IF
           ADD EURT-ROUNDING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EURO YTD.
      *----------------------------------------------------------------*
       3130-PRORATE-EURO.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-EURO-YTD ROUNDED =
                   EURT-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-EURO-YTD
           END-IF
           ADD EURM-EURO-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3140-COUNT-SEQ.
           IF EURT-SEQ < 9999999
               ADD 1 TO EURT-SEQ
           END-IF
           IF EURT-SEQ NOT < 6
               MOVE 'Y' TO EURM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3150-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-NATIONAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3160-COUNT-ITEM.
           IF EURM-ITEM-CNT < 9999999
               ADD 1 TO EURM-ITEM-CNT
           END-IF
           IF EURM-ITEM-CNT NOT < 12
               MOVE 'Y' TO EURM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ROUNDING FEE ON THE CONVERTED YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3170-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN EURM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO EURT-ROUNDING-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURT-REVERSAL-FLAG = 'Y'
                   COMPUTE EURT-ROUNDING-AMT ROUNDED =
                       EURM-CONVERTED-YTD * EURM-FEE-PCT / 200
               WHEN EURM-CURRENCY-CD = 'EW'
                   MOVE 12.00 TO EURT-ROUNDING-AMT
               WHEN EURM-CURRENCY-CD = 'FJ'
                   MOVE 3.00 TO EURT-ROUNDING-AMT
               WHEN OTHER
                   COMPUTE EURT-ROUNDING-AMT ROUNDED =
                       EURM-CONVERTED-YTD * EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURT-ROUNDING-AMT > 0 AND EURT-ROUNDING-AMT < 10.00
               MOVE 10.00 TO EURT-ROUNDING-AMT
           END-IF
           IF EURT-ROUNDING-AMT > 35.00
               MOVE 35.00 TO EURT-ROUNDING-AMT
           END-IF
           ADD EURT-ROUNDING-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR YTD FEE ON THE TRIANGULAR AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3180-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN EURM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO EURM-TRIANGULAR-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-STOP-FLAG = 'Y'
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED =
                       EURM-TRIANGULAR-AMT * EURM-FEE-PCT / 200
               WHEN EURM-CURRENCY-CD = 'EW'
                   MOVE 15.00 TO EURM-TRIANGULAR-YTD
               WHEN EURM-CURRENCY-CD = 'FJ'
                   MOVE 3.00 TO EURM-TRIANGULAR-YTD
               WHEN OTHER
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED =
                       EURM-TRIANGULAR-AMT * EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURM-TRIANGULAR-YTD > 0 AND EURM-TRIANGULAR-YTD < 2.00
               MOVE 2.00 TO EURM-TRIANGULAR-YTD
           END-IF
           IF EURM-TRIANGULAR-YTD > 35.00
               MOVE 35.00 TO EURM-TRIANGULAR-YTD
           END-IF
           ADD EURM-TRIANGULAR-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E706 OPERATOR REQUIRED
      * E399 EURO YTD NEGATIVE
      * E547 EURO YTD OVER 99999
      * E542 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3190-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURT-OPERATOR = SPACES
                   MOVE 'E706' TO WS-REJECT-CODE
               WHEN EURM-EURO-YTD < 0
                   MOVE 'E399' TO WS-REJECT-CODE
               WHEN EURM-EURO-YTD > 99999.00
                   MOVE 'E547' TO WS-REJECT-CODE
               WHEN EURM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E542' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TRIANGULAR AMT BY CHANNEL CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURT-CHANNEL-CD
                   ADD EURM-TRIANGULAR-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-TRIANGULAR-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-ITEM.
           IF EURM-ITEM-CNT < 9999999
               ADD 1 TO EURM-ITEM-CNT
           END-IF
           IF EURM-ITEM-CNT NOT < 10
               MOVE 'Y' TO EURM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF EURM-NEXT-REVIEW-DATE > 0 AND EURM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGL -> SMF WHEN POST DATE IS PAST
      * SMF -> UEC WHEN POST DATE IS PAST
      * UEC -> VJA WHEN POST DATE IS PAST
      * VJA -> WPH WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-TYPE.
           EVALUATE EURM-TYPE-CD
               WHEN 'AGL'
                   IF EURT-POST-DATE > 0 AND EURT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMF' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMF'
                   IF EURT-POST-DATE > 0 AND EURT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UEC' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UEC'
                   IF EURT-POST-DATE > 0 AND EURT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJA' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJA'
                   IF EURT-POST-DATE > 0 AND EURT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WPH' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EURO TIER TIER FOR THE RESIDUAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-TIER-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EURM-RESIDUAL-AMT NOT < WS-EURO-TIER-LOW(WS-TX)
                   MOVE WS-EURO-TIER-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ROUNDING AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO NATIONAL AMT.
      *----------------------------------------------------------------*
       3250-PRORATE-NATIONAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-NATIONAL-AMT ROUNDED =
                   EURT-ROUNDING-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-NATIONAL-AMT
           END-IF
           ADD EURM-NATIONAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE NATIONAL FEE ON THE FEE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3260-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EURM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-NATIONAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE EURM-NATIONAL-AMT ROUNDED = EURM-FEE-YTD *
                       EURM-RATE / 200
               WHEN EURM-CHANNEL-CD = 'AEA'
                   MOVE 7.00 TO EURM-NATIONAL-AMT
               WHEN EURM-CHANNEL-CD = 'EBM'
                   MOVE 5.00 TO EURM-NATIONAL-AMT
               WHEN OTHER
                   COMPUTE EURM-NATIONAL-AMT ROUNDED = EURM-FEE-YTD *
                       EURM-RATE / 100
           END-EVALUATE
           IF EURM-NATIONAL-AMT > 0 AND EURM-NATIONAL-AMT < 1.00
               MOVE 1.00 TO EURM-NATIONAL-AMT
           END-IF
           IF EURM-NATIONAL-AMT > 35.00
               MOVE 35.00 TO EURM-NATIONAL-AMT
           END-IF
           ADD EURM-NATIONAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CONV METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3270-DESCRIBE-CONV-METHOD.
           EVALUATE EURM-CONV-METHOD-CD
               WHEN 'GL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VC'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE NATIONAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3280-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EURT-NATIONAL-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3290-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF EURM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO EURM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONVERTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3300-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-FEE-AMT ROUNDED =
                   EURT-CONVERTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-FEE-AMT
           END-IF
           ADD EURM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE EURO BAND TIER FOR THE TRIANGULAR AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3310-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-BAND-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF EURM-TRIANGULAR-AMT NOT < WS-EURO-BAND-LOW(WS-TX)
                   MOVE WS-EURO-BAND-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRIANGULAR AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3320-ACCRUE-TRIANGULAR.
           IF EURM-TRIANGULAR-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-TRIANGULAR-AMT * EURM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURM-CONVERTED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROUNDING AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3330-ACCRUE-ROUNDING.
           IF EURM-ROUNDING-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-ROUNDING-AMT * EURM-RATE / 100 / 365
               ADD WS-WORK-AMT TO EURM-RESIDUAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CONVERTED YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3340-FLAG-CONVERTED.
           IF EURM-CONVERTED-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EURM-CONVERTED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RESIDUAL AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3350-FLAG-RESIDUAL.
           IF EURM-RESIDUAL-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-RESIDUAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CONVERTED AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3360-LIMIT-CONVERTED.
           IF EURT-CONVERTED-AMT < 10.00
               MOVE 10.00 TO EURT-CONVERTED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURT-CONVERTED-AMT > 500.00
               MOVE 500.00 TO EURT-CONVERTED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE NATIONAL YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO EURO YTD.
      *----------------------------------------------------------------*
       3370-PRORATE-EURO.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-EURO-YTD ROUNDED =
                   EURM-NATIONAL-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-EURO-YTD
           END-IF
           ADD EURM-EURO-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE NATIONAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONVERTED AMT.
      *----------------------------------------------------------------*
       3380-PRORATE-CONVERTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURT-CONVERTED-AMT ROUNDED =
                   EURM-NATIONAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURT-CONVERTED-AMT
           END-IF
           ADD EURT-CONVERTED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE EURO FEE ON THE CONVERTED YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3390-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN EURM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO EURM-EURO-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-PREFERRED-FLAG = 'Y'
                   COMPUTE EURM-EURO-AMT ROUNDED = EURM-CONVERTED-YTD *
                       EURM-PENALTY-PCT / 200
               WHEN EURT-TRAN-CD = 'CV'
                   MOVE 20.00 TO EURM-EURO-AMT
               WHEN EURT-TRAN-CD = 'RV'
                   MOVE 15.00 TO EURM-EURO-AMT
               WHEN OTHER
                   COMPUTE EURM-EURO-AMT ROUNDED = EURM-CONVERTED-YTD *
                       EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF EURM-EURO-AMT > 0 AND EURM-EURO-AMT < 10.00
               MOVE 10.00 TO EURM-EURO-AMT
           END-IF
           IF EURM-EURO-AMT > 35.00
               MOVE 35.00 TO EURM-EURO-AMT
           END-IF
           ADD EURM-EURO-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3400-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF EURM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO EURM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EURO AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EURM-EURO-AMT / 12
           COMPUTE WS-WORK-AMT-2 = EURM-EURO-AMT - (WS-WORK-AMT * 12)
           COMPUTE EURM-RESIDUAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EURO TIER TIER FOR THE ROUNDING YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3420-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-TIER-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EURM-ROUNDING-YTD NOT < WS-EURO-TIER-LOW(WS-TX)
                   MOVE WS-EURO-TIER-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EURO AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3430-LIMIT-EURO.
           IF EURM-EURO-AMT < 50.00
               MOVE 50.00 TO EURM-EURO-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-EURO-AMT > 1000.00
               MOVE 1000.00 TO EURM-EURO-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EURO BAND TIER FOR THE NATIONAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3440-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-BAND-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF EURM-NATIONAL-AMT NOT < WS-EURO-BAND-LOW(WS-TX)
                   MOVE WS-EURO-BAND-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO EURM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3460-COUNT-CYCLE.
           IF EURM-CYCLE-CNT < 9999999
               ADD 1 TO EURM-CYCLE-CNT
           END-IF
           IF EURM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO EURM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3470-LIMIT-TRIANGULAR.
           IF EURM-TRIANGULAR-AMT < 10.00
               MOVE 10.00 TO EURM-TRIANGULAR-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-TRIANGULAR-AMT > 50000.00
               MOVE 50000.00 TO EURM-TRIANGULAR-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3480-FLAG-NATIONAL.
           IF EURM-NATIONAL-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURM-NATIONAL-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3490-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-PAY-DATE > 0 AND EURM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR YTD FEE ON THE ROUNDING AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3500-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN EURM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO EURM-TRIANGULAR-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-STOP-FLAG = 'Y'
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED =
                       EURM-ROUNDING-AMT * EURM-FEE-PCT / 200
               WHEN EURM-STATUS-CD = 'B'
                   MOVE 15.00 TO EURM-TRIANGULAR-YTD
               WHEN EURM-STATUS-CD = 'F'
                   MOVE 15.00 TO EURM-TRIANGULAR-YTD
               WHEN OTHER
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED =
                       EURM-ROUNDING-AMT * EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURM-TRIANGULAR-YTD > 0 AND EURM-TRIANGULAR-YTD < 10.00
               MOVE 10.00 TO EURM-TRIANGULAR-YTD
           END-IF
           IF EURM-TRIANGULAR-YTD > 250.00
               MOVE 250.00 TO EURM-TRIANGULAR-YTD
           END-IF
           ADD EURM-TRIANGULAR-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3510-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF EURT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO EURM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROUNDING AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3520-ACCRUE-ROUNDING.
           IF EURT-ROUNDING-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURT-ROUNDING-AMT * EURM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURM-NATIONAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3530-COUNT-PAYMENT.
           IF EURM-PAYMENT-CNT < 9999999
               ADD 1 TO EURM-PAYMENT-CNT
           END-IF
           IF EURM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO EURT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESIDUAL FEE ON THE RESIDUAL YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3540-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-RESIDUAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-REVIEW-FLAG = 'Y'
                   COMPUTE EURM-RESIDUAL-AMT ROUNDED = EURM-RESIDUAL-YTD
                       * EURM-FEE-PCT / 200
               WHEN EURM-CURRENCY-CD = 'EW'
                   MOVE 15.00 TO EURM-RESIDUAL-AMT
               WHEN EURM-CURRENCY-CD = 'FJ'
                   MOVE 5.00 TO EURM-RESIDUAL-AMT
               WHEN OTHER
                   COMPUTE EURM-RESIDUAL-AMT ROUNDED = EURM-RESIDUAL-YTD
                       * EURM-FEE-PCT / 100
           END-EVALUATE
           IF EURM-RESIDUAL-AMT > 0 AND EURM-RESIDUAL-AMT < 10.00
               MOVE 10.00 TO EURM-RESIDUAL-AMT
           END-IF
           IF EURM-RESIDUAL-AMT > 150.00
               MOVE 150.00 TO EURM-RESIDUAL-AMT
           END-IF
           ADD EURM-RESIDUAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * EURO AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3550-LIMIT-EURO.
           IF EURM-EURO-AMT < 10.00
               MOVE 10.00 TO EURM-EURO-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-EURO-AMT > 50000.00
               MOVE 50000.00 TO EURM-EURO-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONVERTED YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-CONVERTED.
           IF EURM-CONVERTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-CONVERTED-YTD * EURM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURM-TRIANGULAR-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROUNDING AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3570-ACCRUE-ROUNDING.
           IF EURM-ROUNDING-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-ROUNDING-AMT * EURM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO EURM-NATIONAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3580-LIMIT-FEE.
           IF EURM-FEE-YTD < 10.00
               MOVE 10.00 TO EURM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-FEE-YTD > 2500.00
               MOVE 2500.00 TO EURM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESIDUAL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3590-SPLIT-RESIDUAL.
           COMPUTE WS-WORK-AMT = EURM-RESIDUAL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = EURM-RESIDUAL-AMT - (WS-WORK-AMT * 4)
           COMPUTE EURM-EURO-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NATIONAL YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3600-SPLIT-NATIONAL.
           COMPUTE WS-WORK-AMT = EURM-NATIONAL-YTD / 4
           COMPUTE WS-WORK-AMT-2 = EURM-NATIONAL-YTD - (WS-WORK-AMT * 4)
           COMPUTE EURT-ROUNDING-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3610-LIMIT-TRIANGULAR.
           IF EURM-TRIANGULAR-AMT < 50.00
               MOVE 50.00 TO EURM-TRIANGULAR-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-TRIANGULAR-AMT > 50000.00
               MOVE 50000.00 TO EURM-TRIANGULAR-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3620-LIMIT-NATIONAL.
           IF EURM-NATIONAL-AMT < 100.00
               MOVE 100.00 TO EURM-NATIONAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-NATIONAL-AMT > 2500.00
               MOVE 2500.00 TO EURM-NATIONAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3630-LIMIT-NATIONAL.
           IF EURM-NATIONAL-AMT < 0.00
               MOVE 0.00 TO EURM-NATIONAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-NATIONAL-AMT > 5000.00
               MOVE 5000.00 TO EURM-NATIONAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONVERTED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESIDUAL AMT.
      *----------------------------------------------------------------*
       3640-PRORATE-RESIDUAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-RESIDUAL-AMT ROUNDED =
                   EURM-CONVERTED-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-RESIDUAL-AMT
           END-IF
           ADD EURM-RESIDUAL-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE EURO TIER TIER FOR THE TRIANGULAR YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3650-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-TIER-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EURM-TRIANGULAR-YTD NOT < WS-EURO-TIER-LOW(WS-TX)
                   MOVE WS-EURO-TIER-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EURO AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EURM-EURO-AMT / 3
           COMPUTE WS-WORK-AMT-2 = EURM-EURO-AMT - (WS-WORK-AMT * 3)
           COMPUTE EURM-ROUNDING-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EURO AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-EURO.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-EURO-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RESIDUAL YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3680-FLAG-RESIDUAL.
           IF EURM-RESIDUAL-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EURM-RESIDUAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EURO AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO NATIONAL YTD.
      *----------------------------------------------------------------*
       3690-PRORATE-NATIONAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-NATIONAL-YTD ROUNDED =
                   EURM-EURO-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO EURM-NATIONAL-YTD
           END-IF
           ADD EURM-NATIONAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-PAYMENT.
           IF EURM-PAYMENT-CNT < 9999999
               ADD 1 TO EURM-PAYMENT-CNT
           END-IF
           IF EURM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO EURM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3710-COUNT-CYCLE.
           IF EURM-CYCLE-CNT < 9999999
               ADD 1 TO EURM-CYCLE-CNT
           END-IF
           IF EURM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO EURT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE ROUNDING AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CONVERTED YTD.
      *----------------------------------------------------------------*
       3720-PRORATE-CONVERTED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-CONVERTED-YTD ROUNDED =
                   EURT-ROUNDING-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-CONVERTED-YTD
           END-IF
           ADD EURM-CONVERTED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-REGION.
           EVALUATE EURM-REGION-CD
               WHEN 'DTL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HYD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JUL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WSE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'YAU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YJB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON ROUNDING AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3740-ACCRUE-ROUNDING.
           IF EURM-ROUNDING-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-ROUNDING-AMT * EURM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURT-NATIONAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3750-DESCRIBE-STATUS.
           EVALUATE EURM-STATUS-CD
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'K'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT ROUNDING YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-ROUNDING.
           COMPUTE WS-WORK-AMT = EURM-ROUNDING-YTD / 4
           COMPUTE WS-WORK-AMT-2 = EURM-ROUNDING-YTD - (WS-WORK-AMT * 4)
           COMPUTE EURM-EURO-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3770-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = EURM-FEE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = EURM-FEE-YTD - (WS-WORK-AMT * 3)
           COMPUTE EURT-NATIONAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE ROUNDING YTD FEE ON THE CONVERTED AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3780-ASSESS-STOP.
           EVALUATE TRUE
               WHEN EURM-STOP-FLAG = 'Y'
                   MOVE ZERO TO EURM-ROUNDING-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-OVERRIDE-FLAG = 'Y'
                   COMPUTE EURM-ROUNDING-YTD ROUNDED =
                       EURT-CONVERTED-AMT * EURM-PENALTY-PCT / 200
               WHEN EURM-REGION-CD = 'DTL'
                   MOVE 3.00 TO EURM-ROUNDING-YTD
               WHEN EURM-REGION-CD = 'HYD'
                   MOVE 3.00 TO EURM-ROUNDING-YTD
               WHEN OTHER
                   COMPUTE EURM-ROUNDING-YTD ROUNDED =
                       EURT-CONVERTED-AMT * EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF EURM-ROUNDING-YTD > 0 AND EURM-ROUNDING-YTD < 5.00
               MOVE 5.00 TO EURM-ROUNDING-YTD
           END-IF
           IF EURM-ROUNDING-YTD > 50.00
               MOVE 50.00 TO EURM-ROUNDING-YTD
           END-IF
           ADD EURM-ROUNDING-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRIANGULAR YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3790-FLAG-TRIANGULAR.
           IF EURM-TRIANGULAR-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EURM-TRIANGULAR-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3800-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = EURM-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = EURM-FEE-YTD - (WS-WORK-AMT * 4)
           COMPUTE EURM-ROUNDING-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ROUNDING AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3810-SPLIT-ROUNDING.
           COMPUTE WS-WORK-AMT = EURT-ROUNDING-AMT / 2
           COMPUTE WS-WORK-AMT-2 = EURT-ROUNDING-AMT - (WS-WORK-AMT * 2)
           COMPUTE EURT-CONVERTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EURO BAND TIER FOR THE CONVERTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3820-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-BAND-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF EURT-CONVERTED-AMT NOT < WS-EURO-BAND-LOW(WS-TX)
                   MOVE WS-EURO-BAND-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ROUNDING AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3830-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURT-TRAN-CD
                   ADD EURM-ROUNDING-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-ROUNDING-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3840-COUNT-ITEM.
           IF EURM-ITEM-CNT < 9999999
               ADD 1 TO EURM-ITEM-CNT
           END-IF
           IF EURM-ITEM-CNT NOT < 5
               MOVE 'Y' TO EURM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-PAY-DATE > 0 AND EURM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-NATIONAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E283 ADDR REQUIRED
      * E575 RESIDUAL AMT NEGATIVE
      * E388 RESIDUAL AMT OVER 1000000
      * E924 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3860-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-ADDR = SPACES
                   MOVE 'E283' TO WS-REJECT-CODE
               WHEN EURM-RESIDUAL-AMT < 0
                   MOVE 'E575' TO WS-REJECT-CODE
               WHEN EURM-RESIDUAL-AMT > 1000000.00
                   MOVE 'E388' TO WS-REJECT-CODE
               WHEN EURM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT ROUNDING AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3870-SPLIT-ROUNDING.
           COMPUTE WS-WORK-AMT = EURM-ROUNDING-AMT / 3
           COMPUTE WS-WORK-AMT-2 = EURM-ROUNDING-AMT - (WS-WORK-AMT * 3)
           COMPUTE EURM-CONVERTED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EURO BAND TIER FOR THE ROUNDING AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3880-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-BAND-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF EURM-ROUNDING-AMT NOT < WS-EURO-BAND-LOW(WS-TX)
                   MOVE WS-EURO-BAND-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3890-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3900-COUNT-ITEM.
           IF EURM-ITEM-CNT < 9999999
               ADD 1 TO EURM-ITEM-CNT
           END-IF
           IF EURM-ITEM-CNT NOT < 10
               MOVE 'Y' TO EURM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRIANGULAR YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3910-LIMIT-TRIANGULAR.
           IF EURM-TRIANGULAR-YTD < 50.00
               MOVE 50.00 TO EURM-TRIANGULAR-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-TRIANGULAR-YTD > 2500.00
               MOVE 2500.00 TO EURM-TRIANGULAR-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESIDUAL AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3920-SPLIT-RESIDUAL.
           COMPUTE WS-WORK-AMT = EURM-RESIDUAL-AMT / 3
           COMPUTE WS-WORK-AMT-2 = EURM-RESIDUAL-AMT - (WS-WORK-AMT * 3)
           COMPUTE EURT-ROUNDING-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3930-COUNT-PAYMENT.
           IF EURM-PAYMENT-CNT < 9999999
               ADD 1 TO EURM-PAYMENT-CNT
           END-IF
           IF EURM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO EURM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3940-DESCRIBE-STATUS.
           EVALUATE EURM-STATUS-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'F'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'K'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'U'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST MAINT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3950-CHECK-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-MAINT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO EURM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AEA -> EBM WHEN EFFECTIVE DATE IS PAST
      * EBM -> GAU WHEN EFFECTIVE DATE IS PAST
      * GAU -> GSU WHEN EFFECTIVE DATE IS PAST
      * GSU -> VWP WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-CHANNEL.
           EVALUATE EURM-CHANNEL-CD
               WHEN 'AEA'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EBM' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EBM'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GAU' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GAU'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GSU' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GSU'
                   IF EURM-EFFECTIVE-DATE > 0 AND EURM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VWP' TO EURM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3970-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURT-TRAN-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3980-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = EURM-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = EURM-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE EURT-NATIONAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR YTD FEE ON THE EURO YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3990-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN EURM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO EURM-TRIANGULAR-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-EXEMPT-FLAG = 'Y'
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED = EURM-EURO-YTD *
                       EURM-RATE / 200
               WHEN EURT-TRAN-CD = 'CV'
                   MOVE 20.00 TO EURM-TRIANGULAR-YTD
               WHEN EURT-TRAN-CD = 'RV'
                   MOVE 12.00 TO EURM-TRIANGULAR-YTD
               WHEN OTHER
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED = EURM-EURO-YTD *
                       EURM-RATE / 100
           END-EVALUATE
           IF EURM-TRIANGULAR-YTD > 0 AND EURM-TRIANGULAR-YTD < 2.00
               MOVE 2.00 TO EURM-TRIANGULAR-YTD
           END-IF
           IF EURM-TRIANGULAR-YTD > 25.00
               MOVE 25.00 TO EURM-TRIANGULAR-YTD
           END-IF
           ADD EURM-TRIANGULAR-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE TRIANGULAR YTD FEE ON THE EURO AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4000-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-TRIANGULAR-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-PREFERRED-FLAG = 'Y'
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED = EURM-EURO-AMT *
                       EURM-PENALTY-PCT / 200
               WHEN EURM-REGION-CD = 'DTL'
                   MOVE 10.00 TO EURM-TRIANGULAR-YTD
               WHEN EURM-REGION-CD = 'HYD'
                   MOVE 7.00 TO EURM-TRIANGULAR-YTD
               WHEN OTHER
                   COMPUTE EURM-TRIANGULAR-YTD ROUNDED = EURM-EURO-AMT *
                       EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF EURM-TRIANGULAR-YTD > 0 AND EURM-TRIANGULAR-YTD < 1.00
               MOVE 1.00 TO EURM-TRIANGULAR-YTD
           END-IF
           IF EURM-TRIANGULAR-YTD > 75.00
               MOVE 75.00 TO EURM-TRIANGULAR-YTD
           END-IF
           ADD EURM-TRIANGULAR-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4010-DESCRIBE-TYPE.
           EVALUATE EURM-TYPE-CD
               WHEN 'AGL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'SMF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UEC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VJA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WPH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EURO BAND TIER FOR THE EURO AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4020-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-BAND-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF EURM-EURO-AMT NOT < WS-EURO-BAND-LOW(WS-TX)
                   MOVE WS-EURO-BAND-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CONVERTED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO ROUNDING AMT.
      *----------------------------------------------------------------*
       4030-PRORATE-ROUNDING.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURT-ROUNDING-AMT ROUNDED =
                   EURT-CONVERTED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURT-ROUNDING-AMT
           END-IF
           ADD EURT-ROUNDING-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4040-ACCRUE-NATIONAL.
           IF EURM-NATIONAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-NATIONAL-YTD * EURM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO EURM-RESIDUAL-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EURO YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TRIANGULAR AMT.
      *----------------------------------------------------------------*
       4050-PRORATE-TRIANGULAR.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-TRIANGULAR-AMT ROUNDED =
                   EURM-EURO-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-TRIANGULAR-AMT
           END-IF
           ADD EURM-TRIANGULAR-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE EURO YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4060-PRORATE-EURO.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-EURO-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4070-COUNT-SEQ.
           IF EURT-SEQ < 9999999
               ADD 1 TO EURT-SEQ
           END-IF
           IF EURT-SEQ NOT < 12
               MOVE 'Y' TO EURM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE EURO YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EURO AMT.
      *----------------------------------------------------------------*
       4080-PRORATE-EURO.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-EURO-AMT ROUNDED =
                   EURM-EURO-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-EURO-AMT
           END-IF
           ADD EURM-EURO-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-CYCLE.
           IF EURM-CYCLE-CNT < 9999999
               ADD 1 TO EURM-CYCLE-CNT
           END-IF
           IF EURM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO EURM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4100-DESCRIBE-TRAN.
           EVALUATE EURT-TRAN-CD
               WHEN 'CV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'AD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RD'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4110-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO EURM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4120-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURT-TRAN-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4130-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-MATURITY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RESIDUAL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4140-SPLIT-RESIDUAL.
           COMPUTE WS-WORK-AMT = EURM-RESIDUAL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = EURM-RESIDUAL-AMT - (WS-WORK-AMT * 4)
           COMPUTE EURM-NATIONAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE NATIONAL FEE ON THE TRIANGULAR YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4150-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN EURM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-NATIONAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-PREFERRED-FLAG = 'Y'
                   COMPUTE EURM-NATIONAL-AMT ROUNDED =
                       EURM-TRIANGULAR-YTD * EURM-RATE / 200
               WHEN EURM-CURRENCY-CD = 'EW'
                   MOVE 12.00 TO EURM-NATIONAL-AMT
               WHEN EURM-CURRENCY-CD = 'FJ'
                   MOVE 12.00 TO EURM-NATIONAL-AMT
               WHEN OTHER
                   COMPUTE EURM-NATIONAL-AMT ROUNDED =
                       EURM-TRIANGULAR-YTD * EURM-RATE / 100
           END-EVALUATE
           IF EURM-NATIONAL-AMT > 0 AND EURM-NATIONAL-AMT < 5.00
               MOVE 5.00 TO EURM-NATIONAL-AMT
           END-IF
           IF EURM-NATIONAL-AMT > 250.00
               MOVE 250.00 TO EURM-NATIONAL-AMT
           END-IF
           ADD EURM-NATIONAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4160-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-EURO-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD EURM-EURO-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD EURM-EURO-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-EURO-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-EURO-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RESIDUAL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4170-SPLIT-RESIDUAL.
           COMPUTE WS-WORK-AMT = EURM-RESIDUAL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = EURM-RESIDUAL-YTD - (WS-WORK-AMT * 2)
           COMPUTE EURT-ROUNDING-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CONVERTED AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4180-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-STATUS-CD
                   ADD EURT-CONVERTED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURT-CONVERTED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4190-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-TYPE-CD
                   ADD EURM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ROUNDING AMT BY CURRENCY CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4200-ACCUM-CURRENCY.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-CURRENCY-CD
                   ADD EURT-ROUNDING-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-CURRENCY-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURT-ROUNDING-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4210-COUNT-SEQ.
           IF EURT-SEQ < 9999999
               ADD 1 TO EURT-SEQ
           END-IF
           IF EURT-SEQ NOT < 5
               MOVE 'Y' TO EURM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TRIANGULAR YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4220-ACCRUE-TRIANGULAR.
           IF EURM-TRIANGULAR-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-TRIANGULAR-YTD * EURM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * CV -> RV WHEN EFF DATE IS PAST
      * RV -> AD WHEN EFF DATE IS PAST
      * AD -> RD WHEN EFF DATE IS PAST
      * RD -> FX WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4230-SET-TRAN.
           EVALUATE EURT-TRAN-CD
               WHEN 'CV'
                   IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AD'
                   IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RD' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RD'
                   IF EURT-EFF-DATE > 0 AND EURT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FX' TO EURT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EURO BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4240-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-BAND-RATE(1) TO EURM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF EURT-FEE-AMT NOT < WS-EURO-BAND-LOW(WS-TX)
                   MOVE WS-EURO-BAND-RATE(WS-TX) TO EURM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4250-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO EURM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE ROUNDING AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4260-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-TYPE-CD
                   ADD EURM-ROUNDING-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-ROUNDING-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NATIONAL YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4270-FLAG-NATIONAL.
           IF EURM-NATIONAL-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE EURM-NATIONAL-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EURO TIER TIER FOR THE TRIANGULAR YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4280-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-TIER-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EURM-TRIANGULAR-YTD NOT < WS-EURO-TIER-LOW(WS-TX)
                   MOVE WS-EURO-TIER-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CONVERTED YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4290-ACCRUE-CONVERTED.
           IF EURM-CONVERTED-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-CONVERTED-YTD * EURM-RATE / 100 / 365
               ADD WS-WORK-AMT TO EURT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4300-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF EURM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO EURM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4310-COUNT-PAYMENT.
           IF EURM-PAYMENT-CNT < 9999999
               ADD 1 TO EURM-PAYMENT-CNT
           END-IF
           IF EURM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO EURM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4320-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF EURM-MATURITY-DATE > 0 AND EURM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-TRIANGULAR-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE NATIONAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4330-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EURT-NATIONAL-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4340-DESCRIBE-STATUS.
           EVALUATE EURM-STATUS-CD
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'K'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'R'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'U'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EURO TIER TIER FOR THE CONVERTED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4350-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-TIER-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EURM-CONVERTED-AMT NOT < WS-EURO-TIER-LOW(WS-TX)
                   MOVE WS-EURO-TIER-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CONV METHOD CD:
      * GL -> JA WHEN NOTICE DATE IS PAST
      * JA -> LD WHEN NOTICE DATE IS PAST
      * LD -> MR WHEN NOTICE DATE IS PAST
      * MR -> VC WHEN NOTICE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4360-SET-CONV-METHOD.
           EVALUATE EURM-CONV-METHOD-CD
               WHEN 'GL'
                   IF EURM-NOTICE-DATE > 0 AND EURM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JA' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JA'
                   IF EURM-NOTICE-DATE > 0 AND EURM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LD' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LD'
                   IF EURM-NOTICE-DATE > 0 AND EURM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MR' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MR'
                   IF EURM-NOTICE-DATE > 0 AND EURM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VC' TO EURM-CONV-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4370-SET-CHANNEL.
           EVALUATE EURT-CHANNEL-CD
               WHEN 'BRN'
                   IF EURM-NEXT-REVIEW-DATE > 0 AND
                       EURM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF EURM-NEXT-REVIEW-DATE > 0 AND
                       EURM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF EURM-NEXT-REVIEW-DATE > 0 AND
                       EURM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF EURM-NEXT-REVIEW-DATE > 0 AND
                       EURM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4380-DESCRIBE-TYPE.
           EVALUATE EURM-TYPE-CD
               WHEN 'AGL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'SMF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UEC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VJA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WPH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4390-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = EURM-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = EURM-FEE-AMT - (WS-WORK-AMT * 12)
           COMPUTE EURM-NATIONAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESIDUAL FEE ON THE EURO AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4400-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN EURM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO EURM-RESIDUAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-PREFERRED-FLAG = 'Y'
                   COMPUTE EURM-RESIDUAL-AMT ROUNDED = EURM-EURO-AMT *
                       EURM-RATE / 200
               WHEN EURT-TRAN-CD = 'CV'
                   MOVE 12.00 TO EURM-RESIDUAL-AMT
               WHEN EURT-TRAN-CD = 'RV'
                   MOVE 10.00 TO EURM-RESIDUAL-AMT
               WHEN OTHER
                   COMPUTE EURM-RESIDUAL-AMT ROUNDED = EURM-EURO-AMT *
                       EURM-RATE / 100
           END-EVALUATE
           IF EURM-RESIDUAL-AMT > 0 AND EURM-RESIDUAL-AMT < 1.00
               MOVE 1.00 TO EURM-RESIDUAL-AMT
           END-IF
           IF EURM-RESIDUAL-AMT > 75.00
               MOVE 75.00 TO EURM-RESIDUAL-AMT
           END-IF
           ADD EURM-RESIDUAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT ROUNDING AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4410-SPLIT-ROUNDING.
           COMPUTE WS-WORK-AMT = EURT-ROUNDING-AMT / 2
           COMPUTE WS-WORK-AMT-2 = EURT-ROUNDING-AMT - (WS-WORK-AMT * 2)
           COMPUTE EURM-CONVERTED-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ROUNDING AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4420-FLAG-ROUNDING.
           IF EURM-ROUNDING-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE EURM-ROUNDING-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E943 CONTACT REQUIRED
      * E290 ROUNDING YTD NEGATIVE
      * E145 ROUNDING YTD OVER 99999
      * E443 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4430-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-CONTACT = SPACES
                   MOVE 'E943' TO WS-REJECT-CODE
               WHEN EURM-ROUNDING-YTD < 0
                   MOVE 'E290' TO WS-REJECT-CODE
               WHEN EURM-ROUNDING-YTD > 99999.00
                   MOVE 'E145' TO WS-REJECT-CODE
               WHEN EURM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E443' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4440-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = EURT-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = EURT-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE EURM-NATIONAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4450-ACCRUE-NATIONAL.
           IF EURT-NATIONAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURT-NATIONAL-AMT * EURM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE GRID TIER FOR THE TRIANGULAR AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4460-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-GRID-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EURM-TRIANGULAR-AMT NOT < WS-FEE-GRID-LOW(WS-TX)
                   MOVE WS-FEE-GRID-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EURO TIER TIER FOR THE NATIONAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4470-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-TIER-RATE(1) TO EURM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EURT-NATIONAL-AMT NOT < WS-EURO-TIER-LOW(WS-TX)
                   MOVE WS-EURO-TIER-RATE(WS-TX) TO EURM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4480-COUNT-PAYMENT.
           IF EURM-PAYMENT-CNT < 9999999
               ADD 1 TO EURM-PAYMENT-CNT
           END-IF
           IF EURM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO EURM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4490-DESCRIBE-STATUS.
           EVALUATE EURM-STATUS-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'K'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'X'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Y'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST STMT DATE IS PAST
      * BAT -> MAL WHEN LAST STMT DATE IS PAST
      * MAL -> TEL WHEN LAST STMT DATE IS PAST
      * TEL -> ONL WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4500-SET-CHANNEL.
           EVALUATE EURT-CHANNEL-CD
               WHEN 'BRN'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF EURM-LAST-STMT-DATE > 0 AND EURM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO EURT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EURO TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4510-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-TIER-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT-2 NOT < WS-EURO-TIER-LOW(WS-TX)
                   MOVE WS-EURO-TIER-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE TRIANGULAR AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EURO YTD.
      *----------------------------------------------------------------*
       4520-PRORATE-EURO.
           IF WS-PERIOD-DAYS > 0
               COMPUTE EURM-EURO-YTD ROUNDED =
                   EURM-TRIANGULAR-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO EURM-EURO-YTD
           END-IF
           ADD EURM-EURO-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE ROUNDING AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4530-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-CHANNEL-CD
                   ADD EURM-ROUNDING-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-ROUNDING-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4540-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF EURM-LAST-PAY-DATE > 0 AND EURM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-TRIANGULAR-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-TRIANGULAR-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EURM-TRIANGULAR-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD EURM-TRIANGULAR-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-TRIANGULAR-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRIANGULAR YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4550-LIMIT-TRIANGULAR.
           IF EURM-TRIANGULAR-YTD < 50.00
               MOVE 50.00 TO EURM-TRIANGULAR-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF EURM-TRIANGULAR-YTD > 2500.00
               MOVE 2500.00 TO EURM-TRIANGULAR-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E612 CONTACT REQUIRED
      * E095 NATIONAL YTD NEGATIVE
      * E042 NATIONAL YTD OVER 50000
      * E393 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4560-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-CONTACT = SPACES
                   MOVE 'E612' TO WS-REJECT-CODE
               WHEN EURM-NATIONAL-YTD < 0
                   MOVE 'E095' TO WS-REJECT-CODE
               WHEN EURM-NATIONAL-YTD > 50000.00
                   MOVE 'E042' TO WS-REJECT-CODE
               WHEN EURT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E393' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CURRENCY CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4570-DESCRIBE-CURRENCY.
           EVALUATE EURM-CURRENCY-CD
               WHEN 'EW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JC'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4580-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-REGION-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE NATIONAL YTD BY CONV METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4590-ACCUM-CONV-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = EURM-CONV-METHOD-CD
                   ADD EURM-NATIONAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE EURM-CONV-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE EURM-NATIONAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON NATIONAL YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4600-ACCRUE-NATIONAL.
           IF EURM-NATIONAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   EURM-NATIONAL-YTD * EURM-RATE / 100 / 365
               ADD WS-WORK-AMT TO EURT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4610-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF EURM-OPEN-DATE > 0 AND EURM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AGL -> SMF WHEN POST DATE IS PAST
      * SMF -> UEC WHEN POST DATE IS PAST
      * UEC -> VJA WHEN POST DATE IS PAST
      * VJA -> WPH WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4620-SET-TYPE.
           EVALUATE EURM-TYPE-CD
               WHEN 'AGL'
                   IF EURT-POST-DATE > 0 AND EURT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMF' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMF'
                   IF EURT-POST-DATE > 0 AND EURT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UEC' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UEC'
                   IF EURT-POST-DATE > 0 AND EURT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJA' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJA'
                   IF EURT-POST-DATE > 0 AND EURT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WPH' TO EURM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4630-COUNT-SEQ.
           IF EURT-SEQ < 9999999
               ADD 1 TO EURT-SEQ
           END-IF
           IF EURT-SEQ NOT < 12
               MOVE 'Y' TO EURT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4640-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = EURM-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = EURM-FEE-YTD - (WS-WORK-AMT * 12)
           COMPUTE EURT-NATIONAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT NATIONAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4650-SPLIT-NATIONAL.
           COMPUTE WS-WORK-AMT = EURT-NATIONAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = EURT-NATIONAL-AMT - (WS-WORK-AMT * 2)
           COMPUTE EURM-EURO-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EURO YTD FEE ON THE NATIONAL AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4660-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN EURM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO EURM-EURO-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN EURM-OVERRIDE-FLAG = 'Y'
                   COMPUTE EURM-EURO-YTD ROUNDED = EURT-NATIONAL-AMT *
                       EURM-PENALTY-PCT / 200
               WHEN EURM-REGION-CD = 'DTL'
                   MOVE 20.00 TO EURM-EURO-YTD
               WHEN EURM-REGION-CD = 'HYD'
                   MOVE 5.00 TO EURM-EURO-YTD
               WHEN OTHER
                   COMPUTE EURM-EURO-YTD ROUNDED = EURT-NATIONAL-AMT *
                       EURM-PENALTY-PCT / 100
           END-EVALUATE
           IF EURM-EURO-YTD > 0 AND EURM-EURO-YTD < 1.00
               MOVE 1.00 TO EURM-EURO-YTD
           END-IF
           IF EURM-EURO-YTD > 150.00
               MOVE 150.00 TO EURM-EURO-YTD
           END-IF
           ADD EURM-EURO-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E213 CUST ID REQUIRED
      * E850 TRIANGULAR AMT NEGATIVE
      * E550 TRIANGULAR AMT OVER 25000
      * E383 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4670-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN EURM-CUST-ID = SPACES
                   MOVE 'E213' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-AMT < 0
                   MOVE 'E850' TO WS-REJECT-CODE
               WHEN EURM-TRIANGULAR-AMT > 25000.00
                   MOVE 'E550' TO WS-REJECT-CODE
               WHEN EURT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E383' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT EURO YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4680-SPLIT-EURO.
           COMPUTE WS-WORK-AMT = EURM-EURO-YTD / 2
           COMPUTE WS-WORK-AMT-2 = EURM-EURO-YTD - (WS-WORK-AMT * 2)
           COMPUTE EURM-EURO-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4690-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF EURM-NOTICE-DATE > 0 AND EURM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE EURM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD EURT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD EURT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD EURT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD EURT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD EURT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EURO TIER TIER FOR THE RESIDUAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4700-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-EURO-TIER-RATE(1) TO EURM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF EURM-RESIDUAL-AMT NOT < WS-EURO-TIER-LOW(WS-TX)
                   MOVE WS-EURO-TIER-RATE(WS-TX) TO EURM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF EURM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO EURM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ROUNDING AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4710-FLAG-ROUNDING.
           IF EURT-ROUNDING-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE EURT-ROUNDING-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE EUR FIELDS TO EUFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-EUFEE.
           INITIALIZE EUFEE-PARMS
           MOVE EURM-CONVERTED-YTD
             TO EUFEE-CONVERTED-YTD
           MOVE EURM-NATIONAL-YTD
             TO EUFEE-NATIONAL-YTD
           MOVE EURM-NATIONAL-AMT
             TO EUFEE-NATIONAL-AMT
           MOVE EURM-TRIANGULAR-YTD
             TO EUFEE-TRIANGULAR-YTD
           MOVE EURM-EURO-YTD
             TO EUFEE-EURO-YTD
           MOVE EURM-FEE-YTD
             TO EUFEE-FEE-YTD
           MOVE EURM-LAST-MAINT-DATE
             TO EUFEE-LAST-MAINT-DATE
           MOVE EURM-NOTICE-DATE
             TO EUFEE-NOTICE-DATE
           MOVE EURM-EFFECTIVE-DATE
             TO EUFEE-EFFECTIVE-DATE
           MOVE EURM-LAST-PAY-DATE
             TO EUFEE-LAST-PAY-DATE
           MOVE EURM-CONV-METHOD-CD
             TO EUFEE-CONV-METHOD-CD
           MOVE EURM-REGION-CD
             TO EUFEE-REGION-CD
           MOVE EURM-TYPE-CD
             TO EUFEE-TYPE-CD
           MOVE EURM-CHANNEL-CD
             TO EUFEE-CHANNEL-CD
           MOVE EURM-PREFERRED-FLAG
             TO EUFEE-PREFERRED-FLAG
           MOVE EURM-OVERRIDE-FLAG
             TO EUFEE-OVERRIDE-FLAG
           MOVE EURM-REVIEW-FLAG
             TO EUFEE-REVIEW-FLAG
           MOVE EURM-CYCLE-CNT
             TO EUFEE-CYCLE-CNT
           MOVE EURM-PAYMENT-CNT
             TO EUFEE-PAYMENT-CNT
           MOVE EURM-FEE-PCT
             TO EUFEE-FEE-PCT
           MOVE EURM-RATE
             TO EUFEE-RATE
           MOVE EURM-REF-NO
             TO EUFEE-REF-NO
           MOVE EURM-CUST-ID
             TO EUFEE-CUST-ID
           CALL 'EUFEE' USING EUFEE-PARMS
           EVALUATE EUFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE EUFEE-NATIONAL-YTD
                     TO EURM-NATIONAL-YTD
                   MOVE EUFEE-NATIONAL-AMT
                     TO EURM-NATIONAL-AMT
                   MOVE EUFEE-FEE-YTD
                     TO EURM-FEE-YTD
                   MOVE EUFEE-PREFERRED-FLAG
                     TO EURM-PREFERRED-FLAG
                   MOVE EUFEE-REVIEW-FLAG
                     TO EURM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-EUFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE EURT-KEY TO RD-KEY
           MOVE EURT-TRAN-CD TO RD-CODE
           MOVE EURT-CONVERTED-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE EURT-KEY TO RD-KEY
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
           DISPLAY 'EURAUDT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'EURAUDT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
