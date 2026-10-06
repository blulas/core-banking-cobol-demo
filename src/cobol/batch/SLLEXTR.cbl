      *================================================================*
      * PROGRAM  : SLLEXTR                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : STUDENT LOANS - NIGHTLY EXTRACT. READS THE STL      *
      *            TRANSACTIONS, UPDATES THE STL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. GUARANTEED STUDENT LOAN         *
      *            DISBURSEMENT, DEFERMENT AND REPAYMENT               *
      * INPUT    : STLMST (KSDS), STLTRN, BANKCTL                      *
      * OUTPUT   : STLOUT, SLLEXRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SLVAL, SLLIM            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-02-20 TWH  ORIGINAL                                       *
      * 1994-02-16 TWH  DISBURSE CUTOFF MOVED CHG1789                  *
      * 1997-10-05 PQR  PRINCIPAL CALCULATION CORRECTED PRB8639        *
      * 1997-01-17 MKL  SUBSIDY FEE SCHEDULE UPDATE PRB4527            *
      * 1999-12-10 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-11-01 RJT  DISBURSE RERUN PROTECTION CHG6198              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLLEXTR.
       AUTHOR. T W HOLT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "STLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS STLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "STLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "STLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "SLLEXRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY STLMST.
       FD  TRAN-FILE.
       COPY STLTRN.
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
                                           VALUE 19.000000.
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
      * WS-DISBURSE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISBURSE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000592500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000647000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000506500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013000000000804500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013050000000031000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023050000000174000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028050000000010500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028150000000051000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028250000000518000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00028500000000120000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033500000000742000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00034500000000435500'.
       01  WS-DISBURSE-SCHED-TBL REDEFINES WS-DISBURSE-SCHED-VALUES.
           05  WS-DISBURSE-SCHED-ROW OCCURS 12 TIMES.
               10  WS-DISBURSE-SCHED-LOW   PIC 9(09)V99.
               10  WS-DISBURSE-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PRINCIPA-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PRINCIPA-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000643000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000722000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000341000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001150000000499000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001650000000186500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006650000000221500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007650000000250000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007900000000190500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017900000000876000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018150000000450500'.
       01  WS-PRINCIPA-GRID-TBL REDEFINES WS-PRINCIPA-GRID-VALUES.
           05  WS-PRINCIPA-GRID-ROW OCCURS 10 TIMES.
               10  WS-PRINCIPA-GRID-LOW    PIC 9(09)V99.
               10  WS-PRINCIPA-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SLLEXTR  STUDENT LOANS'.
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
       COPY SLVALP.
       COPY SLLIMP.

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
               MOVE 'STLMST' TO ABND-FILE-NAME
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
           MOVE STLT-KEY TO STLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-LIMIT-PAYMENT
               PERFORM 3010-SELECT-DISCOUNT
               PERFORM 3020-SPLIT-PRINCIPAL
               PERFORM 3030-SELECT-PENALTY-PCT
               PERFORM 3040-COUNT-EXCEPT
               PERFORM 3050-LIMIT-PAYMENT
               PERFORM 3060-DESCRIBE-DEFER-CODE
               PERFORM 3070-AGE-EXPIRY
               PERFORM 3080-SET-LOAN-TYPE
               PERFORM 3090-SET-TRAN
               PERFORM 3100-AGE-LAST-PAY
               PERFORM 3110-FLAG-ITEM
               PERFORM 3120-SET-SOURCE
               PERFORM 3130-DESCRIBE-STATUS
               PERFORM 3140-CHECK-OPEN
               PERFORM 3150-COUNT-CYCLE
               PERFORM 3160-FLAG-SUBSIDY
               PERFORM 3170-AGE-LAST-MAINT
               PERFORM 3180-DESCRIBE-REGION
               PERFORM 3190-CHECK-MATURITY
               PERFORM 3200-ACCUM-STATUS
               PERFORM 3210-ACCRUE-SUBSIDY
               PERFORM 3220-FLAG-DEFERINT
               PERFORM 3230-PRORATE-PAYMENT
               PERFORM 3240-SET-CYCLE
               PERFORM 3250-PRORATE-SUBSIDY
               PERFORM 3260-SELECT-DISCOUNT
               PERFORM 3270-SELECT-PENALTY-PCT
               PERFORM 3280-CHECK-MATURITY
               PERFORM 3290-LIMIT-CAPINT
               PERFORM 3300-LIMIT-DEFERINT
               PERFORM 3310-SET-TRAN
               PERFORM 3320-ACCUM-LOAN-TYPE
               PERFORM 3330-CHECK-MATURITY
               PERFORM 3340-SELECT-FEE-PCT
               PERFORM 3350-SET-CHANNEL
               PERFORM 3360-EDIT-ADDR
               PERFORM 3370-ACCRUE-PRINCIPAL
               PERFORM 3380-FLAG-CAPINT
               PERFORM 3390-FLAG-PAYMENT
               PERFORM 3400-ASSESS-REVERSAL
               PERFORM 3410-ACCUM-REGION
               PERFORM 3420-FLAG-DEFERINT
               PERFORM 3430-CHECK-LAST-ACT
               PERFORM 3440-ACCUM-SOURCE
               PERFORM 3450-ACCRUE-PRINCIPAL
               PERFORM 3460-AGE-NEXT-REVIEW
               PERFORM 3470-SELECT-FEE-PCT
               PERFORM 3480-SET-CHANNEL
               PERFORM 3490-SELECT-DISCOUNT
               PERFORM 3500-SELECT-FEE-PCT
               PERFORM 3510-FLAG-CAPINT
               PERFORM 3520-FLAG-PAYMENT
               PERFORM 3530-EDIT-DESC
               PERFORM 3540-FLAG-ITEM
               PERFORM 3550-SPLIT-PRINCIPAL
               PERFORM 3560-FLAG-CAPINT
               PERFORM 3570-AGE-LAST-PAY
               PERFORM 3580-ACCUM-REGION
               PERFORM 3590-SELECT-DISCOUNT
               PERFORM 3600-ASSESS-HOLD
               PERFORM 3610-AGE-MATURITY
               PERFORM 3620-ASSESS-SUSPEND
               PERFORM 3630-COUNT-PAYMENT
               PERFORM 3640-PRORATE-DISBURSE
               PERFORM 3650-PRORATE-DISBURSE
               PERFORM 3660-SPLIT-CAPINT
               PERFORM 3670-DESCRIBE-DEFER-CODE
               PERFORM 3680-AGE-LAST-MAINT
               PERFORM 3690-SPLIT-SUBSIDY
               PERFORM 3700-FLAG-DISBURSE
               PERFORM 3710-ASSESS-OVERRIDE
               PERFORM 3720-AGE-LAST-ACT
               PERFORM 3730-EDIT-REF-NO
               PERFORM 3740-ACCUM-SOURCE
               PERFORM 3750-SET-TRAN
               PERFORM 3760-SPLIT-SUBSIDY
               PERFORM 3770-SELECT-DISCOUNT
               PERFORM 3780-SET-TRAN
               PERFORM 3790-COUNT-SEQ
               PERFORM 3800-DESCRIBE-REGION
               PERFORM 3810-SET-CYCLE
               PERFORM 3820-SPLIT-DISBURSE
               PERFORM 3830-SPLIT-PRINCIPAL
               PERFORM 3840-SPLIT-DISBURSE
               PERFORM 3850-SELECT-PENALTY-PCT
               PERFORM 3860-SET-LOAN-TYPE
               PERFORM 3870-SET-SOURCE
               PERFORM 3880-SELECT-DISCOUNT
               PERFORM 3890-ASSESS-SUSPEND
               PERFORM 3900-LIMIT-PAYMENT
               PERFORM 3910-AGE-POST
               PERFORM 3920-PRORATE-PRINCIPAL
               PERFORM 3930-ACCRUE-DISBURSE
               PERFORM 3940-ACCRUE-DEFERINT
               PERFORM 3950-EDIT-CUST-ID
               PERFORM 3960-SET-CHANNEL
               PERFORM 3970-COUNT-EXCEPT
               PERFORM 3980-LIMIT-CAPINT
               PERFORM 3990-ACCUM-SOURCE
               PERFORM 4000-EDIT-ADDR
               PERFORM 4010-ACCRUE-PRINCIPAL
               PERFORM 4020-CHECK-POST
               PERFORM 4030-SELECT-FEE-PCT
               PERFORM 4040-ASSESS-PREFERRED
               PERFORM 4050-CHECK-OPEN
               PERFORM 4060-EDIT-NAME
               PERFORM 4070-SELECT-DISCOUNT
               PERFORM 7020-CALL-SLVAL
               PERFORM 7000-CALL-SLLIM
               REWRITE STL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'STLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM STL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3000-LIMIT-PAYMENT.
           IF STLM-PAYMENT-AMT < 50.00
               MOVE 50.00 TO STLM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-PAYMENT-AMT > 5000.00
               MOVE 5000.00 TO STLM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF STLM-PAYMENT-YTD NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = STLT-PRINCIPAL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = STLT-PRINCIPAL-AMT - (WS-WORK-AMT *
               4)
           COMPUTE STLM-DEFERINT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE CAPINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF STLT-CAPINT-AMT NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO STLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3050-LIMIT-PAYMENT.
           IF STLM-PAYMENT-YTD < 50.00
               MOVE 50.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-PAYMENT-YTD > 500.00
               MOVE 500.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DEFER CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3060-DESCRIBE-DEFER-CODE.
           EVALUATE STLM-DEFER-CODE-CD
               WHEN 'AH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'CZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ND'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOAN TYPE CD:
      * CS -> DV WHEN LAST STMT DATE IS PAST
      * DV -> JL WHEN LAST STMT DATE IS PAST
      * JL -> NE WHEN LAST STMT DATE IS PAST
      * NE -> SH WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3080-SET-LOAN-TYPE.
           EVALUATE STLM-LOAN-TYPE-CD
               WHEN 'CS'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DV' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DV'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JL'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NE'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DB -> PY WHEN POST DATE IS PAST
      * PY -> CP WHEN POST DATE IS PAST
      * CP -> DF WHEN POST DATE IS PAST
      * DF -> FB WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3090-SET-TRAN.
           EVALUATE STLT-TRAN-CD
               WHEN 'DB'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3100-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3110-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AST -> BCK WHEN NEXT REVIEW DATE IS PAST
      * BCK -> CJG WHEN NEXT REVIEW DATE IS PAST
      * CJG -> DSJ WHEN NEXT REVIEW DATE IS PAST
      * DSJ -> JVY WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-SOURCE.
           EVALUATE STLM-SOURCE-CD
               WHEN 'AST'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BCK' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BCK'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'CJG' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJG'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DSJ' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DSJ'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JVY' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-STATUS.
           EVALUATE STLM-STATUS-CD
               WHEN 'HY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RK'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF STLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO STLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3150-COUNT-CYCLE.
           IF STLM-CYCLE-CNT < 9999999
               ADD 1 TO STLM-CYCLE-CNT
           END-IF
           IF STLM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SUBSIDY YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3160-FLAG-SUBSIDY.
           IF STLM-SUBSIDY-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE STLM-SUBSIDY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3170-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'T'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'U'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF STLM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-STATUS-CD
                   ADD STLM-CAPINT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-CAPINT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SUBSIDY YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-SUBSIDY.
           IF STLM-SUBSIDY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-SUBSIDY-YTD * STLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO STLT-DISBURSE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEFERINT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3220-FLAG-DEFERINT.
           IF STLM-DEFERINT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLM-DEFERINT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3230-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-PAYMENT-AMT ROUNDED =
                   STLM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-PAYMENT-AMT
           END-IF
           ADD STLM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CFJ -> KHC WHEN EXPIRY DATE IS PAST
      * KHC -> SEU WHEN EXPIRY DATE IS PAST
      * SEU -> XKA WHEN EXPIRY DATE IS PAST
      * XKA -> CFJ WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3240-SET-CYCLE.
           EVALUATE STLM-CYCLE-CD
               WHEN 'CFJ'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHC' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KHC'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SEU' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SEU'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XKA' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XKA'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CFJ' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SUBSIDY AMT.
      *----------------------------------------------------------------*
       3250-PRORATE-SUBSIDY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-SUBSIDY-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-SUBSIDY-AMT
           END-IF
           ADD STLM-SUBSIDY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE SCHED TIER FOR THE DISBURSE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3260-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-SCHED-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF STLT-DISBURSE-AMT NOT < WS-DISBURSE-SCHED-LOW(WS-TX)
                   MOVE WS-DISBURSE-SCHED-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE SUBSIDY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF STLM-SUBSIDY-YTD NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3280-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF STLM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO STLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT YTD IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3290-LIMIT-CAPINT.
           IF STLM-CAPINT-YTD < 25.00
               MOVE 25.00 TO STLM-CAPINT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-CAPINT-YTD > 2500.00
               MOVE 2500.00 TO STLM-CAPINT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEFERINT AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3300-LIMIT-DEFERINT.
           IF STLM-DEFERINT-AMT < 100.00
               MOVE 100.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-DEFERINT-AMT > 500.00
               MOVE 500.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DB -> PY WHEN LAST PAY DATE IS PAST
      * PY -> CP WHEN LAST PAY DATE IS PAST
      * CP -> DF WHEN LAST PAY DATE IS PAST
      * DF -> FB WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3310-SET-TRAN.
           EVALUATE STLT-TRAN-CD
               WHEN 'DB'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY LOAN TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3320-ACCUM-LOAN-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-LOAN-TYPE-CD
                   ADD STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-LOAN-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3330-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF STLM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO STLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE CAPINT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3340-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF STLM-CAPINT-YTD NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3350-SET-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E631 ADDR REQUIRED
      * E816 DISBURSE YTD NEGATIVE
      * E127 DISBURSE YTD OVER 25000
      * E138 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-ADDR = SPACES
                   MOVE 'E631' TO WS-REJECT-CODE
               WHEN STLM-DISBURSE-YTD < 0
                   MOVE 'E816' TO WS-REJECT-CODE
               WHEN STLM-DISBURSE-YTD > 25000.00
                   MOVE 'E127' TO WS-REJECT-CODE
               WHEN STLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E138' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-PRINCIPAL.
           IF STLT-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLT-PRINCIPAL-AMT * STLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO STLT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3380-FLAG-CAPINT.
           IF STLM-CAPINT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLM-CAPINT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3390-FLAG-PAYMENT.
           IF STLM-PAYMENT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISBURSE YTD FEE ON THE CAPINT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3400-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO STLM-DISBURSE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-REVIEW-FLAG = 'Y'
                   COMPUTE STLM-DISBURSE-YTD ROUNDED = STLM-CAPINT-AMT *
                       STLM-PENALTY-PCT / 200
               WHEN STLM-LOAN-TYPE-CD = 'CS'
                   MOVE 10.00 TO STLM-DISBURSE-YTD
               WHEN STLM-LOAN-TYPE-CD = 'DV'
                   MOVE 20.00 TO STLM-DISBURSE-YTD
               WHEN OTHER
                   COMPUTE STLM-DISBURSE-YTD ROUNDED = STLM-CAPINT-AMT *
                       STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-DISBURSE-YTD > 0 AND STLM-DISBURSE-YTD < 5.00
               MOVE 5.00 TO STLM-DISBURSE-YTD
           END-IF
           IF STLM-DISBURSE-YTD > 35.00
               MOVE 35.00 TO STLM-DISBURSE-YTD
           END-IF
           ADD STLM-DISBURSE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3410-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-REGION-CD
                   ADD STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DEFERINT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3420-FLAG-DEFERINT.
           IF STLM-DEFERINT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLM-DEFERINT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3430-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO STLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3440-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-SOURCE-CD
                   ADD STLT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3450-ACCRUE-PRINCIPAL.
           IF STLM-PRINCIPAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-PRINCIPAL-YTD * STLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO STLT-PRINCIPAL-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF STLM-NEXT-REVIEW-DATE > 0 AND STLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3470-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3490-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE SCHED TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3500-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-SCHED-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF STLT-PRINCIPAL-AMT NOT < WS-DISBURSE-SCHED-LOW(WS-TX)
                   MOVE WS-DISBURSE-SCHED-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3510-FLAG-CAPINT.
           IF STLM-CAPINT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLM-CAPINT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3520-FLAG-PAYMENT.
           IF STLM-PAYMENT-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E727 DESC REQUIRED
      * E176 DISBURSE AMT NEGATIVE
      * E276 DISBURSE AMT OVER 5000
      * E888 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3530-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-DESC = SPACES
                   MOVE 'E727' TO WS-REJECT-CODE
               WHEN STLT-DISBURSE-AMT < 0
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN STLT-DISBURSE-AMT > 5000.00
                   MOVE 'E276' TO WS-REJECT-CODE
               WHEN STLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E888' TO WS-REJECT-CODE
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
       3540-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3550-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = STLM-PRINCIPAL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = STLM-PRINCIPAL-YTD - (WS-WORK-AMT *
               2)
           COMPUTE STLM-DEFERINT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3560-FLAG-CAPINT.
           IF STLM-CAPINT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLM-CAPINT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3570-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-REGION-CD
                   ADD STLM-DISBURSE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-DISBURSE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF STLM-PAYMENT-AMT NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEFERINT FEE ON THE CAPINT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3600-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN STLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO STLM-DEFERINT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   COMPUTE STLM-DEFERINT-AMT ROUNDED = STLM-CAPINT-AMT *
                       STLM-PENALTY-PCT / 200
               WHEN STLM-DEFER-CODE-CD = 'AH'
                   MOVE 7.00 TO STLM-DEFERINT-AMT
               WHEN STLM-DEFER-CODE-CD = 'CZ'
                   MOVE 10.00 TO STLM-DEFERINT-AMT
               WHEN OTHER
                   COMPUTE STLM-DEFERINT-AMT ROUNDED = STLM-CAPINT-AMT *
                       STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-DEFERINT-AMT > 0 AND STLM-DEFERINT-AMT < 2.00
               MOVE 2.00 TO STLM-DEFERINT-AMT
           END-IF
           IF STLM-DEFERINT-AMT > 25.00
               MOVE 25.00 TO STLM-DEFERINT-AMT
           END-IF
           ADD STLM-DEFERINT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3610-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SUBSIDY FEE ON THE CAPINT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3620-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN STLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO STLM-SUBSIDY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE STLM-SUBSIDY-AMT ROUNDED = STLT-CAPINT-AMT *
                       STLM-FEE-PCT / 200
               WHEN STLM-STATUS-CD = 'HY'
                   MOVE 15.00 TO STLM-SUBSIDY-AMT
               WHEN STLM-STATUS-CD = 'JZ'
                   MOVE 12.00 TO STLM-SUBSIDY-AMT
               WHEN OTHER
                   COMPUTE STLM-SUBSIDY-AMT ROUNDED = STLT-CAPINT-AMT *
                       STLM-FEE-PCT / 100
           END-EVALUATE
           IF STLM-SUBSIDY-AMT > 0 AND STLM-SUBSIDY-AMT < 1.00
               MOVE 1.00 TO STLM-SUBSIDY-AMT
           END-IF
           IF STLM-SUBSIDY-AMT > 35.00
               MOVE 35.00 TO STLM-SUBSIDY-AMT
           END-IF
           ADD STLM-SUBSIDY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       3640-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DISBURSE-AMT ROUNDED =
                   STLT-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-DISBURSE-AMT
           END-IF
           ADD STLM-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE DEFERINT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       3650-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLT-DISBURSE-AMT ROUNDED =
                   STLM-DEFERINT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLT-DISBURSE-AMT
           END-IF
           ADD STLT-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT CAPINT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-CAPINT.
           COMPUTE WS-WORK-AMT = STLT-CAPINT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = STLT-CAPINT-AMT - (WS-WORK-AMT * 2)
           COMPUTE STLT-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DEFER CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-DEFER-CODE.
           EVALUATE STLM-DEFER-CODE-CD
               WHEN 'AH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'CZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ND'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SUBSIDY AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3690-SPLIT-SUBSIDY.
           COMPUTE WS-WORK-AMT = STLM-SUBSIDY-AMT / 12
           COMPUTE WS-WORK-AMT-2 = STLM-SUBSIDY-AMT - (WS-WORK-AMT * 12)
           COMPUTE STLM-SUBSIDY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3700-FLAG-DISBURSE.
           IF STLM-DISBURSE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLM-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PAYMENT YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3710-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN STLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-SUSPEND-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = STLM-PAYMENT-YTD *
                       STLM-PENALTY-PCT / 200
               WHEN STLM-REGION-CD = 'C'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN STLM-REGION-CD = 'D'
                   MOVE 3.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = STLM-PAYMENT-YTD *
                       STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 35.00
               MOVE 35.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3720-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLT-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLT-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD STLT-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD STLT-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLT-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E744 REF NO REQUIRED
      * E948 WORK AMT 2 NEGATIVE
      * E990 WORK AMT 2 OVER 5000
      * E709 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3730-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-REF-NO = SPACES
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E948' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E990' TO WS-REJECT-CODE
               WHEN STLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E709' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3740-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-SOURCE-CD
                   ADD STLT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DB -> PY WHEN LAST PAY DATE IS PAST
      * PY -> CP WHEN LAST PAY DATE IS PAST
      * CP -> DF WHEN LAST PAY DATE IS PAST
      * DF -> FB WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-TRAN.
           EVALUATE STLT-TRAN-CD
               WHEN 'DB'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SUBSIDY YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3760-SPLIT-SUBSIDY.
           COMPUTE WS-WORK-AMT = STLM-SUBSIDY-YTD / 12
           COMPUTE WS-WORK-AMT-2 = STLM-SUBSIDY-YTD - (WS-WORK-AMT * 12)
           COMPUTE STLM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE DISBURSE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3770-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF STLM-DISBURSE-YTD NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DB -> PY WHEN LAST PAY DATE IS PAST
      * PY -> CP WHEN LAST PAY DATE IS PAST
      * CP -> DF WHEN LAST PAY DATE IS PAST
      * DF -> FB WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3780-SET-TRAN.
           EVALUATE STLT-TRAN-CD
               WHEN 'DB'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3790-COUNT-SEQ.
           IF STLT-SEQ < 9999999
               ADD 1 TO STLT-SEQ
           END-IF
           IF STLT-SEQ NOT < 12
               MOVE 'Y' TO STLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3800-DESCRIBE-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CFJ -> KHC WHEN MATURITY DATE IS PAST
      * KHC -> SEU WHEN MATURITY DATE IS PAST
      * SEU -> XKA WHEN MATURITY DATE IS PAST
      * XKA -> CFJ WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3810-SET-CYCLE.
           EVALUATE STLM-CYCLE-CD
               WHEN 'CFJ'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHC' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KHC'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SEU' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SEU'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XKA' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XKA'
                   IF STLM-MATURITY-DATE > 0 AND STLM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CFJ' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3820-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = STLM-DISBURSE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = STLM-DISBURSE-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = STLM-PRINCIPAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = STLM-PRINCIPAL-AMT - (WS-WORK-AMT *
               2)
           COMPUTE STLT-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3840-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE STLM-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PRINCIPA GRID TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3850-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PRINCIPA-GRID-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF STLT-PRINCIPAL-AMT NOT < WS-PRINCIPA-GRID-LOW(WS-TX)
                   MOVE WS-PRINCIPA-GRID-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOAN TYPE CD:
      * CS -> DV WHEN EXPIRY DATE IS PAST
      * DV -> JL WHEN EXPIRY DATE IS PAST
      * JL -> NE WHEN EXPIRY DATE IS PAST
      * NE -> SH WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3860-SET-LOAN-TYPE.
           EVALUATE STLM-LOAN-TYPE-CD
               WHEN 'CS'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DV' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DV'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JL'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NE'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AST -> BCK WHEN LAST MAINT DATE IS PAST
      * BCK -> CJG WHEN LAST MAINT DATE IS PAST
      * CJG -> DSJ WHEN LAST MAINT DATE IS PAST
      * DSJ -> JVY WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-SOURCE.
           EVALUATE STLM-SOURCE-CD
               WHEN 'AST'
                   IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'BCK' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BCK'
                   IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CJG' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CJG'
                   IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'DSJ' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DSJ'
                   IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JVY' TO STLM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE SCHED TIER FOR THE DISBURSE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3880-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-SCHED-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF STLM-DISBURSE-AMT NOT < WS-DISBURSE-SCHED-LOW(WS-TX)
                   MOVE WS-DISBURSE-SCHED-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEFERINT FEE ON THE SUBSIDY YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3890-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN STLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO STLM-DEFERINT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE STLM-DEFERINT-AMT ROUNDED = STLM-SUBSIDY-YTD
                       * STLM-DISCOUNT / 200
               WHEN STLM-LOAN-TYPE-CD = 'CS'
                   MOVE 12.00 TO STLM-DEFERINT-AMT
               WHEN STLM-LOAN-TYPE-CD = 'DV'
                   MOVE 20.00 TO STLM-DEFERINT-AMT
               WHEN OTHER
                   COMPUTE STLM-DEFERINT-AMT ROUNDED = STLM-SUBSIDY-YTD
                       * STLM-DISCOUNT / 100
           END-EVALUATE
           IF STLM-DEFERINT-AMT > 0 AND STLM-DEFERINT-AMT < 10.00
               MOVE 10.00 TO STLM-DEFERINT-AMT
           END-IF
           IF STLM-DEFERINT-AMT > 150.00
               MOVE 150.00 TO STLM-DEFERINT-AMT
           END-IF
           ADD STLM-DEFERINT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3900-LIMIT-PAYMENT.
           IF STLT-PAYMENT-AMT < 100.00
               MOVE 100.00 TO STLT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLT-PAYMENT-AMT > 10000.00
               MOVE 10000.00 TO STLT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF STLT-POST-DATE > 0 AND STLT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL YTD.
      *----------------------------------------------------------------*
       3920-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-PRINCIPAL-YTD ROUNDED =
                   STLM-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-PRINCIPAL-YTD
           END-IF
           ADD STLM-PRINCIPAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3930-ACCRUE-DISBURSE.
           IF STLT-DISBURSE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLT-DISBURSE-AMT * STLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO STLM-DISBURSE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DEFERINT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3940-ACCRUE-DEFERINT.
           IF STLM-DEFERINT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-DEFERINT-AMT * STLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO STLM-SUBSIDY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E262 CUST ID REQUIRED
      * E259 PAYMENT AMT NEGATIVE
      * E850 PAYMENT AMT OVER 5000
      * E872 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3950-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-CUST-ID = SPACES
                   MOVE 'E262' TO WS-REJECT-CODE
               WHEN STLT-PAYMENT-AMT < 0
                   MOVE 'E259' TO WS-REJECT-CODE
               WHEN STLT-PAYMENT-AMT > 5000.00
                   MOVE 'E850' TO WS-REJECT-CODE
               WHEN STLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3970-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO STLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3980-LIMIT-CAPINT.
           IF STLT-CAPINT-AMT < 0.00
               MOVE 0.00 TO STLT-CAPINT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLT-CAPINT-AMT > 5000.00
               MOVE 5000.00 TO STLT-CAPINT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3990-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-SOURCE-CD
                   ADD STLM-CAPINT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-CAPINT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E911 ADDR REQUIRED
      * E244 PAYMENT YTD NEGATIVE
      * E130 PAYMENT YTD OVER 99999
      * E218 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4000-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-ADDR = SPACES
                   MOVE 'E911' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-YTD < 0
                   MOVE 'E244' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-YTD > 99999.00
                   MOVE 'E130' TO WS-REJECT-CODE
               WHEN STLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E218' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4010-ACCRUE-PRINCIPAL.
           IF STLT-PRINCIPAL-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLT-PRINCIPAL-AMT * STLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO STLM-CAPINT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4020-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF STLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4030-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-SCHED-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-DISBURSE-SCHED-LOW(WS-TX)
                   MOVE WS-DISBURSE-SCHED-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE PAYMENT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4040-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN STLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO STLM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE STLM-PRINCIPAL-YTD ROUNDED = STLM-PAYMENT-YTD
                       * STLM-DISCOUNT / 200
               WHEN STLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO STLM-PRINCIPAL-YTD
               WHEN STLT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO STLM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE STLM-PRINCIPAL-YTD ROUNDED = STLM-PAYMENT-YTD
                       * STLM-DISCOUNT / 100
           END-EVALUATE
           IF STLM-PRINCIPAL-YTD > 0 AND STLM-PRINCIPAL-YTD < 10.00
               MOVE 10.00 TO STLM-PRINCIPAL-YTD
           END-IF
           IF STLM-PRINCIPAL-YTD > 75.00
               MOVE 75.00 TO STLM-PRINCIPAL-YTD
           END-IF
           ADD STLM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4050-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF STLM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E531 NAME REQUIRED
      * E508 PAYMENT YTD NEGATIVE
      * E389 PAYMENT YTD OVER 10000
      * E054 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4060-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-NAME = SPACES
                   MOVE 'E531' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-YTD < 0
                   MOVE 'E508' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-YTD > 10000.00
                   MOVE 'E389' TO WS-REJECT-CODE
               WHEN STLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E054' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE SCHED TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-SCHED-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF STLT-PRINCIPAL-AMT NOT < WS-DISBURSE-SCHED-LOW(WS-TX)
                   MOVE WS-DISBURSE-SCHED-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE STL FIELDS TO SLVAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-SLVAL.
           INITIALIZE SLVAL-PARMS
           MOVE STLM-DEFERINT-YTD
             TO SLVAL-DEFERINT-YTD
           MOVE STLM-SUBSIDY-YTD
             TO SLVAL-SUBSIDY-YTD
           MOVE STLM-PAYMENT-YTD
             TO SLVAL-PAYMENT-YTD
           MOVE STLM-CAPINT-AMT
             TO SLVAL-CAPINT-AMT
           MOVE STLM-DISBURSE-YTD
             TO SLVAL-DISBURSE-YTD
           MOVE STLM-CAPINT-YTD
             TO SLVAL-CAPINT-YTD
           MOVE STLM-EXPIRY-DATE
             TO SLVAL-EXPIRY-DATE
           MOVE STLM-LAST-MAINT-DATE
             TO SLVAL-LAST-MAINT-DATE
           MOVE STLM-LAST-PAY-DATE
             TO SLVAL-LAST-PAY-DATE
           MOVE STLM-LAST-ACT-DATE
             TO SLVAL-LAST-ACT-DATE
           MOVE STLM-SOURCE-CD
             TO SLVAL-SOURCE-CD
           MOVE STLM-CYCLE-CD
             TO SLVAL-CYCLE-CD
           MOVE STLM-LOAN-TYPE-CD
             TO SLVAL-LOAN-TYPE-CD
           MOVE STLM-DEFER-CODE-CD
             TO SLVAL-DEFER-CODE-CD
           MOVE STLM-PREFERRED-FLAG
             TO SLVAL-PREFERRED-FLAG
           MOVE STLM-SUSPEND-FLAG
             TO SLVAL-SUSPEND-FLAG
           MOVE STLM-REVIEW-FLAG
             TO SLVAL-REVIEW-FLAG
           MOVE STLM-PAYMENT-CNT
             TO SLVAL-PAYMENT-CNT
           MOVE STLM-ITEM-CNT
             TO SLVAL-ITEM-CNT
           MOVE STLM-FEE-PCT
             TO SLVAL-FEE-PCT
           MOVE STLM-DISCOUNT
             TO SLVAL-DISCOUNT
           MOVE STLM-DESC
             TO SLVAL-DESC
           MOVE STLM-CUST-ID
             TO SLVAL-CUST-ID
           CALL 'SLVAL' USING SLVAL-PARMS
           EVALUATE SLVAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SLVAL-PAYMENT-YTD
                     TO STLM-PAYMENT-YTD
                   MOVE SLVAL-CAPINT-AMT
                     TO STLM-CAPINT-AMT
                   MOVE SLVAL-CAPINT-YTD
                     TO STLM-CAPINT-YTD
                   MOVE SLVAL-PREFERRED-FLAG
                     TO STLM-PREFERRED-FLAG
                   MOVE SLVAL-SUSPEND-FLAG
                     TO STLM-SUSPEND-FLAG
                   MOVE SLVAL-REVIEW-FLAG
                     TO STLM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-SLVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE STL FIELDS TO SLLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-SLLIM.
           INITIALIZE SLLIM-PARMS
           MOVE STLM-CAPINT-AMT
             TO SLLIM-CAPINT-AMT
           MOVE STLM-DISBURSE-YTD
             TO SLLIM-DISBURSE-YTD
           MOVE STLM-SUBSIDY-YTD
             TO SLLIM-SUBSIDY-YTD
           MOVE STLM-PAYMENT-YTD
             TO SLLIM-PAYMENT-YTD
           MOVE STLM-PRINCIPAL-YTD
             TO SLLIM-PRINCIPAL-YTD
           MOVE STLM-PRINCIPAL-AMT
             TO SLLIM-PRINCIPAL-AMT
           MOVE STLM-MATURITY-DATE
             TO SLLIM-MATURITY-DATE
           MOVE STLM-CLOSE-DATE
             TO SLLIM-CLOSE-DATE
           MOVE STLM-LAST-STMT-DATE
             TO SLLIM-LAST-STMT-DATE
           MOVE STLM-EXPIRY-DATE
             TO SLLIM-EXPIRY-DATE
           MOVE STLM-LOAN-TYPE-CD
             TO SLLIM-LOAN-TYPE-CD
           MOVE STLM-SOURCE-CD
             TO SLLIM-SOURCE-CD
           MOVE STLM-REGION-CD
             TO SLLIM-REGION-CD
           MOVE STLM-CYCLE-CD
             TO SLLIM-CYCLE-CD
           MOVE STLM-NOTICE-SENT-FLAG
             TO SLLIM-NOTICE-SENT-FLAG
           MOVE STLM-HOLD-FLAG
             TO SLLIM-HOLD-FLAG
           MOVE STLM-REVIEW-FLAG
             TO SLLIM-REVIEW-FLAG
           MOVE STLM-PAYMENT-CNT
             TO SLLIM-PAYMENT-CNT
           MOVE STLM-ITEM-CNT
             TO SLLIM-ITEM-CNT
           MOVE STLM-DISCOUNT
             TO SLLIM-DISCOUNT
           MOVE STLM-FEE-PCT
             TO SLLIM-FEE-PCT
           MOVE STLM-CONTACT
             TO SLLIM-CONTACT
           MOVE STLM-NAME
             TO SLLIM-NAME
           CALL 'SLLIM' USING SLLIM-PARMS
           EVALUATE SLLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SLLIM-DISBURSE-YTD
                     TO STLM-DISBURSE-YTD
                   MOVE SLLIM-PRINCIPAL-YTD
                     TO STLM-PRINCIPAL-YTD
                   MOVE SLLIM-HOLD-FLAG
                     TO STLM-HOLD-FLAG
                   MOVE SLLIM-REVIEW-FLAG
                     TO STLM-REVIEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-SLLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE STLT-KEY TO RD-KEY
           MOVE STLT-TRAN-CD TO RD-CODE
           MOVE STLT-DISBURSE-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE STLT-KEY TO RD-KEY
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
           DISPLAY 'SLLEXTR READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SLLEXTR' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
