      *================================================================*
      * PROGRAM  : SLLSWEP                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : STUDENT LOANS - SWEEP. READS THE STL TRANSACTIONS,  *
      *            UPDATES THE STL MASTER AND PRODUCES THE CONTROL     *
      *            REPORT. GUARANTEED STUDENT LOAN DISBURSEMENT,       *
      *            DEFERMENT AND REPAYMENT                             *
      * INPUT    : STLMST (KSDS), STLTRN, BANKCTL                      *
      * OUTPUT   : STLOUT, SLLSWRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SLVAL, SLLIM, SLCAL     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-04-11 LMB  ORIGINAL                                       *
      * 1988-10-10 MKL  PAYMENT NOTICE TEXT PER LEGAL CHG8954          *
      * 1993-06-24 MKL  PRINCIPAL CALCULATION CORRECTED REQ0420        *
      * 1998-10-04 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-09-10 PQR  SUBSIDY EXCEPTIONS TO OPS QUEUE REQ0480        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLLSWEP.
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
           SELECT REPORT-FILE ASSIGN TO "SLLSWRPT"
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
                                           VALUE 15.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 28.
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
      * WS-SUBSIDY-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SUBSIDY-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000028000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000344500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000640500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000070500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009000000000126500'.
       01  WS-SUBSIDY-GRID-TBL REDEFINES WS-SUBSIDY-GRID-VALUES.
           05  WS-SUBSIDY-GRID-ROW OCCURS 5 TIMES.
               10  WS-SUBSIDY-GRID-LOW     PIC 9(09)V99.
               10  WS-SUBSIDY-GRID-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISBURSE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISBURSE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000601500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000115000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001500000000419000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001550000000121000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004050000000080500'.
       01  WS-DISBURSE-SCHED-TBL REDEFINES WS-DISBURSE-SCHED-VALUES.
           05  WS-DISBURSE-SCHED-ROW OCCURS 5 TIMES.
               10  WS-DISBURSE-SCHED-LOW   PIC 9(09)V99.
               10  WS-DISBURSE-SCHED-RATE  PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SLLSWEP  STUDENT LOANS'.
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
       COPY SLCALP.

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
               PERFORM 3000-CHECK-POST
               PERFORM 3010-SPLIT-CAPINT
               PERFORM 3020-LIMIT-CAPINT
               PERFORM 3030-CHECK-CLOSE
               PERFORM 3040-SELECT-PENALTY-PCT
               PERFORM 3050-EDIT-OPERATOR
               PERFORM 3060-FLAG-SUBSIDY
               PERFORM 3070-DESCRIBE-REGION
               PERFORM 3080-EDIT-DESC
               PERFORM 3090-LIMIT-CAPINT
               PERFORM 3100-SET-DEFER-CODE
               PERFORM 3110-ACCRUE-PAYMENT
               PERFORM 3120-COUNT-CYCLE
               PERFORM 3130-SPLIT-DEFERINT
               PERFORM 3140-ACCRUE-PAYMENT
               PERFORM 3150-CHECK-CLOSE
               PERFORM 3160-COUNT-EXCEPT
               PERFORM 3170-SPLIT-SUBSIDY
               PERFORM 3180-COUNT-EXCEPT
               PERFORM 3190-COUNT-EXCEPT
               PERFORM 3200-SET-REGION
               PERFORM 3210-PRORATE-PAYMENT
               PERFORM 3220-SPLIT-DEFERINT
               PERFORM 3230-ACCRUE-DISBURSE
               PERFORM 3240-AGE-EFF
               PERFORM 3250-SPLIT-PAYMENT
               PERFORM 3260-COUNT-PAYMENT
               PERFORM 3270-PRORATE-SUBSIDY
               PERFORM 3280-ACCRUE-PRINCIPAL
               PERFORM 3290-AGE-MATURITY
               PERFORM 3300-CHECK-CLOSE
               PERFORM 3310-LIMIT-PAYMENT
               PERFORM 3320-LIMIT-DEFERINT
               PERFORM 3330-LIMIT-PAYMENT
               PERFORM 3340-EDIT-REF-NO
               PERFORM 3350-SELECT-DISCOUNT
               PERFORM 3360-FLAG-CAPINT
               PERFORM 3370-ACCUM-DEFER-CODE
               PERFORM 3380-SET-CHANNEL
               PERFORM 3390-SELECT-FEE-PCT
               PERFORM 3400-SET-REGION
               PERFORM 3410-AGE-LAST-PAY
               PERFORM 3420-SPLIT-CAPINT
               PERFORM 3430-ACCUM-REGION
               PERFORM 3440-ASSESS-PREFERRED
               PERFORM 3450-DESCRIBE-CHANNEL
               PERFORM 3460-DESCRIBE-SOURCE
               PERFORM 3470-CHECK-MATURITY
               PERFORM 3480-LIMIT-PRINCIPAL
               PERFORM 3490-DESCRIBE-STATUS
               PERFORM 3500-PRORATE-PAYMENT
               PERFORM 3510-SELECT-FEE-PCT
               PERFORM 3520-SPLIT-SUBSIDY
               PERFORM 3530-SPLIT-PAYMENT
               PERFORM 3540-SET-CYCLE
               PERFORM 3550-LIMIT-PAYMENT
               PERFORM 3560-ACCUM-LOAN-TYPE
               PERFORM 3570-SPLIT-CAPINT
               PERFORM 3580-PRORATE-PAYMENT
               PERFORM 3590-COUNT-EXCEPT
               PERFORM 3600-SELECT-DISCOUNT
               PERFORM 3610-COUNT-PAYMENT
               PERFORM 3620-SET-LOAN-TYPE
               PERFORM 3630-SET-TRAN
               PERFORM 3640-ACCUM-SOURCE
               PERFORM 3650-ACCRUE-PAYMENT
               PERFORM 3660-CHECK-LAST-PAY
               PERFORM 3670-SPLIT-DISBURSE
               PERFORM 3680-ASSESS-REVIEW
               PERFORM 3690-ACCUM-SOURCE
               PERFORM 3700-SPLIT-DEFERINT
               PERFORM 3710-PRORATE-PRINCIPAL
               PERFORM 3720-SELECT-PENALTY-PCT
               PERFORM 3730-PRORATE-PAYMENT
               PERFORM 3740-ASSESS-SUSPEND
               PERFORM 3750-SET-LOAN-TYPE
               PERFORM 3760-LIMIT-CAPINT
               PERFORM 3770-CHECK-LAST-STMT
               PERFORM 3780-ACCRUE-CAPINT
               PERFORM 3790-SPLIT-PRINCIPAL
               PERFORM 3800-DESCRIBE-LOAN-TYPE
               PERFORM 3810-AGE-LAST-MAINT
               PERFORM 3820-ASSESS-REVIEW
               PERFORM 3830-FLAG-DISBURSE
               PERFORM 3840-DESCRIBE-REGION
               PERFORM 3850-ACCUM-REGION
               PERFORM 3860-ACCUM-CYCLE
               PERFORM 3870-COUNT-EXCEPT
               PERFORM 3880-SPLIT-PAYMENT
               PERFORM 3890-ASSESS-OVERRIDE
               PERFORM 3900-LIMIT-DISBURSE
               PERFORM 3910-LIMIT-DISBURSE
               PERFORM 3920-PRORATE-PAYMENT
               PERFORM 3930-ACCUM-SOURCE
               PERFORM 3940-ASSESS-NOTICE-SENT
               PERFORM 3950-FLAG-PAYMENT
               PERFORM 3960-EDIT-REJECT
               PERFORM 3970-SPLIT-PRINCIPAL
               PERFORM 3980-SET-REGION
               PERFORM 3990-SPLIT-PRINCIPAL
               PERFORM 4000-DESCRIBE-DEFER-CODE
               PERFORM 4010-COUNT-ITEM
               PERFORM 4020-SET-STATUS
               PERFORM 4030-SET-STATUS
               PERFORM 4040-EDIT-MEMO
               PERFORM 4050-LIMIT-PRINCIPAL
               PERFORM 4060-COUNT-EXCEPT
               PERFORM 4070-FLAG-CAPINT
               PERFORM 4080-COUNT-ITEM
               PERFORM 4090-FLAG-ITEM
               PERFORM 4100-FLAG-PAYMENT
               PERFORM 4110-SPLIT-DISBURSE
               PERFORM 4120-ACCRUE-SUBSIDY
               PERFORM 4130-SET-LOAN-TYPE
               PERFORM 4140-AGE-EFF
               PERFORM 4150-EDIT-DESC
               PERFORM 4160-LIMIT-PAYMENT
               PERFORM 4170-EDIT-CUST-ID
               PERFORM 4180-LIMIT-SUBSIDY
               PERFORM 4190-EDIT-CUST-ID
               PERFORM 7020-CALL-SLVAL
               PERFORM 7000-CALL-SLLIM
               PERFORM 7010-CALL-SLCAL
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
      * DAYS SINCE POST DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3000-CHECK-POST.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO STLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CAPINT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3010-SPLIT-CAPINT.
           COMPUTE WS-WORK-AMT = STLM-CAPINT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = STLM-CAPINT-YTD - (WS-WORK-AMT * 4)
           COMPUTE STLM-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3020-LIMIT-CAPINT.
           IF STLM-CAPINT-YTD < 50.00
               MOVE 50.00 TO STLM-CAPINT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-CAPINT-YTD > 50000.00
               MOVE 50000.00 TO STLM-CAPINT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3030-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF STLM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO STLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE SCHED TIER FOR THE DISBURSE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-SCHED-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF STLM-DISBURSE-YTD NOT < WS-DISBURSE-SCHED-LOW(WS-TX)
                   MOVE WS-DISBURSE-SCHED-RATE(WS-TX) TO
                       STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E283 OPERATOR REQUIRED
      * E678 SUBSIDY YTD NEGATIVE
      * E229 SUBSIDY YTD OVER 25000
      * E118 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3050-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-OPERATOR = SPACES
                   MOVE 'E283' TO WS-REJECT-CODE
               WHEN STLM-SUBSIDY-YTD < 0
                   MOVE 'E678' TO WS-REJECT-CODE
               WHEN STLM-SUBSIDY-YTD > 25000.00
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN STLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E118' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SUBSIDY AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3060-FLAG-SUBSIDY.
           IF STLM-SUBSIDY-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE STLM-SUBSIDY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3070-DESCRIBE-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'D'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'T'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E552 DESC REQUIRED
      * E704 PAYMENT YTD NEGATIVE
      * E521 PAYMENT YTD OVER 99999
      * E290 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-DESC = SPACES
                   MOVE 'E552' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-YTD < 0
                   MOVE 'E704' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-YTD > 99999.00
                   MOVE 'E521' TO WS-REJECT-CODE
               WHEN STLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E290' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3090-LIMIT-CAPINT.
           IF STLT-CAPINT-AMT < 100.00
               MOVE 100.00 TO STLT-CAPINT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLT-CAPINT-AMT > 50000.00
               MOVE 50000.00 TO STLT-CAPINT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEFER CODE CD:
      * AH -> CZ WHEN NEXT REVIEW DATE IS PAST
      * CZ -> GM WHEN NEXT REVIEW DATE IS PAST
      * GM -> ND WHEN NEXT REVIEW DATE IS PAST
      * ND -> UC WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3100-SET-DEFER-CODE.
           EVALUATE STLM-DEFER-CODE-CD
               WHEN 'AH'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'CZ' TO STLM-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZ'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'GM' TO STLM-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ND' TO STLM-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UC' TO STLM-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3110-ACCRUE-PAYMENT.
           IF STLT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLT-PAYMENT-AMT * STLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO STLM-DISBURSE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3120-COUNT-CYCLE.
           IF STLM-CYCLE-CNT < 9999999
               ADD 1 TO STLM-CYCLE-CNT
           END-IF
           IF STLM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO STLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3130-SPLIT-DEFERINT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE STLM-DEFERINT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3140-ACCRUE-PAYMENT.
           IF STLM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-PAYMENT-YTD * STLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO STLM-DISBURSE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF STLM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO STLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3160-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SUBSIDY YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-SUBSIDY.
           COMPUTE WS-WORK-AMT = STLM-SUBSIDY-YTD / 2
           COMPUTE WS-WORK-AMT-2 = STLM-SUBSIDY-YTD - (WS-WORK-AMT * 2)
           COMPUTE STLM-DISBURSE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3180-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 12
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3190-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO STLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> D WHEN POST DATE IS PAST
      * D -> T WHEN POST DATE IS PAST
      * T -> U WHEN POST DATE IS PAST
      * U -> W WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3200-SET-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       3210-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-PAYMENT-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-PAYMENT-YTD
           END-IF
           ADD STLM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT DEFERINT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3220-SPLIT-DEFERINT.
           COMPUTE WS-WORK-AMT = STLM-DEFERINT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = STLM-DEFERINT-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-DISBURSE.
           IF STLM-DISBURSE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-DISBURSE-AMT * STLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO STLM-DISBURSE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3240-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3250-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = STLM-PAYMENT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = STLM-PAYMENT-AMT - (WS-WORK-AMT * 4)
           COMPUTE STLM-SUBSIDY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3260-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO STLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SUBSIDY YTD.
      *----------------------------------------------------------------*
       3270-PRORATE-SUBSIDY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-SUBSIDY-YTD ROUNDED =
                   STLM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-SUBSIDY-YTD
           END-IF
           ADD STLM-SUBSIDY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-PRINCIPAL.
           IF STLM-PRINCIPAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-PRINCIPAL-YTD * STLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO STLM-DISBURSE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3290-AGE-MATURITY.
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
                   ADD STLT-DISBURSE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD STLT-DISBURSE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD STLT-DISBURSE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLT-DISBURSE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLT-DISBURSE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3300-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF STLM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO STLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3310-LIMIT-PAYMENT.
           IF STLT-PAYMENT-AMT < 50.00
               MOVE 50.00 TO STLT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLT-PAYMENT-AMT > 500.00
               MOVE 500.00 TO STLT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEFERINT AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3320-LIMIT-DEFERINT.
           IF STLM-DEFERINT-AMT < 10.00
               MOVE 10.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-DEFERINT-AMT > 500.00
               MOVE 500.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3330-LIMIT-PAYMENT.
           IF STLM-PAYMENT-AMT < 100.00
               MOVE 100.00 TO STLM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-PAYMENT-AMT > 2500.00
               MOVE 2500.00 TO STLM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E265 REF NO REQUIRED
      * E816 PAYMENT YTD NEGATIVE
      * E401 PAYMENT YTD OVER 25000
      * E046 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3340-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-REF-NO = SPACES
                   MOVE 'E265' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-YTD < 0
                   MOVE 'E816' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-YTD > 25000.00
                   MOVE 'E401' TO WS-REJECT-CODE
               WHEN STLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E046' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE SCHED TIER FOR THE DEFERINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-SCHED-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF STLM-DEFERINT-AMT NOT < WS-DISBURSE-SCHED-LOW(WS-TX)
                   MOVE WS-DISBURSE-SCHED-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3360-FLAG-CAPINT.
           IF STLM-CAPINT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLM-CAPINT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEFERINT AMT BY DEFER CODE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-DEFER-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-DEFER-CODE-CD
                   ADD STLM-DEFERINT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-DEFER-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-DEFERINT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3380-SET-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO STLT-CHANNEL-CD
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
       3390-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-SCHED-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF STLT-DISBURSE-AMT NOT < WS-DISBURSE-SCHED-LOW(WS-TX)
                   MOVE WS-DISBURSE-SCHED-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> D WHEN EXPIRY DATE IS PAST
      * D -> T WHEN EXPIRY DATE IS PAST
      * T -> U WHEN EXPIRY DATE IS PAST
      * U -> W WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-LAST-PAY.
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
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CAPINT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-CAPINT.
           COMPUTE WS-WORK-AMT = STLM-CAPINT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = STLM-CAPINT-AMT - (WS-WORK-AMT * 2)
           COMPUTE STLM-DISBURSE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-REGION-CD
                   ADD STLT-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLT-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEFERINT YTD FEE ON THE SUBSIDY AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3440-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN STLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO STLM-DEFERINT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-HOLD-FLAG = 'Y'
                   COMPUTE STLM-DEFERINT-YTD ROUNDED = STLM-SUBSIDY-AMT
                       * STLM-FEE-PCT / 200
               WHEN STLM-LOAN-TYPE-CD = 'CS'
                   MOVE 3.00 TO STLM-DEFERINT-YTD
               WHEN STLM-LOAN-TYPE-CD = 'DV'
                   MOVE 12.00 TO STLM-DEFERINT-YTD
               WHEN OTHER
                   COMPUTE STLM-DEFERINT-YTD ROUNDED = STLM-SUBSIDY-AMT
                       * STLM-FEE-PCT / 100
           END-EVALUATE
           IF STLM-DEFERINT-YTD > 0 AND STLM-DEFERINT-YTD < 1.00
               MOVE 1.00 TO STLM-DEFERINT-YTD
           END-IF
           IF STLM-DEFERINT-YTD > 35.00
               MOVE 35.00 TO STLM-DEFERINT-YTD
           END-IF
           ADD STLM-DEFERINT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3450-DESCRIBE-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3460-DESCRIBE-SOURCE.
           EVALUATE STLM-SOURCE-CD
               WHEN 'AST'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BCK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CJG'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DSJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JVY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VEF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3470-CHECK-MATURITY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO STLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3480-LIMIT-PRINCIPAL.
           IF STLT-PRINCIPAL-AMT < 100.00
               MOVE 100.00 TO STLT-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLT-PRINCIPAL-AMT > 500.00
               MOVE 500.00 TO STLT-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-STATUS.
           EVALUATE STLM-STATUS-CD
               WHEN 'HY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       3500-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-PAYMENT-YTD ROUNDED =
                   STLM-DISBURSE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-PAYMENT-YTD
           END-IF
           ADD STLM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE SCHED TIER FOR THE PRINCIPAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3510-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-SCHED-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF STLM-PRINCIPAL-YTD NOT < WS-DISBURSE-SCHED-LOW(WS-TX)
                   MOVE WS-DISBURSE-SCHED-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SUBSIDY YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-SUBSIDY.
           COMPUTE WS-WORK-AMT = STLM-SUBSIDY-YTD / 2
           COMPUTE WS-WORK-AMT-2 = STLM-SUBSIDY-YTD - (WS-WORK-AMT * 2)
           COMPUTE STLM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3530-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = STLM-PAYMENT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = STLM-PAYMENT-AMT - (WS-WORK-AMT * 4)
           COMPUTE STLT-PRINCIPAL-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CFJ -> KHC WHEN LAST MAINT DATE IS PAST
      * KHC -> SEU WHEN LAST MAINT DATE IS PAST
      * SEU -> XKA WHEN LAST MAINT DATE IS PAST
      * XKA -> CFJ WHEN LAST MAINT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3540-SET-CYCLE.
           EVALUATE STLM-CYCLE-CD
               WHEN 'CFJ'
                   IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KHC' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KHC'
                   IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'SEU' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SEU'
                   IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'XKA' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XKA'
                   IF STLM-LAST-MAINT-DATE > 0 AND STLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CFJ' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3550-LIMIT-PAYMENT.
           IF STLM-PAYMENT-YTD < 0.00
               MOVE 0.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-PAYMENT-YTD > 1000.00
               MOVE 1000.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY LOAN TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-LOAN-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-LOAN-TYPE-CD
                   ADD STLT-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-LOAN-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLT-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CAPINT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3570-SPLIT-CAPINT.
           COMPUTE WS-WORK-AMT = STLT-CAPINT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = STLT-CAPINT-AMT - (WS-WORK-AMT * 4)
           COMPUTE STLM-CAPINT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       3580-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-PAYMENT-YTD ROUNDED =
                   STLT-PRINCIPAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-PAYMENT-YTD
           END-IF
           ADD STLM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3590-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO STLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SUBSIDY GRID TIER FOR THE PRINCIPAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3600-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-SUBSIDY-GRID-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF STLM-PRINCIPAL-YTD NOT < WS-SUBSIDY-GRID-LOW(WS-TX)
                   MOVE WS-SUBSIDY-GRID-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3610-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO STLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOAN TYPE CD:
      * CS -> DV WHEN POST DATE IS PAST
      * DV -> JL WHEN POST DATE IS PAST
      * JL -> NE WHEN POST DATE IS PAST
      * NE -> SH WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3620-SET-LOAN-TYPE.
           EVALUATE STLM-LOAN-TYPE-CD
               WHEN 'CS'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DV' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DV'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JL'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NE'
                   IF STLT-POST-DATE > 0 AND STLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DB -> PY WHEN EFF DATE IS PAST
      * PY -> CP WHEN EFF DATE IS PAST
      * CP -> DF WHEN EFF DATE IS PAST
      * DF -> FB WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-TRAN.
           EVALUATE STLT-TRAN-CD
               WHEN 'DB'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3640-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-SOURCE-CD
                   ADD STLT-CAPINT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLT-CAPINT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-PAYMENT.
           IF STLM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-PAYMENT-YTD * STLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3660-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-PAY-DATE TO DTU-DATE-1
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
      * SPLIT DISBURSE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3670-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = STLT-DISBURSE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = STLT-DISBURSE-AMT - (WS-WORK-AMT * 3)
           COMPUTE STLM-DEFERINT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3680-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN STLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO STLM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE STLM-PAYMENT-YTD ROUNDED = STLT-PRINCIPAL-AMT
                       * STLM-DISCOUNT / 200
               WHEN STLM-DEFER-CODE-CD = 'AH'
                   MOVE 12.00 TO STLM-PAYMENT-YTD
               WHEN STLM-DEFER-CODE-CD = 'CZ'
                   MOVE 15.00 TO STLM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE STLM-PAYMENT-YTD ROUNDED = STLT-PRINCIPAL-AMT
                       * STLM-DISCOUNT / 100
           END-EVALUATE
           IF STLM-PAYMENT-YTD > 0 AND STLM-PAYMENT-YTD < 1.00
               MOVE 1.00 TO STLM-PAYMENT-YTD
           END-IF
           IF STLM-PAYMENT-YTD > 75.00
               MOVE 75.00 TO STLM-PAYMENT-YTD
           END-IF
           ADD STLM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3690-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-SOURCE-CD
                   ADD STLM-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEFERINT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3700-SPLIT-DEFERINT.
           COMPUTE WS-WORK-AMT = STLM-DEFERINT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = STLM-DEFERINT-AMT - (WS-WORK-AMT * 4)
           COMPUTE STLM-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CAPINT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL YTD.
      *----------------------------------------------------------------*
       3710-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-PRINCIPAL-YTD ROUNDED =
                   STLT-CAPINT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-PRINCIPAL-YTD
           END-IF
           ADD STLM-PRINCIPAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE SUBSIDY GRID TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3720-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SUBSIDY-GRID-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF STLT-PAYMENT-AMT NOT < WS-SUBSIDY-GRID-LOW(WS-TX)
                   MOVE WS-SUBSIDY-GRID-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3730-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-PAYMENT-AMT ROUNDED =
                   STLM-PRINCIPAL-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-PAYMENT-AMT
           END-IF
           ADD STLM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE SUBSIDY FEE ON THE CAPINT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3740-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN STLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO STLM-SUBSIDY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-REVIEW-FLAG = 'Y'
                   COMPUTE STLM-SUBSIDY-AMT ROUNDED = STLM-CAPINT-AMT *
                       STLM-DISCOUNT / 200
               WHEN STLM-REGION-CD = 'C'
                   MOVE 7.00 TO STLM-SUBSIDY-AMT
               WHEN STLM-REGION-CD = 'D'
                   MOVE 5.00 TO STLM-SUBSIDY-AMT
               WHEN OTHER
                   COMPUTE STLM-SUBSIDY-AMT ROUNDED = STLM-CAPINT-AMT *
                       STLM-DISCOUNT / 100
           END-EVALUATE
           IF STLM-SUBSIDY-AMT > 0 AND STLM-SUBSIDY-AMT < 1.00
               MOVE 1.00 TO STLM-SUBSIDY-AMT
           END-IF
           IF STLM-SUBSIDY-AMT > 35.00
               MOVE 35.00 TO STLM-SUBSIDY-AMT
           END-IF
           ADD STLM-SUBSIDY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOAN TYPE CD:
      * CS -> DV WHEN NEXT REVIEW DATE IS PAST
      * DV -> JL WHEN NEXT REVIEW DATE IS PAST
      * JL -> NE WHEN NEXT REVIEW DATE IS PAST
      * NE -> SH WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-LOAN-TYPE.
           EVALUATE STLM-LOAN-TYPE-CD
               WHEN 'CS'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'DV' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DV'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'JL' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JL'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'NE' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NE'
                   IF STLM-NEXT-REVIEW-DATE > 0 AND
                       STLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'SH' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CAPINT AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3760-LIMIT-CAPINT.
           IF STLM-CAPINT-AMT < 0.00
               MOVE 0.00 TO STLM-CAPINT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-CAPINT-AMT > 2500.00
               MOVE 2500.00 TO STLM-CAPINT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3770-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF STLM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO STLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CAPINT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3780-ACCRUE-CAPINT.
           IF STLT-CAPINT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLT-CAPINT-AMT * STLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO STLM-DISBURSE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3790-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = STLM-PRINCIPAL-YTD / 12
           COMPUTE WS-WORK-AMT-2 = STLM-PRINCIPAL-YTD - (WS-WORK-AMT *
               12)
           COMPUTE STLM-SUBSIDY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE LOAN TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3800-DESCRIBE-LOAN-TYPE.
           EVALUATE STLM-LOAN-TYPE-CD
               WHEN 'CS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3810-AGE-LAST-MAINT.
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
               WHEN WS-DAYS NOT > 30
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-CAPINT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT FEE ON THE WORK AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3820-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN STLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO STLT-CAPINT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE STLT-CAPINT-AMT ROUNDED = WS-WORK-AMT *
                       STLM-PENALTY-PCT / 200
               WHEN STLM-LOAN-TYPE-CD = 'CS'
                   MOVE 12.00 TO STLT-CAPINT-AMT
               WHEN STLM-LOAN-TYPE-CD = 'DV'
                   MOVE 12.00 TO STLT-CAPINT-AMT
               WHEN OTHER
                   COMPUTE STLT-CAPINT-AMT ROUNDED = WS-WORK-AMT *
                       STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLT-CAPINT-AMT > 0 AND STLT-CAPINT-AMT < 1.00
               MOVE 1.00 TO STLT-CAPINT-AMT
           END-IF
           IF STLT-CAPINT-AMT > 250.00
               MOVE 250.00 TO STLT-CAPINT-AMT
           END-IF
           ADD STLT-CAPINT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DISBURSE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3830-FLAG-DISBURSE.
           IF STLT-DISBURSE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLT-DISBURSE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'D'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'U'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3850-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-REGION-CD
                   ADD STLT-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLT-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3860-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-CYCLE-CD
                   ADD STLM-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3870-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO STLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3880-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = STLM-PAYMENT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = STLM-PAYMENT-AMT - (WS-WORK-AMT * 4)
           COMPUTE STLM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISBURSE YTD FEE ON THE PAYMENT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3890-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN STLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO STLM-DISBURSE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE STLM-DISBURSE-YTD ROUNDED = STLM-PAYMENT-AMT
                       * STLM-FEE-PCT / 200
               WHEN STLM-DEFER-CODE-CD = 'AH'
                   MOVE 20.00 TO STLM-DISBURSE-YTD
               WHEN STLM-DEFER-CODE-CD = 'CZ'
                   MOVE 7.00 TO STLM-DISBURSE-YTD
               WHEN OTHER
                   COMPUTE STLM-DISBURSE-YTD ROUNDED = STLM-PAYMENT-AMT
                       * STLM-FEE-PCT / 100
           END-EVALUATE
           IF STLM-DISBURSE-YTD > 0 AND STLM-DISBURSE-YTD < 2.00
               MOVE 2.00 TO STLM-DISBURSE-YTD
           END-IF
           IF STLM-DISBURSE-YTD > 35.00
               MOVE 35.00 TO STLM-DISBURSE-YTD
           END-IF
           ADD STLM-DISBURSE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DISBURSE YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3900-LIMIT-DISBURSE.
           IF STLM-DISBURSE-YTD < 10.00
               MOVE 10.00 TO STLM-DISBURSE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-DISBURSE-YTD > 5000.00
               MOVE 5000.00 TO STLM-DISBURSE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3910-LIMIT-DISBURSE.
           IF STLM-DISBURSE-YTD < 0.00
               MOVE 0.00 TO STLM-DISBURSE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-DISBURSE-YTD > 50000.00
               MOVE 50000.00 TO STLM-DISBURSE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       3920-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-PAYMENT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-PAYMENT-YTD
           END-IF
           ADD STLM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3930-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-SOURCE-CD
                   ADD STLT-DISBURSE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLT-DISBURSE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE WORK AMT 2.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3940-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO STLT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-PREFERRED-FLAG = 'Y'
                   COMPUTE STLT-PAYMENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       STLM-PENALTY-PCT / 200
               WHEN STLM-REGION-CD = 'C'
                   MOVE 20.00 TO STLT-PAYMENT-AMT
               WHEN STLM-REGION-CD = 'D'
                   MOVE 5.00 TO STLT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE STLT-PAYMENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLT-PAYMENT-AMT > 0 AND STLT-PAYMENT-AMT < 5.00
               MOVE 5.00 TO STLT-PAYMENT-AMT
           END-IF
           IF STLT-PAYMENT-AMT > 100.00
               MOVE 100.00 TO STLT-PAYMENT-AMT
           END-IF
           ADD STLT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3950-FLAG-PAYMENT.
           IF STLM-PAYMENT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E506 REJECT CD REQUIRED
      * E837 SUBSIDY YTD NEGATIVE
      * E521 SUBSIDY YTD OVER 50000
      * E198 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-REJECT-CD = SPACES
                   MOVE 'E506' TO WS-REJECT-CODE
               WHEN STLM-SUBSIDY-YTD < 0
                   MOVE 'E837' TO WS-REJECT-CODE
               WHEN STLM-SUBSIDY-YTD > 50000.00
                   MOVE 'E521' TO WS-REJECT-CODE
               WHEN STLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E198' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3970-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = STLM-PRINCIPAL-YTD / 2
           COMPUTE WS-WORK-AMT-2 = STLM-PRINCIPAL-YTD - (WS-WORK-AMT *
               2)
           COMPUTE STLM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> D WHEN EFF DATE IS PAST
      * D -> T WHEN EFF DATE IS PAST
      * T -> U WHEN EFF DATE IS PAST
      * U -> W WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3980-SET-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = STLT-PRINCIPAL-AMT / 4
           COMPUTE WS-WORK-AMT-2 = STLT-PRINCIPAL-AMT - (WS-WORK-AMT *
               4)
           COMPUTE STLM-PRINCIPAL-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DEFER CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4000-DESCRIBE-DEFER-CODE.
           EVALUATE STLM-DEFER-CODE-CD
               WHEN 'AH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ND'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4010-COUNT-ITEM.
           IF STLM-ITEM-CNT < 9999999
               ADD 1 TO STLM-ITEM-CNT
           END-IF
           IF STLM-ITEM-CNT NOT < 5
               MOVE 'Y' TO STLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * HY -> JZ WHEN LAST STMT DATE IS PAST
      * JZ -> RK WHEN LAST STMT DATE IS PAST
      * RK -> HY WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4020-SET-STATUS.
           EVALUATE STLM-STATUS-CD
               WHEN 'HY'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO STLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RK' TO STLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RK'
                   IF STLM-LAST-STMT-DATE > 0 AND STLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HY' TO STLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * HY -> JZ WHEN LAST ACT DATE IS PAST
      * JZ -> RK WHEN LAST ACT DATE IS PAST
      * RK -> HY WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4030-SET-STATUS.
           EVALUATE STLM-STATUS-CD
               WHEN 'HY'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO STLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RK' TO STLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RK'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HY' TO STLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E238 MEMO REQUIRED
      * E712 PAYMENT YTD NEGATIVE
      * E673 PAYMENT YTD OVER 1000000
      * E005 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4040-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-MEMO = SPACES
                   MOVE 'E238' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-YTD < 0
                   MOVE 'E712' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-YTD > 1000000.00
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN STLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E005' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4050-LIMIT-PRINCIPAL.
           IF STLM-PRINCIPAL-AMT < 25.00
               MOVE 25.00 TO STLM-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-PRINCIPAL-AMT > 500.00
               MOVE 500.00 TO STLM-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4060-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO STLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4070-FLAG-CAPINT.
           IF STLM-CAPINT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLM-CAPINT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4080-COUNT-ITEM.
           IF STLM-ITEM-CNT < 9999999
               ADD 1 TO STLM-ITEM-CNT
           END-IF
           IF STLM-ITEM-CNT NOT < 10
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4090-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4100-FLAG-PAYMENT.
           IF STLM-PAYMENT-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4110-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = STLM-DISBURSE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = STLM-DISBURSE-YTD - (WS-WORK-AMT * 4)
           COMPUTE STLM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SUBSIDY YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4120-ACCRUE-SUBSIDY.
           IF STLM-SUBSIDY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-SUBSIDY-YTD * STLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOAN TYPE CD:
      * CS -> DV WHEN LAST PAY DATE IS PAST
      * DV -> JL WHEN LAST PAY DATE IS PAST
      * JL -> NE WHEN LAST PAY DATE IS PAST
      * NE -> SH WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4130-SET-LOAN-TYPE.
           EVALUATE STLM-LOAN-TYPE-CD
               WHEN 'CS'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DV' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DV'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JL'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NE'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4140-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-DEFERINT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-DEFERINT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD STLM-DEFERINT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLM-DEFERINT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-DEFERINT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E264 DESC REQUIRED
      * E610 PAYMENT YTD NEGATIVE
      * E027 PAYMENT YTD OVER 25000
      * E270 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4150-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-DESC = SPACES
                   MOVE 'E264' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-YTD < 0
                   MOVE 'E610' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-YTD > 25000.00
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN STLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E270' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4160-LIMIT-PAYMENT.
           IF STLM-PAYMENT-YTD < 100.00
               MOVE 100.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-PAYMENT-YTD > 2500.00
               MOVE 2500.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E055 CUST ID REQUIRED
      * E187 DISBURSE AMT NEGATIVE
      * E584 DISBURSE AMT OVER 25000
      * E419 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4170-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-CUST-ID = SPACES
                   MOVE 'E055' TO WS-REJECT-CODE
               WHEN STLM-DISBURSE-AMT < 0
                   MOVE 'E187' TO WS-REJECT-CODE
               WHEN STLM-DISBURSE-AMT > 25000.00
                   MOVE 'E584' TO WS-REJECT-CODE
               WHEN STLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E419' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SUBSIDY AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4180-LIMIT-SUBSIDY.
           IF STLM-SUBSIDY-AMT < 10.00
               MOVE 10.00 TO STLM-SUBSIDY-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-SUBSIDY-AMT > 1000.00
               MOVE 1000.00 TO STLM-SUBSIDY-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E621 CUST ID REQUIRED
      * E529 CAPINT AMT NEGATIVE
      * E044 CAPINT AMT OVER 5000
      * E131 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4190-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-CUST-ID = SPACES
                   MOVE 'E621' TO WS-REJECT-CODE
               WHEN STLM-CAPINT-AMT < 0
                   MOVE 'E529' TO WS-REJECT-CODE
               WHEN STLM-CAPINT-AMT > 5000.00
                   MOVE 'E044' TO WS-REJECT-CODE
               WHEN STLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E131' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
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
                   MOVE SLVAL-DEFERINT-YTD
                     TO STLM-DEFERINT-YTD
                   MOVE SLVAL-SUBSIDY-YTD
                     TO STLM-SUBSIDY-YTD
                   MOVE SLVAL-PAYMENT-YTD
                     TO STLM-PAYMENT-YTD
                   MOVE SLVAL-DISBURSE-YTD
                     TO STLM-DISBURSE-YTD
                   MOVE SLVAL-PREFERRED-FLAG
                     TO STLM-PREFERRED-FLAG
                   MOVE SLVAL-SUSPEND-FLAG
                     TO STLM-SUSPEND-FLAG
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
                   MOVE SLLIM-CAPINT-AMT
                     TO STLM-CAPINT-AMT
                   MOVE SLLIM-DISBURSE-YTD
                     TO STLM-DISBURSE-YTD
                   MOVE SLLIM-PAYMENT-YTD
                     TO STLM-PAYMENT-YTD
                   MOVE SLLIM-PRINCIPAL-AMT
                     TO STLM-PRINCIPAL-AMT
                   MOVE SLLIM-HOLD-FLAG
                     TO STLM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-SLLIM' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE STL FIELDS TO SLCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-SLCAL.
           INITIALIZE SLCAL-PARMS
           MOVE STLM-CAPINT-AMT
             TO SLCAL-CAPINT-AMT
           MOVE STLM-SUBSIDY-YTD
             TO SLCAL-SUBSIDY-YTD
           MOVE STLM-PAYMENT-AMT
             TO SLCAL-PAYMENT-AMT
           MOVE STLM-PRINCIPAL-AMT
             TO SLCAL-PRINCIPAL-AMT
           MOVE STLM-DEFERINT-YTD
             TO SLCAL-DEFERINT-YTD
           MOVE STLM-DISBURSE-YTD
             TO SLCAL-DISBURSE-YTD
           MOVE STLM-OPEN-DATE
             TO SLCAL-OPEN-DATE
           MOVE STLM-EXPIRY-DATE
             TO SLCAL-EXPIRY-DATE
           MOVE STLM-LAST-ACT-DATE
             TO SLCAL-LAST-ACT-DATE
           MOVE STLM-LAST-PAY-DATE
             TO SLCAL-LAST-PAY-DATE
           MOVE STLM-CYCLE-CD
             TO SLCAL-CYCLE-CD
           MOVE STLM-DEFER-CODE-CD
             TO SLCAL-DEFER-CODE-CD
           MOVE STLM-SOURCE-CD
             TO SLCAL-SOURCE-CD
           MOVE STLM-REGION-CD
             TO SLCAL-REGION-CD
           MOVE STLM-REVIEW-FLAG
             TO SLCAL-REVIEW-FLAG
           MOVE STLM-NOTICE-SENT-FLAG
             TO SLCAL-NOTICE-SENT-FLAG
           MOVE STLM-OVERRIDE-FLAG
             TO SLCAL-OVERRIDE-FLAG
           MOVE STLM-EXCEPT-CNT
             TO SLCAL-EXCEPT-CNT
           MOVE STLM-PAYMENT-CNT
             TO SLCAL-PAYMENT-CNT
           MOVE STLM-PENALTY-PCT
             TO SLCAL-PENALTY-PCT
           MOVE STLM-FEE-PCT
             TO SLCAL-FEE-PCT
           MOVE STLM-NAME
             TO SLCAL-NAME
           MOVE STLM-ADDR
             TO SLCAL-ADDR
           CALL 'SLCAL' USING SLCAL-PARMS
           EVALUATE SLCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE SLCAL-CAPINT-AMT
                     TO STLM-CAPINT-AMT
                   MOVE SLCAL-DEFERINT-YTD
                     TO STLM-DEFERINT-YTD
                   MOVE SLCAL-DISBURSE-YTD
                     TO STLM-DISBURSE-YTD
                   MOVE SLCAL-REVIEW-FLAG
                     TO STLM-REVIEW-FLAG
                   MOVE SLCAL-NOTICE-SENT-FLAG
                     TO STLM-NOTICE-SENT-FLAG
                   MOVE SLCAL-OVERRIDE-FLAG
                     TO STLM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-SLCAL' TO ABND-PARAGRAPH
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
           DISPLAY 'SLLSWEP READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SLLSWEP' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
