      *================================================================*
      * PROGRAM  : SLLANAL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : STUDENT LOANS - ANALYSIS. READS THE STL             *
      *            TRANSACTIONS, UPDATES THE STL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. GUARANTEED STUDENT LOAN         *
      *            DISBURSEMENT, DEFERMENT AND REPAYMENT               *
      * INPUT    : STLMST (KSDS), STLTRN, BANKCTL                      *
      * OUTPUT   : STLOUT, SLLANRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SLCAL, SLLIM            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-12-06 PQR  ORIGINAL                                       *
      * 1991-06-12 TWH  SUBSIDY RERUN PROTECTION CHG4629               *
      * 1995-05-08 DPS  DISBURSE ROUNDING - HALF UP PRB2239            *
      * 1998-11-03 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-09-18 MKL  PRINCIPAL NOTICE TEXT PER LEGAL PRB6183        *
      * 2001-09-05 PQR  DISBURSE CUTOFF MOVED INC5737                  *
      * 2001-09-04 GAF  SUBSIDY CALCULATION CORRECTED PRB0903          *
      * 2002-12-26 LMB  PRINCIPAL NOTICE TEXT PER LEGAL PRB9605        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLLANAL.
       AUTHOR. M K LOWE.

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
           SELECT REPORT-FILE ASSIGN TO "SLLANRPT"
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
      * WS-DEFERINT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEFERINT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000239000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000746500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000622000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000898500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011200000000639000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011700000000663000'.
       01  WS-DEFERINT-TIER-TBL REDEFINES WS-DEFERINT-TIER-VALUES.
           05  WS-DEFERINT-TIER-ROW OCCURS 6 TIMES.
               10  WS-DEFERINT-TIER-LOW    PIC 9(09)V99.
               10  WS-DEFERINT-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISBURSE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISBURSE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000276500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000095500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000397500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005350000000255000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005850000000269500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005900000000761000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010900000000713000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000404000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000616000'.
       01  WS-DISBURSE-BAND-TBL REDEFINES WS-DISBURSE-BAND-VALUES.
           05  WS-DISBURSE-BAND-ROW OCCURS 9 TIMES.
               10  WS-DISBURSE-BAND-LOW    PIC 9(09)V99.
               10  WS-DISBURSE-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SLLANAL  STUDENT LOANS'.
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
       COPY SLCALP.
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
               PERFORM 3000-SELECT-DISCOUNT
               PERFORM 3010-PRORATE-DEFERINT
               PERFORM 3020-SET-CYCLE
               PERFORM 3030-COUNT-SEQ
               PERFORM 3040-LIMIT-ITEM
               PERFORM 3050-CHECK-POST
               PERFORM 3060-ACCUM-CYCLE
               PERFORM 3070-AGE-LAST-MAINT
               PERFORM 3080-EDIT-MEMO
               PERFORM 3090-DESCRIBE-CYCLE
               PERFORM 3100-COUNT-EXCEPT
               PERFORM 3110-AGE-LAST-PAY
               PERFORM 3120-ASSESS-REVERSAL
               PERFORM 3130-ASSESS-REVIEW
               PERFORM 3140-DESCRIBE-CYCLE
               PERFORM 3150-COUNT-PAYMENT
               PERFORM 3160-PRORATE-PRINCIPAL
               PERFORM 3170-PRORATE-PRINCIPAL
               PERFORM 3180-CHECK-LAST-ACT
               PERFORM 3190-AGE-NEXT-REVIEW
               PERFORM 3200-ACCUM-CYCLE
               PERFORM 3210-DESCRIBE-DEFER-CODE
               PERFORM 3220-LIMIT-DEFERINT
               PERFORM 3230-DESCRIBE-CYCLE
               PERFORM 3240-ACCRUE-CAPINT
               PERFORM 3250-LIMIT-DISBURSE
               PERFORM 3260-PRORATE-CAPINT
               PERFORM 3270-SPLIT-DISBURSE
               PERFORM 3280-ASSESS-SUSPEND
               PERFORM 3290-ACCUM-REGION
               PERFORM 3300-COUNT-ITEM
               PERFORM 3310-ASSESS-REVIEW
               PERFORM 3320-LIMIT-DISBURSE
               PERFORM 3330-ACCUM-TRAN
               PERFORM 3340-COUNT-SEQ
               PERFORM 3350-ASSESS-HOLD
               PERFORM 3360-DESCRIBE-LOAN-TYPE
               PERFORM 3370-PRORATE-SUBSIDY
               PERFORM 3380-EDIT-REF-NO
               PERFORM 3390-SET-TRAN
               PERFORM 3400-SET-CHANNEL
               PERFORM 3410-CHECK-NEXT-REVIEW
               PERFORM 3420-FLAG-DEFERINT
               PERFORM 3430-AGE-MATURITY
               PERFORM 3440-SPLIT-SUBSIDY
               PERFORM 3450-SELECT-FEE-PCT
               PERFORM 3460-SPLIT-PRINCIPAL
               PERFORM 3470-CHECK-LAST-STMT
               PERFORM 3480-SET-CHANNEL
               PERFORM 3490-PRORATE-DISBURSE
               PERFORM 3500-ACCUM-LOAN-TYPE
               PERFORM 3510-PRORATE-CAPINT
               PERFORM 3520-SPLIT-DISBURSE
               PERFORM 3530-CHECK-LAST-PAY
               PERFORM 3540-FLAG-ITEM
               PERFORM 3550-COUNT-CYCLE
               PERFORM 3560-COUNT-ITEM
               PERFORM 3570-SET-LOAN-TYPE
               PERFORM 3580-ACCUM-STATUS
               PERFORM 3590-PRORATE-SUBSIDY
               PERFORM 3600-DESCRIBE-CHANNEL
               PERFORM 3610-PRORATE-PRINCIPAL
               PERFORM 3620-EDIT-ADDR
               PERFORM 3630-CHECK-MATURITY
               PERFORM 3640-LIMIT-PAYMENT
               PERFORM 3650-CHECK-LAST-STMT
               PERFORM 3660-ACCUM-CYCLE
               PERFORM 3670-AGE-LAST-MAINT
               PERFORM 3680-CHECK-POST
               PERFORM 3690-FLAG-ITEM
               PERFORM 3700-ACCUM-TRAN
               PERFORM 3710-PRORATE-DISBURSE
               PERFORM 3720-COUNT-PAYMENT
               PERFORM 3730-SELECT-PENALTY-PCT
               PERFORM 3740-CHECK-EFF
               PERFORM 3750-LIMIT-ITEM
               PERFORM 3760-ACCRUE-PENALTY-PCT
               PERFORM 3770-ACCUM-CYCLE
               PERFORM 3780-FLAG-SUBSIDY
               PERFORM 3790-SELECT-DISCOUNT
               PERFORM 3800-ACCRUE-DISBURSE
               PERFORM 3810-LIMIT-DISBURSE
               PERFORM 3820-DESCRIBE-STATUS
               PERFORM 3830-COUNT-PAYMENT
               PERFORM 3840-ASSESS-HOLD
               PERFORM 3850-PRORATE-CAPINT
               PERFORM 3860-CHECK-CLOSE
               PERFORM 3870-ASSESS-HOLD
               PERFORM 3880-DESCRIBE-LOAN-TYPE
               PERFORM 3890-EDIT-REJECT
               PERFORM 3900-ACCUM-CHANNEL
               PERFORM 3910-CHECK-MATURITY
               PERFORM 3920-PRORATE-CAPINT
               PERFORM 3930-SET-CYCLE
               PERFORM 3940-EDIT-REF-NO
               PERFORM 3950-ACCUM-STATUS
               PERFORM 3960-SET-REGION
               PERFORM 3970-ASSESS-NOTICE-SENT
               PERFORM 3980-COUNT-SEQ
               PERFORM 3990-LIMIT-PAYMENT
               PERFORM 4000-FLAG-CAPINT
               PERFORM 4010-EDIT-CUST-ID
               PERFORM 4020-SPLIT-CAPINT
               PERFORM 4030-ACCUM-REGION
               PERFORM 4040-AGE-LAST-ACT
               PERFORM 4050-DESCRIBE-CHANNEL
               PERFORM 4060-SET-TRAN
               PERFORM 4070-SELECT-PENALTY-PCT
               PERFORM 4080-DESCRIBE-CYCLE
               PERFORM 7010-CALL-SLCAL
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
      * SELECT THE DEFERINT TIER TIER FOR THE SUBSIDY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3000-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-TIER-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF STLM-SUBSIDY-YTD NOT < WS-DEFERINT-TIER-LOW(WS-TX)
                   MOVE WS-DEFERINT-TIER-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SUBSIDY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEFERINT YTD.
      *----------------------------------------------------------------*
       3010-PRORATE-DEFERINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DEFERINT-YTD ROUNDED =
                   STLM-SUBSIDY-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-DEFERINT-YTD
           END-IF
           ADD STLM-DEFERINT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CFJ -> KHC WHEN MATURITY DATE IS PAST
      * KHC -> SEU WHEN MATURITY DATE IS PAST
      * SEU -> XKA WHEN MATURITY DATE IS PAST
      * XKA -> CFJ WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3020-SET-CYCLE.
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
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3030-COUNT-SEQ.
           IF STLT-SEQ < 9999999
               ADD 1 TO STLT-SEQ
           END-IF
           IF STLT-SEQ NOT < 6
               MOVE 'Y' TO STLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3040-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3050-CHECK-POST.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3060-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-CYCLE-CD
                   ADD STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3070-AGE-LAST-MAINT.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E528 MEMO REQUIRED
      * E437 PRINCIPAL AMT NEGATIVE
      * E516 PRINCIPAL AMT OVER 25000
      * E229 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3080-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-MEMO = SPACES
                   MOVE 'E528' TO WS-REJECT-CODE
               WHEN STLM-PRINCIPAL-AMT < 0
                   MOVE 'E437' TO WS-REJECT-CODE
               WHEN STLM-PRINCIPAL-AMT > 25000.00
                   MOVE 'E516' TO WS-REJECT-CODE
               WHEN STLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3090-DESCRIBE-CYCLE.
           EVALUATE STLM-CYCLE-CD
               WHEN 'CFJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'KHC'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SEU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XKA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3110-AGE-LAST-PAY.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT FEE ON THE PRINCIPAL YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3120-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO STLM-CAPINT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-HOLD-FLAG = 'Y'
                   COMPUTE STLM-CAPINT-AMT ROUNDED = STLM-PRINCIPAL-YTD
                       * STLM-FEE-PCT / 200
               WHEN STLM-STATUS-CD = 'HY'
                   MOVE 10.00 TO STLM-CAPINT-AMT
               WHEN STLM-STATUS-CD = 'JZ'
                   MOVE 7.00 TO STLM-CAPINT-AMT
               WHEN OTHER
                   COMPUTE STLM-CAPINT-AMT ROUNDED = STLM-PRINCIPAL-YTD
                       * STLM-FEE-PCT / 100
           END-EVALUATE
           IF STLM-CAPINT-AMT > 0 AND STLM-CAPINT-AMT < 2.00
               MOVE 2.00 TO STLM-CAPINT-AMT
           END-IF
           IF STLM-CAPINT-AMT > 50.00
               MOVE 50.00 TO STLM-CAPINT-AMT
           END-IF
           ADD STLM-CAPINT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL YTD FEE ON THE DISBURSE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3130-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN STLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO STLM-PRINCIPAL-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-SUSPEND-FLAG = 'Y'
                   COMPUTE STLM-PRINCIPAL-YTD ROUNDED =
                       STLM-DISBURSE-AMT * STLM-FEE-PCT / 200
               WHEN STLT-TRAN-CD = 'DB'
                   MOVE 3.00 TO STLM-PRINCIPAL-YTD
               WHEN STLT-TRAN-CD = 'PY'
                   MOVE 12.00 TO STLM-PRINCIPAL-YTD
               WHEN OTHER
                   COMPUTE STLM-PRINCIPAL-YTD ROUNDED =
                       STLM-DISBURSE-AMT * STLM-FEE-PCT / 100
           END-EVALUATE
           IF STLM-PRINCIPAL-YTD > 0 AND STLM-PRINCIPAL-YTD < 1.00
               MOVE 1.00 TO STLM-PRINCIPAL-YTD
           END-IF
           IF STLM-PRINCIPAL-YTD > 250.00
               MOVE 250.00 TO STLM-PRINCIPAL-YTD
           END-IF
           ADD STLM-PRINCIPAL-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3140-DESCRIBE-CYCLE.
           EVALUATE STLM-CYCLE-CD
               WHEN 'CFJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KHC'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'SEU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'XKA'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3150-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO STLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SUBSIDY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL YTD.
      *----------------------------------------------------------------*
       3160-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-PRINCIPAL-YTD ROUNDED =
                   STLM-SUBSIDY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-PRINCIPAL-YTD
           END-IF
           ADD STLM-PRINCIPAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE CAPINT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PRINCIPAL YTD.
      *----------------------------------------------------------------*
       3170-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-PRINCIPAL-YTD ROUNDED =
                   STLT-CAPINT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-PRINCIPAL-YTD
           END-IF
           ADD STLM-PRINCIPAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3180-CHECK-LAST-ACT.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3190-AGE-NEXT-REVIEW.
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
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-CYCLE-CD
                   ADD STLM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DEFER CODE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3210-DESCRIBE-DEFER-CODE.
           EVALUATE STLM-DEFER-CODE-CD
               WHEN 'AH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'CZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ND'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEFERINT AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3220-LIMIT-DEFERINT.
           IF STLM-DEFERINT-AMT < 100.00
               MOVE 100.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-DEFERINT-AMT > 10000.00
               MOVE 10000.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3230-DESCRIBE-CYCLE.
           EVALUATE STLM-CYCLE-CD
               WHEN 'CFJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KHC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SEU'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XKA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CAPINT YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-CAPINT.
           IF STLM-CAPINT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-CAPINT-YTD * STLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3250-LIMIT-DISBURSE.
           IF STLT-DISBURSE-AMT < 100.00
               MOVE 100.00 TO STLT-DISBURSE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLT-DISBURSE-AMT > 500.00
               MOVE 500.00 TO STLT-DISBURSE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CAPINT AMT.
      *----------------------------------------------------------------*
       3260-PRORATE-CAPINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLT-CAPINT-AMT ROUNDED =
                   STLM-DISBURSE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLT-CAPINT-AMT
           END-IF
           ADD STLT-CAPINT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3270-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = STLT-DISBURSE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = STLT-DISBURSE-AMT - (WS-WORK-AMT * 4)
           COMPUTE STLM-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE DISBURSE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3280-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN STLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = STLT-DISBURSE-AMT *
                       STLM-FEE-PCT / 200
               WHEN STLT-TRAN-CD = 'DB'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN STLT-TRAN-CD = 'PY'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = STLT-DISBURSE-AMT *
                       STLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 5.00
               MOVE 5.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 100.00
               MOVE 100.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3290-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-REGION-CD
                   ADD STLM-CAPINT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-CAPINT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-ITEM.
           IF STLM-ITEM-CNT < 9999999
               ADD 1 TO STLM-ITEM-CNT
           END-IF
           IF STLM-ITEM-CNT NOT < 6
               MOVE 'Y' TO STLM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT FEE ON THE CAPINT YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3310-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN STLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO STLM-CAPINT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE STLM-CAPINT-AMT ROUNDED = STLM-CAPINT-YTD *
                       STLM-DISCOUNT / 200
               WHEN STLM-STATUS-CD = 'HY'
                   MOVE 15.00 TO STLM-CAPINT-AMT
               WHEN STLM-STATUS-CD = 'JZ'
                   MOVE 3.00 TO STLM-CAPINT-AMT
               WHEN OTHER
                   COMPUTE STLM-CAPINT-AMT ROUNDED = STLM-CAPINT-YTD *
                       STLM-DISCOUNT / 100
           END-EVALUATE
           IF STLM-CAPINT-AMT > 0 AND STLM-CAPINT-AMT < 2.00
               MOVE 2.00 TO STLM-CAPINT-AMT
           END-IF
           IF STLM-CAPINT-AMT > 35.00
               MOVE 35.00 TO STLM-CAPINT-AMT
           END-IF
           ADD STLM-CAPINT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DISBURSE AMT IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3320-LIMIT-DISBURSE.
           IF STLT-DISBURSE-AMT < 25.00
               MOVE 25.00 TO STLT-DISBURSE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLT-DISBURSE-AMT > 1000.00
               MOVE 1000.00 TO STLT-DISBURSE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3330-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLT-TRAN-CD
                   ADD STLM-DISBURSE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-DISBURSE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3340-COUNT-SEQ.
           IF STLT-SEQ < 9999999
               ADD 1 TO STLT-SEQ
           END-IF
           IF STLT-SEQ NOT < 5
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEFERINT FEE ON THE WORK AMT 2.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3350-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN STLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO STLM-DEFERINT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-SUSPEND-FLAG = 'Y'
                   COMPUTE STLM-DEFERINT-AMT ROUNDED = WS-WORK-AMT-2 *
                       STLM-FEE-PCT / 200
               WHEN STLT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO STLM-DEFERINT-AMT
               WHEN STLT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO STLM-DEFERINT-AMT
               WHEN OTHER
                   COMPUTE STLM-DEFERINT-AMT ROUNDED = WS-WORK-AMT-2 *
                       STLM-FEE-PCT / 100
           END-EVALUATE
           IF STLM-DEFERINT-AMT > 0 AND STLM-DEFERINT-AMT < 1.00
               MOVE 1.00 TO STLM-DEFERINT-AMT
           END-IF
           IF STLM-DEFERINT-AMT > 250.00
               MOVE 250.00 TO STLM-DEFERINT-AMT
           END-IF
           ADD STLM-DEFERINT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE LOAN TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3360-DESCRIBE-LOAN-TYPE.
           EVALUATE STLM-LOAN-TYPE-CD
               WHEN 'CS'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'DV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NE'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SUBSIDY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SUBSIDY YTD.
      *----------------------------------------------------------------*
       3370-PRORATE-SUBSIDY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-SUBSIDY-YTD ROUNDED =
                   STLM-SUBSIDY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-SUBSIDY-YTD
           END-IF
           ADD STLM-SUBSIDY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E948 REF NO REQUIRED
      * E869 DISBURSE AMT NEGATIVE
      * E445 DISBURSE AMT OVER 50000
      * E161 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3380-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-REF-NO = SPACES
                   MOVE 'E948' TO WS-REJECT-CODE
               WHEN STLM-DISBURSE-AMT < 0
                   MOVE 'E869' TO WS-REJECT-CODE
               WHEN STLM-DISBURSE-AMT > 50000.00
                   MOVE 'E445' TO WS-REJECT-CODE
               WHEN STLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E161' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DB -> PY WHEN LAST ACT DATE IS PAST
      * PY -> CP WHEN LAST ACT DATE IS PAST
      * CP -> DF WHEN LAST ACT DATE IS PAST
      * DF -> FB WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3390-SET-TRAN.
           EVALUATE STLT-TRAN-CD
               WHEN 'DB'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CP' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CP'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DF' TO STLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DF'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FB' TO STLT-TRAN-CD
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
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3400-SET-CHANNEL.
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
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3410-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF STLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-NEXT-REVIEW-DATE TO DTU-DATE-1
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
      * DEFERINT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3420-FLAG-DEFERINT.
           IF STLM-DEFERINT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLM-DEFERINT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-MATURITY.
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
               WHEN WS-DAYS NOT > 15
                   ADD STLM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD STLM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SUBSIDY AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3440-SPLIT-SUBSIDY.
           COMPUTE WS-WORK-AMT = STLM-SUBSIDY-AMT / 4
           COMPUTE WS-WORK-AMT-2 = STLM-SUBSIDY-AMT - (WS-WORK-AMT * 4)
           COMPUTE STLM-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE BAND TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-BAND-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF STLT-PRINCIPAL-AMT NOT < WS-DISBURSE-BAND-LOW(WS-TX)
                   MOVE WS-DISBURSE-BAND-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3460-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = STLM-PRINCIPAL-AMT / 2
           COMPUTE WS-WORK-AMT-2 = STLM-PRINCIPAL-AMT - (WS-WORK-AMT *
               2)
           COMPUTE STLT-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3470-CHECK-LAST-STMT.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO STLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF STLM-EXPIRY-DATE > 0 AND STLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       3490-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DISBURSE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-DISBURSE-AMT
           END-IF
           ADD STLM-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY LOAN TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3500-ACCUM-LOAN-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-LOAN-TYPE-CD
                   ADD STLT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-LOAN-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SUBSIDY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CAPINT AMT.
      *----------------------------------------------------------------*
       3510-PRORATE-CAPINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-CAPINT-AMT ROUNDED =
                   STLM-SUBSIDY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-CAPINT-AMT
           END-IF
           ADD STLM-CAPINT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = STLM-DISBURSE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = STLM-DISBURSE-AMT - (WS-WORK-AMT * 2)
           COMPUTE STLM-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-LAST-PAY.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3540-FLAG-ITEM.
           IF WS-WORK-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3550-COUNT-CYCLE.
           IF STLM-CYCLE-CNT < 9999999
               ADD 1 TO STLM-CYCLE-CNT
           END-IF
           IF STLM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO STLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3560-COUNT-ITEM.
           IF STLM-ITEM-CNT < 9999999
               ADD 1 TO STLM-ITEM-CNT
           END-IF
           IF STLM-ITEM-CNT NOT < 6
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR LOAN TYPE CD:
      * CS -> DV WHEN LAST ACT DATE IS PAST
      * DV -> JL WHEN LAST ACT DATE IS PAST
      * JL -> NE WHEN LAST ACT DATE IS PAST
      * NE -> SH WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-LOAN-TYPE.
           EVALUATE STLM-LOAN-TYPE-CD
               WHEN 'CS'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DV' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DV'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JL'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NE'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO STLM-LOAN-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-STATUS.
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
      * PRORATE SUBSIDY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3590-PRORATE-SUBSIDY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-SUBSIDY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SUBSIDY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PRINCIPAL YTD.
      *----------------------------------------------------------------*
       3610-PRORATE-PRINCIPAL.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-PRINCIPAL-YTD ROUNDED =
                   STLM-SUBSIDY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-PRINCIPAL-YTD
           END-IF
           ADD STLM-PRINCIPAL-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E683 ADDR REQUIRED
      * E286 DISBURSE AMT NEGATIVE
      * E196 DISBURSE AMT OVER 10000
      * E479 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3620-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-ADDR = SPACES
                   MOVE 'E683' TO WS-REJECT-CODE
               WHEN STLT-DISBURSE-AMT < 0
                   MOVE 'E286' TO WS-REJECT-CODE
               WHEN STLT-DISBURSE-AMT > 10000.00
                   MOVE 'E196' TO WS-REJECT-CODE
               WHEN STLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E479' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3630-CHECK-MATURITY.
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
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3640-LIMIT-PAYMENT.
           IF STLM-PAYMENT-YTD < 50.00
               MOVE 50.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-PAYMENT-YTD > 50000.00
               MOVE 50000.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-LAST-STMT.
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
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3660-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-CYCLE-CD
                   ADD STLM-CAPINT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-CAPINT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3670-AGE-LAST-MAINT.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3680-CHECK-POST.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO STLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3690-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3700-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLT-TRAN-CD
                   ADD STLT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CAPINT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       3710-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLT-DISBURSE-AMT ROUNDED =
                   STLM-CAPINT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLT-DISBURSE-AMT
           END-IF
           ADD STLT-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3720-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 5
               MOVE 'Y' TO STLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT TIER TIER FOR THE DEFERINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3730-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-TIER-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF STLM-DEFERINT-AMT NOT < WS-DEFERINT-TIER-LOW(WS-TX)
                   MOVE WS-DEFERINT-TIER-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3740-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF STLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLT-EFF-DATE TO DTU-DATE-1
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
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3750-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3760-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * STLM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3770-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-CYCLE-CD
                   ADD STLM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SUBSIDY AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3780-FLAG-SUBSIDY.
           IF STLM-SUBSIDY-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLM-SUBSIDY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE BAND TIER FOR THE PRINCIPAL AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3790-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-BAND-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF STLM-PRINCIPAL-AMT NOT < WS-DISBURSE-BAND-LOW(WS-TX)
                   MOVE WS-DISBURSE-BAND-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3800-ACCRUE-DISBURSE.
           IF STLM-DISBURSE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-DISBURSE-AMT * STLM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO STLM-SUBSIDY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3810-LIMIT-DISBURSE.
           IF STLM-DISBURSE-YTD < 25.00
               MOVE 25.00 TO STLM-DISBURSE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-DISBURSE-YTD > 5000.00
               MOVE 5000.00 TO STLM-DISBURSE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-STATUS.
           EVALUATE STLM-STATUS-CD
               WHEN 'HY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RK'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3830-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO STLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE DEFERINT AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3840-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN STLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO STLT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   COMPUTE STLT-PAYMENT-AMT ROUNDED = STLM-DEFERINT-AMT
                       * STLM-PENALTY-PCT / 200
               WHEN STLT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO STLT-PAYMENT-AMT
               WHEN STLT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO STLT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE STLT-PAYMENT-AMT ROUNDED = STLM-DEFERINT-AMT
                       * STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLT-PAYMENT-AMT > 0 AND STLT-PAYMENT-AMT < 1.00
               MOVE 1.00 TO STLT-PAYMENT-AMT
           END-IF
           IF STLT-PAYMENT-AMT > 50.00
               MOVE 50.00 TO STLT-PAYMENT-AMT
           END-IF
           ADD STLT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CAPINT AMT.
      *----------------------------------------------------------------*
       3850-PRORATE-CAPINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLT-CAPINT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLT-CAPINT-AMT
           END-IF
           ADD STLT-CAPINT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3860-CHECK-CLOSE.
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
               MOVE 'Y' TO STLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEFERINT FEE ON THE DISBURSE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3870-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN STLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO STLM-DEFERINT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-SUSPEND-FLAG = 'Y'
                   COMPUTE STLM-DEFERINT-AMT ROUNDED = STLM-DISBURSE-YTD
                       * STLM-PENALTY-PCT / 200
               WHEN STLM-REGION-CD = 'C'
                   MOVE 12.00 TO STLM-DEFERINT-AMT
               WHEN STLM-REGION-CD = 'D'
                   MOVE 12.00 TO STLM-DEFERINT-AMT
               WHEN OTHER
                   COMPUTE STLM-DEFERINT-AMT ROUNDED = STLM-DISBURSE-YTD
                       * STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-DEFERINT-AMT > 0 AND STLM-DEFERINT-AMT < 5.00
               MOVE 5.00 TO STLM-DEFERINT-AMT
           END-IF
           IF STLM-DEFERINT-AMT > 250.00
               MOVE 250.00 TO STLM-DEFERINT-AMT
           END-IF
           ADD STLM-DEFERINT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE LOAN TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3880-DESCRIBE-LOAN-TYPE.
           EVALUATE STLM-LOAN-TYPE-CD
               WHEN 'CS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'NE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E919 REJECT CD REQUIRED
      * E252 CAPINT AMT NEGATIVE
      * E257 CAPINT AMT OVER 250000
      * E503 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3890-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-REJECT-CD = SPACES
                   MOVE 'E919' TO WS-REJECT-CODE
               WHEN STLT-CAPINT-AMT < 0
                   MOVE 'E252' TO WS-REJECT-CODE
               WHEN STLT-CAPINT-AMT > 250000.00
                   MOVE 'E257' TO WS-REJECT-CODE
               WHEN STLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E503' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3900-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLT-CHANNEL-CD
                   ADD STLM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3910-CHECK-MATURITY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO STLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SUBSIDY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CAPINT AMT.
      *----------------------------------------------------------------*
       3920-PRORATE-CAPINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-CAPINT-AMT ROUNDED =
                   STLM-SUBSIDY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-CAPINT-AMT
           END-IF
           ADD STLM-CAPINT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * CFJ -> KHC WHEN CLOSE DATE IS PAST
      * KHC -> SEU WHEN CLOSE DATE IS PAST
      * SEU -> XKA WHEN CLOSE DATE IS PAST
      * XKA -> CFJ WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3930-SET-CYCLE.
           EVALUATE STLM-CYCLE-CD
               WHEN 'CFJ'
                   IF STLM-CLOSE-DATE > 0 AND STLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KHC' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KHC'
                   IF STLM-CLOSE-DATE > 0 AND STLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SEU' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SEU'
                   IF STLM-CLOSE-DATE > 0 AND STLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XKA' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XKA'
                   IF STLM-CLOSE-DATE > 0 AND STLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CFJ' TO STLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E058 REF NO REQUIRED
      * E041 CAPINT AMT NEGATIVE
      * E829 CAPINT AMT OVER 25000
      * E944 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3940-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-REF-NO = SPACES
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN STLM-CAPINT-AMT < 0
                   MOVE 'E041' TO WS-REJECT-CODE
               WHEN STLM-CAPINT-AMT > 25000.00
                   MOVE 'E829' TO WS-REJECT-CODE
               WHEN STLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E944' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3950-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-STATUS-CD
                   ADD STLT-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLT-PRINCIPAL-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> D WHEN LAST ACT DATE IS PAST
      * D -> T WHEN LAST ACT DATE IS PAST
      * T -> U WHEN LAST ACT DATE IS PAST
      * U -> W WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF STLM-LAST-ACT-DATE > 0 AND STLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT YTD FEE ON THE DISBURSE AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3970-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO STLM-CAPINT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-HOLD-FLAG = 'Y'
                   COMPUTE STLM-CAPINT-YTD ROUNDED = STLM-DISBURSE-AMT *
                       STLM-PENALTY-PCT / 200
               WHEN STLM-LOAN-TYPE-CD = 'CS'
                   MOVE 7.00 TO STLM-CAPINT-YTD
               WHEN STLM-LOAN-TYPE-CD = 'DV'
                   MOVE 12.00 TO STLM-CAPINT-YTD
               WHEN OTHER
                   COMPUTE STLM-CAPINT-YTD ROUNDED = STLM-DISBURSE-AMT *
                       STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-CAPINT-YTD > 0 AND STLM-CAPINT-YTD < 2.00
               MOVE 2.00 TO STLM-CAPINT-YTD
           END-IF
           IF STLM-CAPINT-YTD > 100.00
               MOVE 100.00 TO STLM-CAPINT-YTD
           END-IF
           ADD STLM-CAPINT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-SEQ.
           IF STLT-SEQ < 9999999
               ADD 1 TO STLT-SEQ
           END-IF
           IF STLT-SEQ NOT < 3
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3990-LIMIT-PAYMENT.
           IF STLM-PAYMENT-YTD < 10.00
               MOVE 10.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-PAYMENT-YTD > 50000.00
               MOVE 50000.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CAPINT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4000-FLAG-CAPINT.
           IF STLT-CAPINT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLT-CAPINT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E360 CUST ID REQUIRED
      * E674 WORK AMT NEGATIVE
      * E899 WORK AMT OVER 250000
      * E313 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-CUST-ID = SPACES
                   MOVE 'E360' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E674' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 250000.00
                   MOVE 'E899' TO WS-REJECT-CODE
               WHEN STLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E313' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CAPINT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4020-SPLIT-CAPINT.
           COMPUTE WS-WORK-AMT = STLT-CAPINT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = STLT-CAPINT-AMT - (WS-WORK-AMT * 12)
           COMPUTE STLM-SUBSIDY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4030-ACCUM-REGION.
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
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4040-AGE-LAST-ACT.
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
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLT-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4050-DESCRIBE-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DB -> PY WHEN LAST PAY DATE IS PAST
      * PY -> CP WHEN LAST PAY DATE IS PAST
      * CP -> DF WHEN LAST PAY DATE IS PAST
      * DF -> FB WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4060-SET-TRAN.
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
      * SELECT THE DISBURSE BAND TIER FOR THE SUBSIDY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-BAND-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF STLM-SUBSIDY-AMT NOT < WS-DISBURSE-BAND-LOW(WS-TX)
                   MOVE WS-DISBURSE-BAND-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4080-DESCRIBE-CYCLE.
           EVALUATE STLM-CYCLE-CD
               WHEN 'CFJ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KHC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SEU'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XKA'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
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
                   MOVE SLCAL-SUBSIDY-YTD
                     TO STLM-SUBSIDY-YTD
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
                   MOVE SLLIM-PRINCIPAL-YTD
                     TO STLM-PRINCIPAL-YTD
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
           DISPLAY 'SLLANAL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SLLANAL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
