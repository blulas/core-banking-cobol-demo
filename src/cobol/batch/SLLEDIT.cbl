      *================================================================*
      * PROGRAM  : SLLEDIT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : STUDENT LOANS - INPUT EDIT. READS THE STL           *
      *            TRANSACTIONS, UPDATES THE STL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. GUARANTEED STUDENT LOAN         *
      *            DISBURSEMENT, DEFERMENT AND REPAYMENT               *
      * INPUT    : STLMST (KSDS), STLTRN, BANKCTL                      *
      * OUTPUT   : STLOUT, SLLEDRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, SLVAL                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1987-07-02 SKN  ORIGINAL                                       *
      * 1996-03-07 DPS  DISBURSE ROUNDING - HALF UP CHG2003            *
      * 1997-02-21 RJT  DEFERINT NOTICE TEXT PER LEGAL INC1556         *
      * 1999-04-12 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-03-01 SKN  PRINCIPAL NOTICE TEXT PER LEGAL PRB3608        *
      * 2002-06-06 BHS  DISBURSE RERUN PROTECTION CHG9265              *
      * 2003-01-25 SKN  SUBSIDY RERUN PROTECTION PRB4478               *
      * 2003-12-13 BHS  PERFORMANCE - PAYMENT READ AHEAD REQ3655       *
      * 2004-12-22 PQR  CAPINT CUTOFF MOVED REQ7962                    *
      * 2004-09-26 JLM  PAYMENT RERUN PROTECTION CHG5767               *
      * 2005-10-06 GAF  SUBSIDY FEE SCHEDULE UPDATE INC7408            *
      * 2006-03-12 CRP  PRINCIPAL STATUS CODES ADDED INC0291           *
      * 2006-04-09 MKL  CAPINT ROUNDING - HALF UP PRB3742              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLLEDIT.
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
           SELECT REPORT-FILE ASSIGN TO "SLLEDRPT"
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
                                           VALUE 16.000000.
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
      * WS-DEFERINT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DEFERINT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000828000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000465500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000074000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002800000000352000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003050000000653500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003150000000548500'.
       01  WS-DEFERINT-TIER-TBL REDEFINES WS-DEFERINT-TIER-VALUES.
           05  WS-DEFERINT-TIER-ROW OCCURS 6 TIMES.
               10  WS-DEFERINT-TIER-LOW    PIC 9(09)V99.
               10  WS-DEFERINT-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-DISBURSE-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISBURSE-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000718000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000641500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002600000000214500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000021000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003150000000105000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005650000000531500'.
       01  WS-DISBURSE-GRID-TBL REDEFINES WS-DISBURSE-GRID-VALUES.
           05  WS-DISBURSE-GRID-ROW OCCURS 6 TIMES.
               10  WS-DISBURSE-GRID-LOW    PIC 9(09)V99.
               10  WS-DISBURSE-GRID-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CAPINT-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CAPINT-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000808000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000840000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000435500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006050000000514500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006300000000292500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011300000000493000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011400000000824000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016400000000421000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021400000000235500'.
       01  WS-CAPINT-SCHED-TBL REDEFINES WS-CAPINT-SCHED-VALUES.
           05  WS-CAPINT-SCHED-ROW OCCURS 9 TIMES.
               10  WS-CAPINT-SCHED-LOW     PIC 9(09)V99.
               10  WS-CAPINT-SCHED-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  SLLEDIT  STUDENT LOANS'.
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
               PERFORM 3000-CHECK-LAST-PAY
               PERFORM 3010-EDIT-DESC
               PERFORM 3020-CHECK-LAST-PAY
               PERFORM 3030-SELECT-PENALTY-PCT
               PERFORM 3040-COUNT-SEQ
               PERFORM 3050-SELECT-PENALTY-PCT
               PERFORM 3060-SPLIT-DISBURSE
               PERFORM 3070-SPLIT-DEFERINT
               PERFORM 3080-ACCUM-DEFER-CODE
               PERFORM 3090-EDIT-REF-NO
               PERFORM 3100-SPLIT-DISBURSE
               PERFORM 3110-FLAG-PAYMENT
               PERFORM 3120-COUNT-CYCLE
               PERFORM 3130-LIMIT-PAYMENT
               PERFORM 3140-EDIT-OPERATOR
               PERFORM 3150-LIMIT-DISBURSE
               PERFORM 3160-AGE-EFF
               PERFORM 3170-FLAG-PAYMENT
               PERFORM 3180-DESCRIBE-CYCLE
               PERFORM 3190-CHECK-OPEN
               PERFORM 3200-EDIT-MEMO
               PERFORM 3210-CHECK-MATURITY
               PERFORM 3220-SELECT-DISCOUNT
               PERFORM 3230-ACCUM-CHANNEL
               PERFORM 3240-EDIT-OPERATOR
               PERFORM 3250-EDIT-ADDR
               PERFORM 3260-LIMIT-PRINCIPAL
               PERFORM 3270-SELECT-FEE-PCT
               PERFORM 3280-PRORATE-DISBURSE
               PERFORM 3290-ASSESS-SUSPEND
               PERFORM 3300-AGE-POST
               PERFORM 3310-CHECK-OPEN
               PERFORM 3320-SPLIT-CAPINT
               PERFORM 3330-AGE-CLOSE
               PERFORM 3340-ACCUM-STATUS
               PERFORM 3350-COUNT-CYCLE
               PERFORM 3360-LIMIT-ITEM
               PERFORM 3370-ASSESS-OVERRIDE
               PERFORM 3380-SELECT-PENALTY-PCT
               PERFORM 3390-COUNT-CYCLE
               PERFORM 3400-EDIT-OPERATOR
               PERFORM 3410-EDIT-OPERATOR
               PERFORM 3420-SPLIT-DEFERINT
               PERFORM 3430-ACCUM-SOURCE
               PERFORM 3440-AGE-EFF
               PERFORM 3450-ACCRUE-CAPINT
               PERFORM 3460-PRORATE-CAPINT
               PERFORM 3470-EDIT-MEMO
               PERFORM 3480-SELECT-FEE-PCT
               PERFORM 3490-SELECT-FEE-PCT
               PERFORM 3500-PRORATE-SUBSIDY
               PERFORM 3510-AGE-OPEN
               PERFORM 3520-PRORATE-PAYMENT
               PERFORM 3530-LIMIT-DEFERINT
               PERFORM 3540-ACCUM-DEFER-CODE
               PERFORM 3550-SPLIT-DISBURSE
               PERFORM 3560-CHECK-POST
               PERFORM 3570-SPLIT-CAPINT
               PERFORM 3580-ACCRUE-CAPINT
               PERFORM 3590-ASSESS-OVERRIDE
               PERFORM 3600-FLAG-PAYMENT
               PERFORM 3610-COUNT-PAYMENT
               PERFORM 3620-SELECT-FEE-PCT
               PERFORM 3630-COUNT-ITEM
               PERFORM 3640-FLAG-DISBURSE
               PERFORM 3650-SPLIT-PRINCIPAL
               PERFORM 3660-COUNT-CYCLE
               PERFORM 3670-ACCRUE-PRINCIPAL
               PERFORM 3680-SELECT-FEE-PCT
               PERFORM 3690-SELECT-FEE-PCT
               PERFORM 3700-COUNT-PAYMENT
               PERFORM 3710-AGE-OPEN
               PERFORM 3720-EDIT-REF-NO
               PERFORM 3730-ASSESS-HOLD
               PERFORM 3740-ACCUM-SOURCE
               PERFORM 3750-DESCRIBE-CHANNEL
               PERFORM 3760-ACCRUE-DISBURSE
               PERFORM 3770-ASSESS-REVIEW
               PERFORM 3780-SELECT-FEE-PCT
               PERFORM 3790-SET-DEFER-CODE
               PERFORM 3800-CHECK-CLOSE
               PERFORM 3810-ACCUM-SOURCE
               PERFORM 3820-AGE-LAST-MAINT
               PERFORM 3830-LIMIT-PRINCIPAL
               PERFORM 3840-DESCRIBE-TRAN
               PERFORM 3850-AGE-MATURITY
               PERFORM 3860-ASSESS-OVERRIDE
               PERFORM 3870-SET-STATUS
               PERFORM 3880-SPLIT-SUBSIDY
               PERFORM 3890-DESCRIBE-CHANNEL
               PERFORM 3900-AGE-EXPIRY
               PERFORM 3910-SET-SOURCE
               PERFORM 3920-SET-CHANNEL
               PERFORM 3930-DESCRIBE-TRAN
               PERFORM 3940-DESCRIBE-LOAN-TYPE
               PERFORM 3950-PRORATE-DISBURSE
               PERFORM 3960-PRORATE-DISBURSE
               PERFORM 3970-DESCRIBE-TRAN
               PERFORM 3980-LIMIT-DEFERINT
               PERFORM 3990-AGE-NEXT-REVIEW
               PERFORM 4000-SET-CHANNEL
               PERFORM 4010-ACCUM-LOAN-TYPE
               PERFORM 4020-COUNT-EXCEPT
               PERFORM 4030-PRORATE-DISBURSE
               PERFORM 4040-PRORATE-DEFERINT
               PERFORM 4050-COUNT-SEQ
               PERFORM 4060-SET-CHANNEL
               PERFORM 4070-COUNT-ITEM
               PERFORM 4080-COUNT-ITEM
               PERFORM 4090-FLAG-ITEM
               PERFORM 4100-SET-REGION
               PERFORM 4110-SELECT-FEE-PCT
               PERFORM 4120-PRORATE-DEFERINT
               PERFORM 4130-SELECT-FEE-PCT
               PERFORM 7020-CALL-SLVAL
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
      * DAYS SINCE LAST PAY DATE. MORE THAN 120 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3000-CHECK-LAST-PAY.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO STLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E749 DESC REQUIRED
      * E641 DEFERINT AMT NEGATIVE
      * E546 DEFERINT AMT OVER 5000
      * E831 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3010-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-DESC = SPACES
                   MOVE 'E749' TO WS-REJECT-CODE
               WHEN STLM-DEFERINT-AMT < 0
                   MOVE 'E641' TO WS-REJECT-CODE
               WHEN STLM-DEFERINT-AMT > 5000.00
                   MOVE 'E546' TO WS-REJECT-CODE
               WHEN STLT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E831' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3020-CHECK-LAST-PAY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO STLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CAPINT SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CAPINT-SCHED-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-CAPINT-SCHED-LOW(WS-TX)
                   MOVE WS-CAPINT-SCHED-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3040-COUNT-SEQ.
           IF STLT-SEQ < 9999999
               ADD 1 TO STLT-SEQ
           END-IF
           IF STLT-SEQ NOT < 5
               MOVE 'Y' TO STLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CAPINT SCHED TIER FOR THE CAPINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3050-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CAPINT-SCHED-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF STLT-CAPINT-AMT NOT < WS-CAPINT-SCHED-LOW(WS-TX)
                   MOVE WS-CAPINT-SCHED-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3060-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = STLT-DISBURSE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = STLT-DISBURSE-AMT - (WS-WORK-AMT * 4)
           COMPUTE STLM-DEFERINT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEFERINT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3070-SPLIT-DEFERINT.
           COMPUTE WS-WORK-AMT = STLM-DEFERINT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = STLM-DEFERINT-AMT - (WS-WORK-AMT * 4)
           COMPUTE STLM-DISBURSE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY DEFER CODE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3080-ACCUM-DEFER-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-DEFER-CODE-CD
                   ADD STLM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-DEFER-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E045 REF NO REQUIRED
      * E035 PAYMENT AMT NEGATIVE
      * E624 PAYMENT AMT OVER 25000
      * E188 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-REF-NO = SPACES
                   MOVE 'E045' TO WS-REJECT-CODE
               WHEN STLT-PAYMENT-AMT < 0
                   MOVE 'E035' TO WS-REJECT-CODE
               WHEN STLT-PAYMENT-AMT > 25000.00
                   MOVE 'E624' TO WS-REJECT-CODE
               WHEN STLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E188' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3100-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = STLM-DISBURSE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = STLM-DISBURSE-YTD - (WS-WORK-AMT * 4)
           COMPUTE STLM-CAPINT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3110-FLAG-PAYMENT.
           IF STLT-PAYMENT-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3120-COUNT-CYCLE.
           IF STLM-CYCLE-CNT < 9999999
               ADD 1 TO STLM-CYCLE-CNT
           END-IF
           IF STLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3130-LIMIT-PAYMENT.
           IF STLM-PAYMENT-YTD < 25.00
               MOVE 25.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-PAYMENT-YTD > 500.00
               MOVE 500.00 TO STLM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E366 OPERATOR REQUIRED
      * E198 PAYMENT AMT NEGATIVE
      * E968 PAYMENT AMT OVER 1000000
      * E553 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-OPERATOR = SPACES
                   MOVE 'E366' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-AMT < 0
                   MOVE 'E198' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-AMT > 1000000.00
                   MOVE 'E968' TO WS-REJECT-CODE
               WHEN STLM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E553' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3150-LIMIT-DISBURSE.
           IF STLT-DISBURSE-AMT < 100.00
               MOVE 100.00 TO STLT-DISBURSE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLT-DISBURSE-AMT > 50000.00
               MOVE 50000.00 TO STLT-DISBURSE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-EFF.
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
                   ADD STLM-DISBURSE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-DISBURSE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD STLM-DISBURSE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLM-DISBURSE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-DISBURSE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3170-FLAG-PAYMENT.
           IF STLT-PAYMENT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE STLT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-CYCLE.
           EVALUATE STLM-CYCLE-CD
               WHEN 'CFJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KHC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SEU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XKA'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3190-CHECK-OPEN.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO STLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E699 MEMO REQUIRED
      * E248 DEFERINT AMT NEGATIVE
      * E840 DEFERINT AMT OVER 10000
      * E455 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3200-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-MEMO = SPACES
                   MOVE 'E699' TO WS-REJECT-CODE
               WHEN STLM-DEFERINT-AMT < 0
                   MOVE 'E248' TO WS-REJECT-CODE
               WHEN STLM-DEFERINT-AMT > 10000.00
                   MOVE 'E840' TO WS-REJECT-CODE
               WHEN STLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E455' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3210-CHECK-MATURITY.
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
               MOVE 'Y' TO STLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CAPINT SCHED TIER FOR THE PRINCIPAL YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3220-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-CAPINT-SCHED-RATE(1) TO STLM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF STLM-PRINCIPAL-YTD NOT < WS-CAPINT-SCHED-LOW(WS-TX)
                   MOVE WS-CAPINT-SCHED-RATE(WS-TX) TO STLM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3230-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLT-CHANNEL-CD
                   ADD STLM-CAPINT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-CAPINT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E517 OPERATOR REQUIRED
      * E614 PRINCIPAL AMT NEGATIVE
      * E506 PRINCIPAL AMT OVER 5000
      * E909 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-OPERATOR = SPACES
                   MOVE 'E517' TO WS-REJECT-CODE
               WHEN STLT-PRINCIPAL-AMT < 0
                   MOVE 'E614' TO WS-REJECT-CODE
               WHEN STLT-PRINCIPAL-AMT > 5000.00
                   MOVE 'E506' TO WS-REJECT-CODE
               WHEN STLM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E909' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E555 ADDR REQUIRED
      * E867 PAYMENT AMT NEGATIVE
      * E789 PAYMENT AMT OVER 25000
      * E890 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-ADDR = SPACES
                   MOVE 'E555' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-AMT < 0
                   MOVE 'E867' TO WS-REJECT-CODE
               WHEN STLM-PAYMENT-AMT > 25000.00
                   MOVE 'E789' TO WS-REJECT-CODE
               WHEN STLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E890' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3260-LIMIT-PRINCIPAL.
           IF STLM-PRINCIPAL-AMT < 0.00
               MOVE 0.00 TO STLM-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-PRINCIPAL-AMT > 50000.00
               MOVE 50000.00 TO STLM-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CAPINT SCHED TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CAPINT-SCHED-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF STLT-PAYMENT-AMT NOT < WS-CAPINT-SCHED-LOW(WS-TX)
                   MOVE WS-CAPINT-SCHED-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PRINCIPAL AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       3280-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DISBURSE-AMT ROUNDED =
                   STLT-PRINCIPAL-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-DISBURSE-AMT
           END-IF
           ADD STLM-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT FEE ON THE CAPINT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3290-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN STLM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO STLT-CAPINT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   COMPUTE STLT-CAPINT-AMT ROUNDED = STLM-CAPINT-AMT *
                       STLM-PENALTY-PCT / 200
               WHEN STLT-TRAN-CD = 'DB'
                   MOVE 10.00 TO STLT-CAPINT-AMT
               WHEN STLT-TRAN-CD = 'PY'
                   MOVE 7.00 TO STLT-CAPINT-AMT
               WHEN OTHER
                   COMPUTE STLT-CAPINT-AMT ROUNDED = STLM-CAPINT-AMT *
                       STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLT-CAPINT-AMT > 0 AND STLT-CAPINT-AMT < 10.00
               MOVE 10.00 TO STLT-CAPINT-AMT
           END-IF
           IF STLT-CAPINT-AMT > 35.00
               MOVE 35.00 TO STLT-CAPINT-AMT
           END-IF
           ADD STLT-CAPINT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-POST.
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
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-OPEN.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO STLM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CAPINT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3320-SPLIT-CAPINT.
           COMPUTE WS-WORK-AMT = STLM-CAPINT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = STLM-CAPINT-AMT - (WS-WORK-AMT * 4)
           COMPUTE STLM-CAPINT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3330-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF STLM-CLOSE-DATE > 0 AND STLM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-PRINCIPAL-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE SUBSIDY YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3340-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-STATUS-CD
                   ADD STLM-SUBSIDY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-SUBSIDY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3350-COUNT-CYCLE.
           IF STLM-CYCLE-CNT < 9999999
               ADD 1 TO STLM-CYCLE-CNT
           END-IF
           IF STLM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO STLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3360-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PRINCIPAL FEE ON THE PAYMENT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3370-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN STLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO STLM-PRINCIPAL-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE STLM-PRINCIPAL-AMT ROUNDED = STLM-PAYMENT-AMT
                       * STLM-FEE-PCT / 200
               WHEN STLT-TRAN-CD = 'DB'
                   MOVE 12.00 TO STLM-PRINCIPAL-AMT
               WHEN STLT-TRAN-CD = 'PY'
                   MOVE 3.00 TO STLM-PRINCIPAL-AMT
               WHEN OTHER
                   COMPUTE STLM-PRINCIPAL-AMT ROUNDED = STLM-PAYMENT-AMT
                       * STLM-FEE-PCT / 100
           END-EVALUATE
           IF STLM-PRINCIPAL-AMT > 0 AND STLM-PRINCIPAL-AMT < 10.00
               MOVE 10.00 TO STLM-PRINCIPAL-AMT
           END-IF
           IF STLM-PRINCIPAL-AMT > 150.00
               MOVE 150.00 TO STLM-PRINCIPAL-AMT
           END-IF
           ADD STLM-PRINCIPAL-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE GRID TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-GRID-RATE(1) TO STLM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-DISBURSE-GRID-LOW(WS-TX)
                   MOVE WS-DISBURSE-GRID-RATE(WS-TX) TO STLM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3390-COUNT-CYCLE.
           IF STLM-CYCLE-CNT < 9999999
               ADD 1 TO STLM-CYCLE-CNT
           END-IF
           IF STLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO STLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E942 OPERATOR REQUIRED
      * E625 PRINCIPAL AMT NEGATIVE
      * E203 PRINCIPAL AMT OVER 50000
      * E585 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3400-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-OPERATOR = SPACES
                   MOVE 'E942' TO WS-REJECT-CODE
               WHEN STLT-PRINCIPAL-AMT < 0
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN STLT-PRINCIPAL-AMT > 50000.00
                   MOVE 'E203' TO WS-REJECT-CODE
               WHEN STLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E585' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E986 OPERATOR REQUIRED
      * E535 DISBURSE YTD NEGATIVE
      * E788 DISBURSE YTD OVER 99999
      * E344 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-OPERATOR = SPACES
                   MOVE 'E986' TO WS-REJECT-CODE
               WHEN STLM-DISBURSE-YTD < 0
                   MOVE 'E535' TO WS-REJECT-CODE
               WHEN STLM-DISBURSE-YTD > 99999.00
                   MOVE 'E788' TO WS-REJECT-CODE
               WHEN STLM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E344' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEFERINT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-DEFERINT.
           COMPUTE WS-WORK-AMT = STLM-DEFERINT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = STLM-DEFERINT-YTD - (WS-WORK-AMT * 3)
           COMPUTE STLM-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3430-ACCUM-SOURCE.
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
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-EFF.
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
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CAPINT AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3450-ACCRUE-CAPINT.
           IF STLM-CAPINT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-CAPINT-AMT * STLM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CAPINT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CAPINT YTD.
      *----------------------------------------------------------------*
       3460-PRORATE-CAPINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-CAPINT-YTD ROUNDED =
                   STLM-CAPINT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-CAPINT-YTD
           END-IF
           ADD STLM-CAPINT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E713 MEMO REQUIRED
      * E883 DEFERINT YTD NEGATIVE
      * E997 DEFERINT YTD OVER 250000
      * E921 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3470-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLT-MEMO = SPACES
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN STLM-DEFERINT-YTD < 0
                   MOVE 'E883' TO WS-REJECT-CODE
               WHEN STLM-DEFERINT-YTD > 250000.00
                   MOVE 'E997' TO WS-REJECT-CODE
               WHEN STLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E921' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT TIER TIER FOR THE CAPINT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3480-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-TIER-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF STLM-CAPINT-YTD NOT < WS-DEFERINT-TIER-LOW(WS-TX)
                   MOVE WS-DEFERINT-TIER-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CAPINT SCHED TIER FOR THE CAPINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3490-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CAPINT-SCHED-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF STLT-CAPINT-AMT NOT < WS-CAPINT-SCHED-LOW(WS-TX)
                   MOVE WS-CAPINT-SCHED-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SUBSIDY AMT.
      *----------------------------------------------------------------*
       3500-PRORATE-SUBSIDY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-SUBSIDY-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-SUBSIDY-AMT
           END-IF
           ADD STLM-SUBSIDY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3510-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-PRINCIPAL-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-PRINCIPAL-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD STLM-PRINCIPAL-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-PRINCIPAL-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-PRINCIPAL-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3520-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLT-PAYMENT-AMT ROUNDED =
                   STLT-DISBURSE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLT-PAYMENT-AMT
           END-IF
           ADD STLT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DEFERINT AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3530-LIMIT-DEFERINT.
           IF STLM-DEFERINT-AMT < 100.00
               MOVE 100.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-DEFERINT-AMT > 1000.00
               MOVE 1000.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY DEFER CODE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3540-ACCUM-DEFER-CODE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-DEFER-CODE-CD
                   ADD STLM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-DEFER-CODE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3550-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = STLT-DISBURSE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = STLT-DISBURSE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE STLM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3560-CHECK-POST.
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
               MOVE 'Y' TO STLM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CAPINT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3570-SPLIT-CAPINT.
           COMPUTE WS-WORK-AMT = STLM-CAPINT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = STLM-CAPINT-YTD - (WS-WORK-AMT * 3)
           COMPUTE STLM-SUBSIDY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CAPINT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3580-ACCRUE-CAPINT.
           IF STLT-CAPINT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLT-CAPINT-AMT * STLM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO STLM-CAPINT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SUBSIDY FEE ON THE DISBURSE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3590-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN STLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO STLM-SUBSIDY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLT-REVERSAL-FLAG = 'Y'
                   COMPUTE STLM-SUBSIDY-AMT ROUNDED = STLM-DISBURSE-AMT
                       * STLM-DISCOUNT / 200
               WHEN STLM-REGION-CD = 'C'
                   MOVE 3.00 TO STLM-SUBSIDY-AMT
               WHEN STLM-REGION-CD = 'D'
                   MOVE 20.00 TO STLM-SUBSIDY-AMT
               WHEN OTHER
                   COMPUTE STLM-SUBSIDY-AMT ROUNDED = STLM-DISBURSE-AMT
                       * STLM-DISCOUNT / 100
           END-EVALUATE
           IF STLM-SUBSIDY-AMT > 0 AND STLM-SUBSIDY-AMT < 10.00
               MOVE 10.00 TO STLM-SUBSIDY-AMT
           END-IF
           IF STLM-SUBSIDY-AMT > 25.00
               MOVE 25.00 TO STLM-SUBSIDY-AMT
           END-IF
           ADD STLM-SUBSIDY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3600-FLAG-PAYMENT.
           IF STLM-PAYMENT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3610-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO STLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE GRID TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3620-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-GRID-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF STLM-PAYMENT-AMT NOT < WS-DISBURSE-GRID-LOW(WS-TX)
                   MOVE WS-DISBURSE-GRID-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-ITEM.
           IF STLM-ITEM-CNT < 9999999
               ADD 1 TO STLM-ITEM-CNT
           END-IF
           IF STLM-ITEM-CNT NOT < 10
               MOVE 'Y' TO STLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3640-FLAG-DISBURSE.
           IF STLM-DISBURSE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE STLM-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PRINCIPAL YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3650-SPLIT-PRINCIPAL.
           COMPUTE WS-WORK-AMT = STLM-PRINCIPAL-YTD / 3
           COMPUTE WS-WORK-AMT-2 = STLM-PRINCIPAL-YTD - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-CYCLE.
           IF STLM-CYCLE-CNT < 9999999
               ADD 1 TO STLM-CYCLE-CNT
           END-IF
           IF STLM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PRINCIPAL YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3670-ACCRUE-PRINCIPAL.
           IF STLM-PRINCIPAL-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLM-PRINCIPAL-YTD * STLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO STLM-CAPINT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CAPINT SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CAPINT-SCHED-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT NOT < WS-CAPINT-SCHED-LOW(WS-TX)
                   MOVE WS-CAPINT-SCHED-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE GRID TIER FOR THE CAPINT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3690-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-GRID-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF STLM-CAPINT-YTD NOT < WS-DISBURSE-GRID-LOW(WS-TX)
                   MOVE WS-DISBURSE-GRID-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3700-COUNT-PAYMENT.
           IF STLM-PAYMENT-CNT < 9999999
               ADD 1 TO STLM-PAYMENT-CNT
           END-IF
           IF STLM-PAYMENT-CNT NOT < 3
               MOVE 'Y' TO STLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE STLM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E869 REF NO REQUIRED
      * E712 PRINCIPAL AMT NEGATIVE
      * E493 PRINCIPAL AMT OVER 10000
      * E569 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3720-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN STLM-REF-NO = SPACES
                   MOVE 'E869' TO WS-REJECT-CODE
               WHEN STLT-PRINCIPAL-AMT < 0
                   MOVE 'E712' TO WS-REJECT-CODE
               WHEN STLT-PRINCIPAL-AMT > 10000.00
                   MOVE 'E493' TO WS-REJECT-CODE
               WHEN STLM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E569' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CAPINT FEE ON THE PRINCIPAL AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3730-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN STLM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO STLM-CAPINT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-SUSPEND-FLAG = 'Y'
                   COMPUTE STLM-CAPINT-AMT ROUNDED = STLM-PRINCIPAL-AMT
                       * STLM-DISCOUNT / 200
               WHEN STLM-DEFER-CODE-CD = 'AH'
                   MOVE 3.00 TO STLM-CAPINT-AMT
               WHEN STLM-DEFER-CODE-CD = 'CZ'
                   MOVE 10.00 TO STLM-CAPINT-AMT
               WHEN OTHER
                   COMPUTE STLM-CAPINT-AMT ROUNDED = STLM-PRINCIPAL-AMT
                       * STLM-DISCOUNT / 100
           END-EVALUATE
           IF STLM-CAPINT-AMT > 0 AND STLM-CAPINT-AMT < 1.00
               MOVE 1.00 TO STLM-CAPINT-AMT
           END-IF
           IF STLM-CAPINT-AMT > 75.00
               MOVE 75.00 TO STLM-CAPINT-AMT
           END-IF
           ADD STLM-CAPINT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3740-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-SOURCE-CD
                   ADD STLM-DISBURSE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-DISBURSE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3750-DESCRIBE-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3760-ACCRUE-DISBURSE.
           IF STLT-DISBURSE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   STLT-DISBURSE-AMT * STLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO STLM-CAPINT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SUBSIDY AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3770-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN STLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = STLM-SUBSIDY-AMT *
                       STLM-PENALTY-PCT / 200
               WHEN STLM-REGION-CD = 'C'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN STLM-REGION-CD = 'D'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = STLM-SUBSIDY-AMT *
                       STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 250.00
               MOVE 250.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE DEFERINT TIER TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3780-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DEFERINT-TIER-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF WS-WORK-AMT-2 NOT < WS-DEFERINT-TIER-LOW(WS-TX)
                   MOVE WS-DEFERINT-TIER-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEFER CODE CD:
      * AH -> CZ WHEN OPEN DATE IS PAST
      * CZ -> GM WHEN OPEN DATE IS PAST
      * GM -> ND WHEN OPEN DATE IS PAST
      * ND -> UC WHEN OPEN DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-DEFER-CODE.
           EVALUATE STLM-DEFER-CODE-CD
               WHEN 'AH'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CZ' TO STLM-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CZ'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO STLM-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ND' TO STLM-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'ND'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UC' TO STLM-DEFER-CODE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-CLOSE.
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
               MOVE 'Y' TO STLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PRINCIPAL YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3810-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-SOURCE-CD
                   ADD STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-PRINCIPAL-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3820-AGE-LAST-MAINT.
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
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-CAPINT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRINCIPAL AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3830-LIMIT-PRINCIPAL.
           IF STLT-PRINCIPAL-AMT < 25.00
               MOVE 25.00 TO STLT-PRINCIPAL-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLT-PRINCIPAL-AMT > 2500.00
               MOVE 2500.00 TO STLT-PRINCIPAL-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-TRAN.
           EVALUATE STLT-TRAN-CD
               WHEN 'DB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'CP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3850-AGE-MATURITY.
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
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-SUBSIDY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE WORK AMT 2.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3860-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN STLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO STLM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN STLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE STLM-PAYMENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       STLM-PENALTY-PCT / 200
               WHEN STLM-CYCLE-CD = 'CFJ'
                   MOVE 5.00 TO STLM-PAYMENT-AMT
               WHEN STLM-CYCLE-CD = 'KHC'
                   MOVE 10.00 TO STLM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE STLM-PAYMENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       STLM-PENALTY-PCT / 100
           END-EVALUATE
           IF STLM-PAYMENT-AMT > 0 AND STLM-PAYMENT-AMT < 1.00
               MOVE 1.00 TO STLM-PAYMENT-AMT
           END-IF
           IF STLM-PAYMENT-AMT > 150.00
               MOVE 150.00 TO STLM-PAYMENT-AMT
           END-IF
           ADD STLM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * HY -> JZ WHEN LAST PAY DATE IS PAST
      * JZ -> RK WHEN LAST PAY DATE IS PAST
      * RK -> HY WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-STATUS.
           EVALUATE STLM-STATUS-CD
               WHEN 'HY'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JZ' TO STLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JZ'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RK' TO STLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RK'
                   IF STLM-LAST-PAY-DATE > 0 AND STLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HY' TO STLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SUBSIDY YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3880-SPLIT-SUBSIDY.
           COMPUTE WS-WORK-AMT = STLM-SUBSIDY-YTD / 4
           COMPUTE WS-WORK-AMT-2 = STLM-SUBSIDY-YTD - (WS-WORK-AMT * 4)
           COMPUTE STLM-CAPINT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3890-DESCRIBE-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3900-AGE-EXPIRY.
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
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD STLM-SUBSIDY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * AST -> BCK WHEN NEXT REVIEW DATE IS PAST
      * BCK -> CJG WHEN NEXT REVIEW DATE IS PAST
      * CJG -> DSJ WHEN NEXT REVIEW DATE IS PAST
      * DSJ -> JVY WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-SOURCE.
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
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFF DATE IS PAST
      * BAT -> MAL WHEN EFF DATE IS PAST
      * MAL -> TEL WHEN EFF DATE IS PAST
      * TEL -> ONL WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3920-SET-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF STLT-EFF-DATE > 0 AND STLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-TRAN.
           EVALUATE STLT-TRAN-CD
               WHEN 'DB'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'FB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE LOAN TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3940-DESCRIBE-LOAN-TYPE.
           EVALUATE STLM-LOAN-TYPE-CD
               WHEN 'CS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'DV'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'NE'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DEFERINT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       3950-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLT-DISBURSE-AMT ROUNDED =
                   STLM-DEFERINT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLT-DISBURSE-AMT
           END-IF
           ADD STLT-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE SUBSIDY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       3960-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DISBURSE-AMT ROUNDED =
                   STLM-SUBSIDY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-DISBURSE-AMT
           END-IF
           ADD STLM-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3970-DESCRIBE-TRAN.
           EVALUATE STLT-TRAN-CD
               WHEN 'DB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'CP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'DF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEFERINT AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3980-LIMIT-DEFERINT.
           IF STLM-DEFERINT-AMT < 0.00
               MOVE 0.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF STLM-DEFERINT-AMT > 500.00
               MOVE 500.00 TO STLM-DEFERINT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3990-AGE-NEXT-REVIEW.
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
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4000-SET-CHANNEL.
           EVALUATE STLT-CHANNEL-CD
               WHEN 'BRN'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF STLM-OPEN-DATE > 0 AND STLM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO STLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CAPINT YTD BY LOAN TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4010-ACCUM-LOAN-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = STLM-LOAN-TYPE-CD
                   ADD STLM-CAPINT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE STLM-LOAN-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE STLM-CAPINT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4020-COUNT-EXCEPT.
           IF STLM-EXCEPT-CNT < 9999999
               ADD 1 TO STLM-EXCEPT-CNT
           END-IF
           IF STLM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO STLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEFERINT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       4030-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DISBURSE-AMT ROUNDED =
                   STLM-DEFERINT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-DISBURSE-AMT
           END-IF
           ADD STLM-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DEFERINT AMT.
      *----------------------------------------------------------------*
       4040-PRORATE-DEFERINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DEFERINT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO STLM-DEFERINT-AMT
           END-IF
           ADD STLM-DEFERINT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4050-COUNT-SEQ.
           IF STLT-SEQ < 9999999
               ADD 1 TO STLT-SEQ
           END-IF
           IF STLT-SEQ NOT < 10
               MOVE 'Y' TO STLM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4060-SET-CHANNEL.
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
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4070-COUNT-ITEM.
           IF STLM-ITEM-CNT < 9999999
               ADD 1 TO STLM-ITEM-CNT
           END-IF
           IF STLM-ITEM-CNT NOT < 3
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4080-COUNT-ITEM.
           IF STLM-ITEM-CNT < 9999999
               ADD 1 TO STLM-ITEM-CNT
           END-IF
           IF STLM-ITEM-CNT NOT < 3
               MOVE 'Y' TO STLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4090-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * C -> D WHEN CLOSE DATE IS PAST
      * D -> T WHEN CLOSE DATE IS PAST
      * T -> U WHEN CLOSE DATE IS PAST
      * U -> W WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4100-SET-REGION.
           EVALUATE STLM-REGION-CD
               WHEN 'C'
                   IF STLM-CLOSE-DATE > 0 AND STLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF STLM-CLOSE-DATE > 0 AND STLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF STLM-CLOSE-DATE > 0 AND STLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF STLM-CLOSE-DATE > 0 AND STLM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO STLM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE GRID TIER FOR THE CAPINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4110-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-GRID-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF STLT-CAPINT-AMT NOT < WS-DISBURSE-GRID-LOW(WS-TX)
                   MOVE WS-DISBURSE-GRID-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEFERINT YTD.
      *----------------------------------------------------------------*
       4120-PRORATE-DEFERINT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE STLM-DEFERINT-YTD ROUNDED =
                   STLT-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO STLM-DEFERINT-YTD
           END-IF
           ADD STLM-DEFERINT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CAPINT SCHED TIER FOR THE CAPINT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4130-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CAPINT-SCHED-RATE(1) TO STLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF STLM-CAPINT-AMT NOT < WS-CAPINT-SCHED-LOW(WS-TX)
                   MOVE WS-CAPINT-SCHED-RATE(WS-TX) TO STLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF STLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO STLM-FEE-PCT
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
                   MOVE SLVAL-SUBSIDY-YTD
                     TO STLM-SUBSIDY-YTD
                   MOVE SLVAL-CAPINT-AMT
                     TO STLM-CAPINT-AMT
                   MOVE SLVAL-DISBURSE-YTD
                     TO STLM-DISBURSE-YTD
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
           DISPLAY 'SLLEDIT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'SLLEDIT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
