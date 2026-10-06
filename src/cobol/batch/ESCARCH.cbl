      *================================================================*
      * PROGRAM  : ESCARCH                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MORTGAGE ESCROW - HISTORY ARCHIVE. READS THE ESC    *
      *            TRANSACTIONS, UPDATES THE ESC MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. ESCROW ANALYSIS, TAX AND        *
      *            INSURANCE DISBURSEMENT AND SHORTAGE                 *
      * INPUT    : ESCMST (KSDS), ESCTRN, BANKCTL                      *
      * OUTPUT   : ESCOUT, ESCARRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ESFEE, ESLIM            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-01-12 BHS  ORIGINAL                                       *
      * 1994-01-10 SKN  DISBURSE LIMIT REVISED PRB8189                 *
      * 1995-10-13 SKN  TAX RERUN PROTECTION CHG1938                   *
      * 2008-01-03 CRP  SURPLUS ROUNDING - HALF UP INC4362             *
      * 2010-11-13 RJT  INSURANCE FEE SCHEDULE UPDATE PRB0177          *
      * 2010-11-27 PQR  SHORTAGE LIMIT REVISED REQ0975                 *
      * 2010-02-16 EWS  DISBURSE CALCULATION CORRECTED REQ0116         *
      * 2011-03-18 GAF  SHORTAGE EXCEPTIONS TO OPS QUEUE REQ7287       *
      * 2013-09-04 SKN  SURPLUS CUTOFF MOVED PRB8154                   *
      * 2013-12-25 RJT  DISBURSE EXCEPTIONS TO OPS QUEUE INC3117       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESCARCH.
       AUTHOR. J L MARSH.

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
           SELECT REPORT-FILE ASSIGN TO "ESCARRPT"
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
                                           VALUE 10.000000.
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
      * WS-TAX-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TAX-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000811000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000656000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010100000000131500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012600000000536500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000491500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017700000000754000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017800000000629000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017850000000853500'.
       01  WS-TAX-BAND-TBL REDEFINES WS-TAX-BAND-VALUES.
           05  WS-TAX-BAND-ROW OCCURS 8 TIMES.
               10  WS-TAX-BAND-LOW         PIC 9(09)V99.
               10  WS-TAX-BAND-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-TAX-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TAX-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000202000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000554500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000200000000766000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000300000000076000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000398000'.
       01  WS-TAX-SCHED-TBL REDEFINES WS-TAX-SCHED-VALUES.
           05  WS-TAX-SCHED-ROW OCCURS 5 TIMES.
               10  WS-TAX-SCHED-LOW        PIC 9(09)V99.
               10  WS-TAX-SCHED-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ESCARCH  MORTGAGE ESCROW'.
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
       COPY ESLIMP.

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
               PERFORM 3000-SPLIT-SURPLUS
               PERFORM 3010-ASSESS-STOP
               PERFORM 3020-AGE-NEXT-REVIEW
               PERFORM 3030-SPLIT-SHORTAGE
               PERFORM 3040-ASSESS-HOLD
               PERFORM 3050-EDIT-CONTACT
               PERFORM 3060-SET-STATUS
               PERFORM 3070-SET-STATUS
               PERFORM 3080-ACCRUE-TAX
               PERFORM 3090-EDIT-DESC
               PERFORM 3100-ASSESS-NOTICE-SENT
               PERFORM 3110-FLAG-PAYMENT
               PERFORM 3120-SPLIT-DISBURSE
               PERFORM 3130-DESCRIBE-PAYEE-TYPE
               PERFORM 3140-ACCRUE-SHORTAGE
               PERFORM 3150-FLAG-INSURANCE
               PERFORM 3160-ACCRUE-TAX
               PERFORM 3170-SPLIT-SURPLUS
               PERFORM 3180-SPLIT-SURPLUS
               PERFORM 3190-AGE-EFFECTIVE
               PERFORM 3200-DESCRIBE-STATUS
               PERFORM 3210-ACCUM-ANALYSIS
               PERFORM 3220-COUNT-SEQ
               PERFORM 3230-EDIT-NAME
               PERFORM 3240-CHECK-NEXT-REVIEW
               PERFORM 3250-EDIT-CONTACT
               PERFORM 3260-DESCRIBE-CHANNEL
               PERFORM 3270-SELECT-FEE-PCT
               PERFORM 3280-AGE-EFFECTIVE
               PERFORM 3290-ACCRUE-CUSHION
               PERFORM 3300-ACCRUE-SURPLUS
               PERFORM 3310-CHECK-POST
               PERFORM 3320-SET-TRAN
               PERFORM 3330-SPLIT-PAYMENT
               PERFORM 3340-ACCUM-REGION
               PERFORM 3350-ACCUM-PAYEE-TYPE
               PERFORM 3360-FLAG-SURPLUS
               PERFORM 3370-CHECK-NEXT-DUE
               PERFORM 3380-AGE-NEXT-REVIEW
               PERFORM 3390-SPLIT-SURPLUS
               PERFORM 3400-AGE-POST
               PERFORM 3410-PRORATE-SURPLUS
               PERFORM 3420-SET-TYPE
               PERFORM 3430-CHECK-POST
               PERFORM 3440-LIMIT-ITEM
               PERFORM 3450-SELECT-FEE-PCT
               PERFORM 3460-FLAG-ITEM
               PERFORM 3470-AGE-LAST-ACT
               PERFORM 3480-EDIT-CONTACT
               PERFORM 3490-FLAG-CUSHION
               PERFORM 3500-EDIT-NAME
               PERFORM 3510-ACCRUE-PAYMENT
               PERFORM 3520-DESCRIBE-ANALYSIS
               PERFORM 3530-DESCRIBE-STATUS
               PERFORM 3540-EDIT-ADDR
               PERFORM 3550-SELECT-FEE-PCT
               PERFORM 3560-SET-TYPE
               PERFORM 3570-CHECK-LAST-ACT
               PERFORM 3580-ACCUM-STATUS
               PERFORM 3590-AGE-POST
               PERFORM 3600-SET-TRAN
               PERFORM 3610-COUNT-SEQ
               PERFORM 3620-DESCRIBE-REASON
               PERFORM 3630-COUNT-CYCLE
               PERFORM 3640-ACCRUE-INSURANCE
               PERFORM 3650-DESCRIBE-TYPE
               PERFORM 3660-EDIT-DESC
               PERFORM 3670-COUNT-EXCEPT
               PERFORM 3680-ACCRUE-SHORTAGE
               PERFORM 3690-DESCRIBE-PAYEE-TYPE
               PERFORM 3700-FLAG-SURPLUS
               PERFORM 3710-ACCRUE-PAYMENT
               PERFORM 3720-CHECK-LAST-PAY
               PERFORM 3730-ACCRUE-CUSHION
               PERFORM 3740-COUNT-NOTICE
               PERFORM 3750-CHECK-EFF
               PERFORM 3760-FLAG-SHORTAGE
               PERFORM 3770-DESCRIBE-REGION
               PERFORM 3780-DESCRIBE-PAYEE-TYPE
               PERFORM 3790-PRORATE-SHORTAGE
               PERFORM 3800-LIMIT-ITEM
               PERFORM 3810-FLAG-INSURANCE
               PERFORM 3820-LIMIT-ITEM
               PERFORM 3830-EDIT-ADDR
               PERFORM 3840-SPLIT-CUSHION
               PERFORM 3850-SET-CHANNEL
               PERFORM 3860-LIMIT-PAYMENT
               PERFORM 3870-CHECK-CLOSE
               PERFORM 3880-PRORATE-INSURANCE
               PERFORM 3890-FLAG-DISBURSE
               PERFORM 3900-PRORATE-SHORTAGE
               PERFORM 3910-SET-STATUS
               PERFORM 3920-ASSESS-PREFERRED
               PERFORM 3930-PRORATE-SURPLUS
               PERFORM 3940-DESCRIBE-TRAN
               PERFORM 3950-FLAG-DISBURSE
               PERFORM 3960-CHECK-EFFECTIVE
               PERFORM 3970-CHECK-EXPIRY
               PERFORM 3980-CHECK-EFFECTIVE
               PERFORM 3990-AGE-CLOSE
               PERFORM 4000-SET-TYPE
               PERFORM 4010-COUNT-SEQ
               PERFORM 4020-FLAG-DISBURSE
               PERFORM 4030-SPLIT-TAX
               PERFORM 4040-CHECK-LAST-PAY
               PERFORM 4050-FLAG-DISBURSE
               PERFORM 4060-ACCUM-REGION
               PERFORM 4070-DESCRIBE-PAYEE-TYPE
               PERFORM 4080-SPLIT-TAX
               PERFORM 4090-LIMIT-INSURANCE
               PERFORM 4100-SET-TYPE
               PERFORM 4110-DESCRIBE-STATUS
               PERFORM 4120-CHECK-LAST-ACT
               PERFORM 4130-FLAG-TAX
               PERFORM 4140-SET-PAYEE-TYPE
               PERFORM 4150-CHECK-EFF
               PERFORM 4160-LIMIT-INSURANCE
               PERFORM 4170-EDIT-ADDR
               PERFORM 4180-AGE-EFF
               PERFORM 4190-LIMIT-CUSHION
               PERFORM 4200-DESCRIBE-PAYEE-TYPE
               PERFORM 4210-LIMIT-CUSHION
               PERFORM 4220-SPLIT-TAX
               PERFORM 4230-SPLIT-SHORTAGE
               PERFORM 4240-DESCRIBE-PAYEE-TYPE
               PERFORM 4250-ACCRUE-SHORTAGE
               PERFORM 4260-SELECT-FEE-PCT
               PERFORM 4270-ASSESS-PREFERRED
               PERFORM 4280-DESCRIBE-CHANNEL
               PERFORM 4290-ASSESS-STOP
               PERFORM 4300-SELECT-RATE
               PERFORM 4310-SPLIT-TAX
               PERFORM 4320-PRORATE-SURPLUS
               PERFORM 4330-LIMIT-TAX
               PERFORM 4340-ACCRUE-DISBURSE
               PERFORM 7000-CALL-ESFEE
               PERFORM 7010-CALL-ESLIM
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
      * SPLIT SURPLUS AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3000-SPLIT-SURPLUS.
           COMPUTE WS-WORK-AMT = ESCM-SURPLUS-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ESCM-SURPLUS-AMT - (WS-WORK-AMT * 2)
           COMPUTE ESCM-SURPLUS-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURPLUS YTD FEE ON THE INSURANCE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3010-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SURPLUS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-HOLD-FLAG = 'Y'
                   COMPUTE ESCM-SURPLUS-YTD ROUNDED = ESCM-INSURANCE-YTD
                       * ESCM-RATE / 200
               WHEN ESCM-TYPE-CD = 'AKX'
                   MOVE 3.00 TO ESCM-SURPLUS-YTD
               WHEN ESCM-TYPE-CD = 'REP'
                   MOVE 10.00 TO ESCM-SURPLUS-YTD
               WHEN OTHER
                   COMPUTE ESCM-SURPLUS-YTD ROUNDED = ESCM-INSURANCE-YTD
                       * ESCM-RATE / 100
           END-EVALUATE
           IF ESCM-SURPLUS-YTD > 0 AND ESCM-SURPLUS-YTD < 5.00
               MOVE 5.00 TO ESCM-SURPLUS-YTD
           END-IF
           IF ESCM-SURPLUS-YTD > 250.00
               MOVE 250.00 TO ESCM-SURPLUS-YTD
           END-IF
           ADD ESCM-SURPLUS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-NEXT-REVIEW.
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
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SHORTAGE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3030-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = ESCT-SHORTAGE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ESCT-SHORTAGE-AMT - (WS-WORK-AMT * 2)
           COMPUTE ESCT-CUSHION-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE FEE ON THE INSURANCE YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3040-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ESCM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SHORTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   COMPUTE ESCM-SHORTAGE-AMT ROUNDED =
                       ESCM-INSURANCE-YTD * ESCM-RATE / 200
               WHEN ESCM-STATUS-CD = 'A'
                   MOVE 7.00 TO ESCM-SHORTAGE-AMT
               WHEN ESCM-STATUS-CD = 'E'
                   MOVE 7.00 TO ESCM-SHORTAGE-AMT
               WHEN OTHER
                   COMPUTE ESCM-SHORTAGE-AMT ROUNDED =
                       ESCM-INSURANCE-YTD * ESCM-RATE / 100
           END-EVALUATE
           IF ESCM-SHORTAGE-AMT > 0 AND ESCM-SHORTAGE-AMT < 1.00
               MOVE 1.00 TO ESCM-SHORTAGE-AMT
           END-IF
           IF ESCM-SHORTAGE-AMT > 75.00
               MOVE 75.00 TO ESCM-SHORTAGE-AMT
           END-IF
           ADD ESCM-SHORTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E350 CONTACT REQUIRED
      * E365 SURPLUS AMT NEGATIVE
      * E945 SURPLUS AMT OVER 99999
      * E631 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3050-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CONTACT = SPACES
                   MOVE 'E350' TO WS-REJECT-CODE
               WHEN ESCM-SURPLUS-AMT < 0
                   MOVE 'E365' TO WS-REJECT-CODE
               WHEN ESCM-SURPLUS-AMT > 99999.00
                   MOVE 'E945' TO WS-REJECT-CODE
               WHEN ESCM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E631' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * A -> E WHEN NEXT REVIEW DATE IS PAST
      * E -> R WHEN NEXT REVIEW DATE IS PAST
      * R -> U WHEN NEXT REVIEW DATE IS PAST
      * U -> Z WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3060-SET-STATUS.
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
      * STATUS TRANSITIONS FOR STATUS CD:
      * A -> E WHEN EFF DATE IS PAST
      * E -> R WHEN EFF DATE IS PAST
      * R -> U WHEN EFF DATE IS PAST
      * U -> Z WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-TAX.
           IF ESCM-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-TAX-YTD * ESCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E222 DESC REQUIRED
      * E948 INSURANCE AMT NEGATIVE
      * E706 INSURANCE AMT OVER 250000
      * E036 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3090-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-DESC = SPACES
                   MOVE 'E222' TO WS-REJECT-CODE
               WHEN ESCM-INSURANCE-AMT < 0
                   MOVE 'E948' TO WS-REJECT-CODE
               WHEN ESCM-INSURANCE-AMT > 250000.00
                   MOVE 'E706' TO WS-REJECT-CODE
               WHEN ESCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E036' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURPLUS YTD FEE ON THE INSURANCE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3100-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SURPLUS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ESCM-SURPLUS-YTD ROUNDED = ESCM-INSURANCE-YTD
                       * ESCM-DISCOUNT / 200
               WHEN ESCM-ANALYSIS-CD = 'GD'
                   MOVE 5.00 TO ESCM-SURPLUS-YTD
               WHEN ESCM-ANALYSIS-CD = 'LY'
                   MOVE 3.00 TO ESCM-SURPLUS-YTD
               WHEN OTHER
                   COMPUTE ESCM-SURPLUS-YTD ROUNDED = ESCM-INSURANCE-YTD
                       * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-SURPLUS-YTD > 0 AND ESCM-SURPLUS-YTD < 5.00
               MOVE 5.00 TO ESCM-SURPLUS-YTD
           END-IF
           IF ESCM-SURPLUS-YTD > 250.00
               MOVE 250.00 TO ESCM-SURPLUS-YTD
           END-IF
           ADD ESCM-SURPLUS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3110-FLAG-PAYMENT.
           IF ESCM-PAYMENT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3120-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = ESCM-DISBURSE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ESCM-DISBURSE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE ESCM-CUSHION-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UP'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3140-ACCRUE-SHORTAGE.
           IF ESCM-SHORTAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SHORTAGE-YTD * ESCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ESCT-INSURANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3150-FLAG-INSURANCE.
           IF ESCM-INSURANCE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCM-INSURANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-TAX.
           IF ESCM-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-TAX-AMT * ESCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ESCT-SHORTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SURPLUS YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-SURPLUS.
           COMPUTE WS-WORK-AMT = ESCM-SURPLUS-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ESCM-SURPLUS-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SURPLUS AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3180-SPLIT-SURPLUS.
           COMPUTE WS-WORK-AMT = ESCM-SURPLUS-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ESCM-SURPLUS-AMT - (WS-WORK-AMT * 4)
           COMPUTE ESCM-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3190-AGE-EFFECTIVE.
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
                   ADD ESCM-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3200-DESCRIBE-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'E'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'R'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'U'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'Z'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE AMT BY ANALYSIS CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3210-ACCUM-ANALYSIS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-ANALYSIS-CD
                   ADD ESCT-INSURANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-ANALYSIS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCT-INSURANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3220-COUNT-SEQ.
           IF ESCT-SEQ < 9999999
               ADD 1 TO ESCT-SEQ
           END-IF
           IF ESCT-SEQ NOT < 6
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E975 NAME REQUIRED
      * E161 WORK AMT 2 NEGATIVE
      * E978 WORK AMT 2 OVER 50000
      * E470 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3230-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-NAME = SPACES
                   MOVE 'E975' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E161' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E978' TO WS-REJECT-CODE
               WHEN ESCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E470' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NEXT-REVIEW-DATE TO DTU-DATE-1
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E309 CONTACT REQUIRED
      * E411 TAX AMT NEGATIVE
      * E628 TAX AMT OVER 99999
      * E493 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CONTACT = SPACES
                   MOVE 'E309' TO WS-REJECT-CODE
               WHEN ESCM-TAX-AMT < 0
                   MOVE 'E411' TO WS-REJECT-CODE
               WHEN ESCM-TAX-AMT > 99999.00
                   MOVE 'E628' TO WS-REJECT-CODE
               WHEN ESCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E493' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3260-DESCRIBE-CHANNEL.
           EVALUATE ESCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE TAX SCHED TIER FOR THE SURPLUS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-SCHED-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ESCM-SURPLUS-AMT NOT < WS-TAX-SCHED-LOW(WS-TX)
                   MOVE WS-TAX-SCHED-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3280-AGE-EFFECTIVE.
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
                   ADD ESCT-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCT-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ESCT-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCT-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCT-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CUSHION YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3290-ACCRUE-CUSHION.
           IF ESCM-CUSHION-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-CUSHION-YTD * ESCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ESCM-CUSHION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURPLUS AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3300-ACCRUE-SURPLUS.
           IF ESCM-SURPLUS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SURPLUS-AMT * ESCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-DISBURSE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-POST.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO ESCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DS -> PY WHEN EXPIRY DATE IS PAST
      * PY -> AN WHEN EXPIRY DATE IS PAST
      * AN -> RF WHEN EXPIRY DATE IS PAST
      * RF -> SH WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3320-SET-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   IF ESCM-EXPIRY-DATE > 0 AND ESCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ESCM-EXPIRY-DATE > 0 AND ESCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AN' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AN'
                   IF ESCM-EXPIRY-DATE > 0 AND ESCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RF'
                   IF ESCM-EXPIRY-DATE > 0 AND ESCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3330-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = ESCM-PAYMENT-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ESCM-PAYMENT-AMT - (WS-WORK-AMT * 2)
           COMPUTE ESCM-DISBURSE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CUSHION YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3340-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REGION-CD
                   ADD ESCM-CUSHION-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-CUSHION-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE YTD BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3350-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-PAYEE-TYPE-CD
                   ADD ESCM-DISBURSE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-DISBURSE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3360-FLAG-SURPLUS.
           IF ESCM-SURPLUS-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCM-SURPLUS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3370-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ESCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3380-AGE-NEXT-REVIEW.
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
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT SURPLUS YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3390-SPLIT-SURPLUS.
           COMPUTE WS-WORK-AMT = ESCM-SURPLUS-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ESCM-SURPLUS-YTD - (WS-WORK-AMT * 2)
           COMPUTE ESCT-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3400-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-CUSHION-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-CUSHION-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-CUSHION-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-CUSHION-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-CUSHION-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SURPLUS YTD.
      *----------------------------------------------------------------*
       3410-PRORATE-SURPLUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SURPLUS-YTD ROUNDED =
                   ESCM-DISBURSE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-SURPLUS-YTD
           END-IF
           ADD ESCM-SURPLUS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AKX -> REP WHEN LAST PAY DATE IS PAST
      * REP -> SMX WHEN LAST PAY DATE IS PAST
      * SMX -> VJL WHEN LAST PAY DATE IS PAST
      * VJL -> WKN WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-TYPE.
           EVALUATE ESCM-TYPE-CD
               WHEN 'AKX'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'REP' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'REP'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMX' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMX'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJL' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJL'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKN' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 180 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3430-CHECK-POST.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3440-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAX SCHED TIER FOR THE INSURANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-SCHED-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ESCM-INSURANCE-YTD NOT < WS-TAX-SCHED-LOW(WS-TX)
                   MOVE WS-TAX-SCHED-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3460-FLAG-ITEM.
           IF WS-WORK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-LAST-ACT.
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
                   ADD ESCM-TAX-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-TAX-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-TAX-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-TAX-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-TAX-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E231 CONTACT REQUIRED
      * E968 CUSHION AMT NEGATIVE
      * E444 CUSHION AMT OVER 1000000
      * E388 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3480-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CONTACT = SPACES
                   MOVE 'E231' TO WS-REJECT-CODE
               WHEN ESCT-CUSHION-AMT < 0
                   MOVE 'E968' TO WS-REJECT-CODE
               WHEN ESCT-CUSHION-AMT > 1000000.00
                   MOVE 'E444' TO WS-REJECT-CODE
               WHEN ESCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E388' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CUSHION AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3490-FLAG-CUSHION.
           IF ESCM-CUSHION-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCM-CUSHION-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E229 NAME REQUIRED
      * E872 CUSHION AMT NEGATIVE
      * E452 CUSHION AMT OVER 1000000
      * E515 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3500-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-NAME = SPACES
                   MOVE 'E229' TO WS-REJECT-CODE
               WHEN ESCM-CUSHION-AMT < 0
                   MOVE 'E872' TO WS-REJECT-CODE
               WHEN ESCM-CUSHION-AMT > 1000000.00
                   MOVE 'E452' TO WS-REJECT-CODE
               WHEN ESCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E515' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3510-ACCRUE-PAYMENT.
           IF ESCM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-PAYMENT-AMT * ESCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ESCM-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ANALYSIS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3520-DESCRIBE-ANALYSIS.
           EVALUATE ESCM-ANALYSIS-CD
               WHEN 'GD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'E'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E077 ADDR REQUIRED
      * E374 PAYMENT AMT NEGATIVE
      * E984 PAYMENT AMT OVER 5000
      * E722 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3540-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-ADDR = SPACES
                   MOVE 'E077' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-AMT < 0
                   MOVE 'E374' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-AMT > 5000.00
                   MOVE 'E984' TO WS-REJECT-CODE
               WHEN ESCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E722' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAX SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-SCHED-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-TAX-SCHED-LOW(WS-TX)
                   MOVE WS-TAX-SCHED-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AKX -> REP WHEN POST DATE IS PAST
      * REP -> SMX WHEN POST DATE IS PAST
      * SMX -> VJL WHEN POST DATE IS PAST
      * VJL -> WKN WHEN POST DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3560-SET-TYPE.
           EVALUATE ESCM-TYPE-CD
               WHEN 'AKX'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'REP' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'REP'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMX' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMX'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJL' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJL'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKN' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3570-CHECK-LAST-ACT.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO ESCM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-STATUS.
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
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DS -> PY WHEN EXPIRY DATE IS PAST
      * PY -> AN WHEN EXPIRY DATE IS PAST
      * AN -> RF WHEN EXPIRY DATE IS PAST
      * RF -> SH WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3600-SET-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   IF ESCM-EXPIRY-DATE > 0 AND ESCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ESCM-EXPIRY-DATE > 0 AND ESCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AN' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AN'
                   IF ESCM-EXPIRY-DATE > 0 AND ESCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RF'
                   IF ESCM-EXPIRY-DATE > 0 AND ESCM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3610-COUNT-SEQ.
           IF ESCT-SEQ < 9999999
               ADD 1 TO ESCT-SEQ
           END-IF
           IF ESCT-SEQ NOT < 3
               MOVE 'Y' TO ESCM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3620-DESCRIBE-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'F'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'K'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-CYCLE.
           IF ESCM-CYCLE-CNT < 9999999
               ADD 1 TO ESCM-CYCLE-CNT
           END-IF
           IF ESCM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INSURANCE YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3640-ACCRUE-INSURANCE.
           IF ESCM-INSURANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-INSURANCE-YTD * ESCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-CUSHION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3650-DESCRIBE-TYPE.
           EVALUATE ESCM-TYPE-CD
               WHEN 'AKX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'REP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'SMX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VJL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WKN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XKU'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XTF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E962 DESC REQUIRED
      * E947 PAYMENT AMT NEGATIVE
      * E339 PAYMENT AMT OVER 5000
      * E065 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-DESC = SPACES
                   MOVE 'E962' TO WS-REJECT-CODE
               WHEN ESCT-PAYMENT-AMT < 0
                   MOVE 'E947' TO WS-REJECT-CODE
               WHEN ESCT-PAYMENT-AMT > 5000.00
                   MOVE 'E339' TO WS-REJECT-CODE
               WHEN ESCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E065' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-EXCEPT.
           IF ESCM-EXCEPT-CNT < 9999999
               ADD 1 TO ESCM-EXCEPT-CNT
           END-IF
           IF ESCM-EXCEPT-CNT NOT < 6
               MOVE 'Y' TO ESCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3680-ACCRUE-SHORTAGE.
           IF ESCM-SHORTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SHORTAGE-AMT * ESCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ESCT-SHORTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3690-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SURPLUS YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3700-FLAG-SURPLUS.
           IF ESCM-SURPLUS-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCM-SURPLUS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-PAYMENT.
           IF ESCM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-PAYMENT-YTD * ESCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ESCM-SHORTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3720-CHECK-LAST-PAY.
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
               MOVE 'Y' TO ESCM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CUSHION AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3730-ACCRUE-CUSHION.
           IF ESCM-CUSHION-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-CUSHION-AMT * ESCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ESCM-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO ESCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3750-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF ESCT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3760-FLAG-SHORTAGE.
           IF ESCM-SHORTAGE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-SHORTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-REGION.
           EVALUATE ESCM-REGION-CD
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'G'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3780-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHORTAGE AMT.
      *----------------------------------------------------------------*
       3790-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SHORTAGE-AMT ROUNDED =
                   ESCM-PAYMENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-SHORTAGE-AMT
           END-IF
           ADD ESCM-SHORTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3800-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3810-FLAG-INSURANCE.
           IF ESCT-INSURANCE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCT-INSURANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3820-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E712 ADDR REQUIRED
      * E620 INSURANCE AMT NEGATIVE
      * E744 INSURANCE AMT OVER 250000
      * E518 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3830-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-ADDR = SPACES
                   MOVE 'E712' TO WS-REJECT-CODE
               WHEN ESCT-INSURANCE-AMT < 0
                   MOVE 'E620' TO WS-REJECT-CODE
               WHEN ESCT-INSURANCE-AMT > 250000.00
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN ESCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E518' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CUSHION YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3840-SPLIT-CUSHION.
           COMPUTE WS-WORK-AMT = ESCM-CUSHION-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ESCM-CUSHION-YTD - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NOTICE DATE IS PAST
      * BAT -> MAL WHEN NOTICE DATE IS PAST
      * MAL -> TEL WHEN NOTICE DATE IS PAST
      * TEL -> ONL WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3850-SET-CHANNEL.
           EVALUATE ESCT-CHANNEL-CD
               WHEN 'BRN'
                   IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3860-LIMIT-PAYMENT.
           IF ESCM-PAYMENT-YTD < 0.00
               MOVE 0.00 TO ESCM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-PAYMENT-YTD > 10000.00
               MOVE 10000.00 TO ESCM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3870-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURPLUS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INSURANCE YTD.
      *----------------------------------------------------------------*
       3880-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-INSURANCE-YTD ROUNDED =
                   ESCM-SURPLUS-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-INSURANCE-YTD
           END-IF
           ADD ESCM-INSURANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3890-FLAG-DISBURSE.
           IF ESCM-DISBURSE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCM-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INSURANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHORTAGE AMT.
      *----------------------------------------------------------------*
       3900-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCT-SHORTAGE-AMT ROUNDED =
                   ESCM-INSURANCE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCT-SHORTAGE-AMT
           END-IF
           ADD ESCT-SHORTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * A -> E WHEN LAST PAY DATE IS PAST
      * E -> R WHEN LAST PAY DATE IS PAST
      * R -> U WHEN LAST PAY DATE IS PAST
      * U -> Z WHEN LAST PAY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'E' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'E'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'R' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'R'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'U'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO ESCM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CUSHION FEE ON THE TAX YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3920-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ESCM-CUSHION-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   COMPUTE ESCM-CUSHION-AMT ROUNDED = ESCM-TAX-YTD *
                       ESCM-DISCOUNT / 200
               WHEN ESCM-STATUS-CD = 'A'
                   MOVE 20.00 TO ESCM-CUSHION-AMT
               WHEN ESCM-STATUS-CD = 'E'
                   MOVE 5.00 TO ESCM-CUSHION-AMT
               WHEN OTHER
                   COMPUTE ESCM-CUSHION-AMT ROUNDED = ESCM-TAX-YTD *
                       ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-CUSHION-AMT > 0 AND ESCM-CUSHION-AMT < 10.00
               MOVE 10.00 TO ESCM-CUSHION-AMT
           END-IF
           IF ESCM-CUSHION-AMT > 100.00
               MOVE 100.00 TO ESCM-CUSHION-AMT
           END-IF
           ADD ESCM-CUSHION-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SURPLUS AMT.
      *----------------------------------------------------------------*
       3930-PRORATE-SURPLUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SURPLUS-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-SURPLUS-AMT
           END-IF
           ADD ESCM-SURPLUS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3940-DESCRIBE-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PY'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'AN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RF'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'SH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3950-FLAG-DISBURSE.
           IF ESCM-DISBURSE-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCM-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3960-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3970-CHECK-EXPIRY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3980-CHECK-EFFECTIVE.
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
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3990-AGE-CLOSE.
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
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AKX -> REP WHEN EFF DATE IS PAST
      * REP -> SMX WHEN EFF DATE IS PAST
      * SMX -> VJL WHEN EFF DATE IS PAST
      * VJL -> WKN WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4000-SET-TYPE.
           EVALUATE ESCM-TYPE-CD
               WHEN 'AKX'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'REP' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'REP'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMX' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMX'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJL' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJL'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKN' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4010-COUNT-SEQ.
           IF ESCT-SEQ < 9999999
               ADD 1 TO ESCT-SEQ
           END-IF
           IF ESCT-SEQ NOT < 12
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4020-FLAG-DISBURSE.
           IF ESCM-DISBURSE-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAX YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4030-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = ESCM-TAX-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ESCM-TAX-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4040-CHECK-LAST-PAY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO ESCM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4050-FLAG-DISBURSE.
           IF ESCM-DISBURSE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCM-DISBURSE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CUSHION YTD BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4060-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REGION-CD
                   ADD ESCM-CUSHION-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-CUSHION-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UP'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT TAX YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4080-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = ESCM-TAX-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-TAX-YTD - (WS-WORK-AMT * 3)
           COMPUTE ESCM-SURPLUS-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4090-LIMIT-INSURANCE.
           IF ESCM-INSURANCE-AMT < 25.00
               MOVE 25.00 TO ESCM-INSURANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-INSURANCE-AMT > 50000.00
               MOVE 50000.00 TO ESCM-INSURANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * AKX -> REP WHEN LAST STMT DATE IS PAST
      * REP -> SMX WHEN LAST STMT DATE IS PAST
      * SMX -> VJL WHEN LAST STMT DATE IS PAST
      * VJL -> WKN WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4100-SET-TYPE.
           EVALUATE ESCM-TYPE-CD
               WHEN 'AKX'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'REP' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'REP'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SMX' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SMX'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VJL' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VJL'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WKN' TO ESCM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4110-DESCRIBE-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'E'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'R'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'U'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4120-CHECK-LAST-ACT.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO ESCM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAX YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4130-FLAG-TAX.
           IF ESCM-TAX-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * AM -> UE WHEN EFFECTIVE DATE IS PAST
      * UE -> UP WHEN EFFECTIVE DATE IS PAST
      * UP -> AM WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4140-SET-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   IF ESCM-EFFECTIVE-DATE > 0 AND ESCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UE' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UE'
                   IF ESCM-EFFECTIVE-DATE > 0 AND ESCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UP' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UP'
                   IF ESCM-EFFECTIVE-DATE > 0 AND ESCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AM' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4150-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF ESCT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCT-EFF-DATE TO DTU-DATE-1
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
      * INSURANCE YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4160-LIMIT-INSURANCE.
           IF ESCM-INSURANCE-YTD < 50.00
               MOVE 50.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-INSURANCE-YTD > 1000.00
               MOVE 1000.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E286 ADDR REQUIRED
      * E718 SURPLUS AMT NEGATIVE
      * E050 SURPLUS AMT OVER 50000
      * E099 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4170-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-ADDR = SPACES
                   MOVE 'E286' TO WS-REJECT-CODE
               WHEN ESCM-SURPLUS-AMT < 0
                   MOVE 'E718' TO WS-REJECT-CODE
               WHEN ESCM-SURPLUS-AMT > 50000.00
                   MOVE 'E050' TO WS-REJECT-CODE
               WHEN ESCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E099' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4180-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCT-EFF-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-SHORTAGE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SHORTAGE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CUSHION AMT IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4190-LIMIT-CUSHION.
           IF ESCM-CUSHION-AMT < 100.00
               MOVE 100.00 TO ESCM-CUSHION-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-CUSHION-AMT > 2500.00
               MOVE 2500.00 TO ESCM-CUSHION-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4200-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CUSHION AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4210-LIMIT-CUSHION.
           IF ESCT-CUSHION-AMT < 50.00
               MOVE 50.00 TO ESCT-CUSHION-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCT-CUSHION-AMT > 50000.00
               MOVE 50000.00 TO ESCT-CUSHION-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAX AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4220-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = ESCM-TAX-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ESCM-TAX-AMT - (WS-WORK-AMT * 2)
           COMPUTE ESCM-INSURANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHORTAGE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4230-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = ESCM-SHORTAGE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ESCM-SHORTAGE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE ESCM-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4240-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-SHORTAGE.
           IF ESCM-SHORTAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SHORTAGE-YTD * ESCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ESCM-SHORTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAX SCHED TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4260-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-SCHED-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF ESCM-PAYMENT-AMT NOT < WS-TAX-SCHED-LOW(WS-TX)
                   MOVE WS-TAX-SCHED-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISBURSE FEE ON THE INSURANCE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4270-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ESCM-DISBURSE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-HOLD-FLAG = 'Y'
                   COMPUTE ESCM-DISBURSE-AMT ROUNDED =
                       ESCT-INSURANCE-AMT * ESCM-RATE / 200
               WHEN ESCM-STATUS-CD = 'A'
                   MOVE 10.00 TO ESCM-DISBURSE-AMT
               WHEN ESCM-STATUS-CD = 'E'
                   MOVE 10.00 TO ESCM-DISBURSE-AMT
               WHEN OTHER
                   COMPUTE ESCM-DISBURSE-AMT ROUNDED =
                       ESCT-INSURANCE-AMT * ESCM-RATE / 100
           END-EVALUATE
           IF ESCM-DISBURSE-AMT > 0 AND ESCM-DISBURSE-AMT < 2.00
               MOVE 2.00 TO ESCM-DISBURSE-AMT
           END-IF
           IF ESCM-DISBURSE-AMT > 35.00
               MOVE 35.00 TO ESCM-DISBURSE-AMT
           END-IF
           ADD ESCM-DISBURSE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4280-DESCRIBE-CHANNEL.
           EVALUATE ESCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE INSURANCE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4290-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ESCM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-HOLD-FLAG = 'Y'
                   COMPUTE ESCM-PAYMENT-YTD ROUNDED = ESCM-INSURANCE-YTD
                       * ESCM-DISCOUNT / 200
               WHEN ESCT-TRAN-CD = 'DS'
                   MOVE 5.00 TO ESCM-PAYMENT-YTD
               WHEN ESCT-TRAN-CD = 'PY'
                   MOVE 12.00 TO ESCM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE ESCM-PAYMENT-YTD ROUNDED = ESCM-INSURANCE-YTD
                       * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-PAYMENT-YTD > 0 AND ESCM-PAYMENT-YTD < 1.00
               MOVE 1.00 TO ESCM-PAYMENT-YTD
           END-IF
           IF ESCM-PAYMENT-YTD > 100.00
               MOVE 100.00 TO ESCM-PAYMENT-YTD
           END-IF
           ADD ESCM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE TAX SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4300-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-SCHED-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF WS-WORK-AMT-2 NOT < WS-TAX-SCHED-LOW(WS-TX)
                   MOVE WS-TAX-SCHED-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAX YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4310-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = ESCM-TAX-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-TAX-YTD - (WS-WORK-AMT * 3)
           COMPUTE ESCT-CUSHION-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SURPLUS YTD.
      *----------------------------------------------------------------*
       4320-PRORATE-SURPLUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SURPLUS-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-SURPLUS-YTD
           END-IF
           ADD ESCM-SURPLUS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TAX AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4330-LIMIT-TAX.
           IF ESCM-TAX-AMT < 0.00
               MOVE 0.00 TO ESCM-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-TAX-AMT > 2500.00
               MOVE 2500.00 TO ESCM-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4340-ACCRUE-DISBURSE.
           IF ESCM-DISBURSE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-DISBURSE-AMT * ESCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ESCT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

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
                   MOVE ESFEE-SURPLUS-YTD
                     TO ESCM-SURPLUS-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-ESFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE ESC FIELDS TO ESLIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-ESLIM.
           INITIALIZE ESLIM-PARMS
           MOVE ESCM-TAX-YTD
             TO ESLIM-TAX-YTD
           MOVE ESCM-INSURANCE-AMT
             TO ESLIM-INSURANCE-AMT
           MOVE ESCM-SHORTAGE-YTD
             TO ESLIM-SHORTAGE-YTD
           MOVE ESCM-SURPLUS-AMT
             TO ESLIM-SURPLUS-AMT
           MOVE ESCM-PAYMENT-AMT
             TO ESLIM-PAYMENT-AMT
           MOVE ESCM-SHORTAGE-AMT
             TO ESLIM-SHORTAGE-AMT
           MOVE ESCM-EXPIRY-DATE
             TO ESLIM-EXPIRY-DATE
           MOVE ESCM-LAST-ACT-DATE
             TO ESLIM-LAST-ACT-DATE
           MOVE ESCM-LAST-PAY-DATE
             TO ESLIM-LAST-PAY-DATE
           MOVE ESCM-NOTICE-DATE
             TO ESLIM-NOTICE-DATE
           MOVE ESCM-REASON-CD
             TO ESLIM-REASON-CD
           MOVE ESCM-PAYEE-TYPE-CD
             TO ESLIM-PAYEE-TYPE-CD
           MOVE ESCM-TYPE-CD
             TO ESLIM-TYPE-CD
           MOVE ESCM-STATUS-CD
             TO ESLIM-STATUS-CD
           MOVE ESCM-NOTICE-SENT-FLAG
             TO ESLIM-NOTICE-SENT-FLAG
           MOVE ESCM-SUSPEND-FLAG
             TO ESLIM-SUSPEND-FLAG
           MOVE ESCM-OVERRIDE-FLAG
             TO ESLIM-OVERRIDE-FLAG
           MOVE ESCM-RETRY-CNT
             TO ESLIM-RETRY-CNT
           MOVE ESCM-EXCEPT-CNT
             TO ESLIM-EXCEPT-CNT
           MOVE ESCM-FEE-PCT
             TO ESLIM-FEE-PCT
           MOVE ESCM-RATE
             TO ESLIM-RATE
           MOVE ESCM-CUST-ID
             TO ESLIM-CUST-ID
           MOVE ESCM-DESC
             TO ESLIM-DESC
           CALL 'ESLIM' USING ESLIM-PARMS
           EVALUATE ESLIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ESLIM-TAX-YTD
                     TO ESCM-TAX-YTD
                   MOVE ESLIM-INSURANCE-AMT
                     TO ESCM-INSURANCE-AMT
                   MOVE ESLIM-SURPLUS-AMT
                     TO ESCM-SURPLUS-AMT
                   MOVE ESLIM-SUSPEND-FLAG
                     TO ESCM-SUSPEND-FLAG
                   MOVE ESLIM-OVERRIDE-FLAG
                     TO ESCM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-ESLIM' TO ABND-PARAGRAPH
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
           DISPLAY 'ESCARCH READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ESCARCH' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
