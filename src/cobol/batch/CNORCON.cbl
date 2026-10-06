      *================================================================*
      * PROGRAM  : CNORCON                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : COIN AND CURRENCY - RECONCILIATION. READS THE CCO   *
      *            TRANSACTIONS, UPDATES THE CCO MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. BRANCH COIN AND CURRENCY ORDERS *
      *            AND ARMORED CARRIER SHIPMENT                        *
      * INPUT    : CCOMST (KSDS), CCOTRN, BANKCTL                      *
      * OUTPUT   : CCOOUT, CNORCRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CNPEN, CNDTE            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-03-16 JLM  ORIGINAL                                       *
      * 1989-07-10 DPS  FEE RERUN PROTECTION CHG9622                   *
      * 1991-01-11 PQR  COIN TABLE EXPANDED INC2310                    *
      * 1993-06-24 LMB  CURRENCY LIMIT REVISED CHG9461                 *
      * 1996-01-24 DPS  COIN FEE SCHEDULE UPDATE INC0314               *
      * 1996-10-19 SKN  SHIPMENT ROUNDING - HALF UP PRB7673            *
      * 1998-07-27 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-08-20 TWH  PERFORMANCE - CURRENCY READ AHEAD PRB5393      *
      * 2003-04-18 PQR  COIN CUTOFF MOVED REQ3667                      *
      * 2003-12-12 CRP  SHIPMENT EXCEPTIONS TO OPS QUEUE CHG1954       *
      * 2004-12-25 SKN  STRAP CUTOFF MOVED CHG4674                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CNORCON.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "CCOMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CCOM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "CCOTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "CCOOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "CNORCRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY CCOMST.
       FD  TRAN-FILE.
       COPY CCOTRN.
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
                                           VALUE 8.000000.
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
      * WS-COIN-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COIN-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000137500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000524500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000510000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000196000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025250000000375000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025350000000585500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030350000000715500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030450000000572000'.
       01  WS-COIN-TIER-TBL REDEFINES WS-COIN-TIER-VALUES.
           05  WS-COIN-TIER-ROW OCCURS 8 TIMES.
               10  WS-COIN-TIER-LOW        PIC 9(09)V99.
               10  WS-COIN-TIER-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CURRENCY-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CURRENCY-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000059000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000394500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000859000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010600000000577500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020600000000187000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020650000000203000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021650000000191000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022650000000032000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022700000000535500'.
       01  WS-CURRENCY-SCHED-TBL REDEFINES WS-CURRENCY-SCHED-VALUES.
           05  WS-CURRENCY-SCHED-ROW OCCURS 9 TIMES.
               10  WS-CURRENCY-SCHED-LOW   PIC 9(09)V99.
               10  WS-CURRENCY-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VARIANCE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VARIANCE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000840500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000622000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010250000000825000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000889500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017750000000734000'.
       01  WS-VARIANCE-BAND-TBL REDEFINES WS-VARIANCE-BAND-VALUES.
           05  WS-VARIANCE-BAND-ROW OCCURS 5 TIMES.
               10  WS-VARIANCE-BAND-LOW    PIC 9(09)V99.
               10  WS-VARIANCE-BAND-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COIN-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COIN-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000642000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000493000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003500000000485000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000362000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007000000000753000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017000000000484500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017050000000659000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018050000000198500'.
       01  WS-COIN-GRID-TBL REDEFINES WS-COIN-GRID-VALUES.
           05  WS-COIN-GRID-ROW OCCURS 8 TIMES.
               10  WS-COIN-GRID-LOW        PIC 9(09)V99.
               10  WS-COIN-GRID-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CNORCON  COIN AND CURRENCY'.
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
       COPY CNPENP.
       COPY CNDTEP.

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
               MOVE 'CCOMST' TO ABND-FILE-NAME
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
           MOVE CCOT-KEY TO CCOM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-ACCUM-REASON
               PERFORM 3010-DESCRIBE-CARRIER
               PERFORM 3020-SPLIT-FEE
               PERFORM 3030-SPLIT-FEE
               PERFORM 3040-SPLIT-CURRENCY
               PERFORM 3050-SET-REASON
               PERFORM 3060-ACCUM-REASON
               PERFORM 3070-PRORATE-SHIPMENT
               PERFORM 3080-ACCRUE-CURRENCY
               PERFORM 3090-FLAG-SHIPMENT
               PERFORM 3100-ASSESS-EXEMPT
               PERFORM 3110-FLAG-STRAP
               PERFORM 3120-SET-SOURCE
               PERFORM 3130-CHECK-EFFECTIVE
               PERFORM 3140-CHECK-NOTICE
               PERFORM 3150-AGE-NOTICE
               PERFORM 3160-LIMIT-FEE
               PERFORM 3170-SELECT-PENALTY-PCT
               PERFORM 3180-COUNT-ITEM
               PERFORM 3190-DESCRIBE-TRAN
               PERFORM 3200-AGE-NOTICE
               PERFORM 3210-AGE-POST
               PERFORM 3220-FLAG-SHIPMENT
               PERFORM 3230-SPLIT-COIN
               PERFORM 3240-FLAG-ITEM
               PERFORM 3250-SET-REASON
               PERFORM 3260-SPLIT-STRAP
               PERFORM 3270-PRORATE-FEE
               PERFORM 3280-DESCRIBE-STATUS
               PERFORM 3290-SELECT-RATE
               PERFORM 3300-EDIT-CONTACT
               PERFORM 3310-ACCRUE-COIN
               PERFORM 3320-SPLIT-SHIPMENT
               PERFORM 3330-SELECT-RATE
               PERFORM 3340-SET-CHANNEL
               PERFORM 3350-ACCRUE-VARIANCE
               PERFORM 3360-CHECK-NEXT-DUE
               PERFORM 3370-FLAG-CURRENCY
               PERFORM 3380-CHECK-LAST-PAY
               PERFORM 3390-ACCRUE-STRAP
               PERFORM 3400-SPLIT-STRAP
               PERFORM 3410-SPLIT-STRAP
               PERFORM 3420-CHECK-MATURITY
               PERFORM 3430-SPLIT-CURRENCY
               PERFORM 3440-COUNT-NOTICE
               PERFORM 3450-ASSESS-AUTO-RENEW
               PERFORM 3460-CHECK-NEXT-DUE
               PERFORM 3470-SPLIT-COIN
               PERFORM 3480-EDIT-CUST-ID
               PERFORM 3490-DESCRIBE-CARRIER
               PERFORM 3500-ASSESS-AUTO-RENEW
               PERFORM 3510-LIMIT-SHIPMENT
               PERFORM 3520-ACCRUE-COIN
               PERFORM 3530-ACCRUE-SHIPMENT
               PERFORM 3540-AGE-OPEN
               PERFORM 3550-SPLIT-CURRENCY
               PERFORM 3560-CHECK-LAST-ACT
               PERFORM 3570-ASSESS-REVERSAL
               PERFORM 3580-SET-CHANNEL
               PERFORM 3590-SELECT-FEE-PCT
               PERFORM 3600-ASSESS-REVERSAL
               PERFORM 3610-FLAG-SHIPMENT
               PERFORM 3620-COUNT-RETRY
               PERFORM 3630-COUNT-NOTICE
               PERFORM 3640-ASSESS-OVERRIDE
               PERFORM 3650-ACCUM-DENOM
               PERFORM 3660-ASSESS-EXEMPT
               PERFORM 3670-EDIT-CONTACT
               PERFORM 3680-SPLIT-FEE
               PERFORM 3690-COUNT-RETRY
               PERFORM 3700-FLAG-SHIPMENT
               PERFORM 3710-FLAG-VARIANCE
               PERFORM 3720-CHECK-NEXT-REVIEW
               PERFORM 3730-ASSESS-NOTICE-SENT
               PERFORM 3740-EDIT-NAME
               PERFORM 3750-AGE-NEXT-REVIEW
               PERFORM 3760-SET-CARRIER
               PERFORM 3770-DESCRIBE-TRAN
               PERFORM 3780-ASSESS-HOLD
               PERFORM 3790-DESCRIBE-STATUS
               PERFORM 3800-CHECK-OPEN
               PERFORM 3810-ASSESS-EXEMPT
               PERFORM 3820-EDIT-CONTACT
               PERFORM 3830-PRORATE-VARIANCE
               PERFORM 3840-ASSESS-NOTICE-SENT
               PERFORM 3850-EDIT-CONTACT
               PERFORM 3860-AGE-MATURITY
               PERFORM 3870-SELECT-FEE-PCT
               PERFORM 3880-SET-STATUS
               PERFORM 3890-SPLIT-VARIANCE
               PERFORM 3900-ACCUM-CARRIER
               PERFORM 3910-ACCRUE-VARIANCE
               PERFORM 3920-DESCRIBE-REASON
               PERFORM 3930-COUNT-ITEM
               PERFORM 3940-FLAG-FEE
               PERFORM 3950-SET-REASON
               PERFORM 3960-ASSESS-EXEMPT
               PERFORM 3970-COUNT-CYCLE
               PERFORM 3980-ACCUM-CARRIER
               PERFORM 3990-ACCRUE-SHIPMENT
               PERFORM 4000-SPLIT-STRAP
               PERFORM 4010-LIMIT-SHIPMENT
               PERFORM 4020-FLAG-STRAP
               PERFORM 4030-CHECK-NEXT-DUE
               PERFORM 4040-SELECT-PENALTY-PCT
               PERFORM 4050-PRORATE-STRAP
               PERFORM 4060-LIMIT-CURRENCY
               PERFORM 4070-DESCRIBE-CARRIER
               PERFORM 4080-SET-TRAN
               PERFORM 4090-FLAG-FEE
               PERFORM 4100-ASSESS-OVERRIDE
               PERFORM 4110-SELECT-RATE
               PERFORM 4120-FLAG-STRAP
               PERFORM 4130-SPLIT-STRAP
               PERFORM 4140-ACCUM-CHANNEL
               PERFORM 4150-COUNT-RETRY
               PERFORM 4160-ASSESS-OVERRIDE
               PERFORM 4170-EDIT-CONTACT
               PERFORM 4180-SELECT-PENALTY-PCT
               PERFORM 4190-DESCRIBE-STATUS
               PERFORM 4200-AGE-OPEN
               PERFORM 4210-DESCRIBE-TRAN
               PERFORM 4220-DESCRIBE-DENOM
               PERFORM 4230-SET-REASON
               PERFORM 4240-AGE-NEXT-REVIEW
               PERFORM 4250-AGE-EFF
               PERFORM 4260-CHECK-NEXT-REVIEW
               PERFORM 4270-LIMIT-VARIANCE
               PERFORM 4280-ACCRUE-SHIPMENT
               PERFORM 4290-ACCRUE-VARIANCE
               PERFORM 4300-SPLIT-VARIANCE
               PERFORM 4310-LIMIT-ITEM
               PERFORM 4320-DESCRIBE-CHANNEL
               PERFORM 4330-SPLIT-VARIANCE
               PERFORM 4340-SELECT-RATE
               PERFORM 4350-ASSESS-OVERRIDE
               PERFORM 4360-PRORATE-VARIANCE
               PERFORM 4370-SPLIT-VARIANCE
               PERFORM 4380-ACCUM-REASON
               PERFORM 4390-SELECT-PENALTY-PCT
               PERFORM 4400-PRORATE-CURRENCY
               PERFORM 4410-CHECK-LAST-ACT
               PERFORM 4420-ACCRUE-STRAP
               PERFORM 4430-SET-REASON
               PERFORM 4440-FLAG-VARIANCE
               PERFORM 4450-SET-DENOM
               PERFORM 4460-CHECK-EFFECTIVE
               PERFORM 4470-COUNT-SEQ
               PERFORM 4480-ASSESS-REVERSAL
               PERFORM 4490-FLAG-VARIANCE
               PERFORM 4500-AGE-MATURITY
               PERFORM 4510-FLAG-SHIPMENT
               PERFORM 4520-SET-STATUS
               PERFORM 7010-CALL-CNPEN
               PERFORM 7000-CALL-CNDTE
               REWRITE CCO-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'CCOMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM CCO-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * ACCUMULATE COIN AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3000-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-REASON-CD
                   ADD CCOT-COIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOT-COIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3010-DESCRIBE-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'M'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3020-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = CCOM-FEE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-FEE-AMT - (WS-WORK-AMT * 12)
           COMPUTE CCOT-SHIPMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3030-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = CCOM-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CCOM-FEE-YTD - (WS-WORK-AMT * 4)
           COMPUTE CCOT-COIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CURRENCY YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-CURRENCY.
           COMPUTE WS-WORK-AMT = CCOM-CURRENCY-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CCOM-CURRENCY-YTD - (WS-WORK-AMT * 2)
           COMPUTE CCOM-SHIPMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * JL -> LW WHEN EFFECTIVE DATE IS PAST
      * LW -> PS WHEN EFFECTIVE DATE IS PAST
      * PS -> JL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-REASON.
           EVALUATE CCOM-REASON-CD
               WHEN 'JL'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LW' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LW'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3060-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-REASON-CD
                   ADD CCOM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CURRENCY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPMENT AMT.
      *----------------------------------------------------------------*
       3070-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-SHIPMENT-AMT ROUNDED =
                   CCOM-CURRENCY-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOM-SHIPMENT-AMT
           END-IF
           ADD CCOM-SHIPMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CURRENCY YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-CURRENCY.
           IF CCOM-CURRENCY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-CURRENCY-YTD * CCOM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-STRAP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3090-FLAG-SHIPMENT.
           IF CCOT-SHIPMENT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOT-SHIPMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CURRENCY FEE ON THE SHIPMENT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3100-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CCOM-CURRENCY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CCOM-CURRENCY-AMT ROUNDED = CCOM-SHIPMENT-YTD
                       * CCOM-RATE / 200
               WHEN CCOT-TRAN-CD = 'OR'
                   MOVE 3.00 TO CCOM-CURRENCY-AMT
               WHEN CCOT-TRAN-CD = 'SH'
                   MOVE 5.00 TO CCOM-CURRENCY-AMT
               WHEN OTHER
                   COMPUTE CCOM-CURRENCY-AMT ROUNDED = CCOM-SHIPMENT-YTD
                       * CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-CURRENCY-AMT > 0 AND CCOM-CURRENCY-AMT < 1.00
               MOVE 1.00 TO CCOM-CURRENCY-AMT
           END-IF
           IF CCOM-CURRENCY-AMT > 75.00
               MOVE 75.00 TO CCOM-CURRENCY-AMT
           END-IF
           ADD CCOM-CURRENCY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STRAP AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3110-FLAG-STRAP.
           IF CCOM-STRAP-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOM-STRAP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CFR -> DTX WHEN MATURITY DATE IS PAST
      * DTX -> EEA WHEN MATURITY DATE IS PAST
      * EEA -> EUA WHEN MATURITY DATE IS PAST
      * EUA -> TPB WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-SOURCE.
           EVALUATE CCOM-SOURCE-CD
               WHEN 'CFR'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DTX' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DTX'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EEA' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EEA'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EUA' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EUA'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TPB' TO CCOM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3130-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3150-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3160-LIMIT-FEE.
           IF CCOM-FEE-AMT < 50.00
               MOVE 50.00 TO CCOM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-FEE-AMT > 500.00
               MOVE 500.00 TO CCOM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CURRENCY SCHED TIER FOR THE SHIPMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3170-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CURRENCY-SCHED-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CCOT-SHIPMENT-AMT NOT < WS-CURRENCY-SCHED-LOW(WS-TX)
                   MOVE WS-CURRENCY-SCHED-RATE(WS-TX) TO
                       CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3180-COUNT-ITEM.
           IF CCOM-ITEM-CNT < 9999999
               ADD 1 TO CCOM-ITEM-CNT
           END-IF
           IF CCOM-ITEM-CNT NOT < 3
               MOVE 'Y' TO CCOM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3190-DESCRIBE-TRAN.
           EVALUATE CCOT-TRAN-CD
               WHEN 'OR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'SH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RC'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NOTICE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3210-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF CCOT-POST-DATE > 0 AND CCOT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPMENT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3220-FLAG-SHIPMENT.
           IF CCOM-SHIPMENT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-SHIPMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COIN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3230-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = CCOT-COIN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CCOT-COIN-AMT - (WS-WORK-AMT * 4)
           COMPUTE CCOT-VARIANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3240-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * JL -> LW WHEN EXPIRY DATE IS PAST
      * LW -> PS WHEN EXPIRY DATE IS PAST
      * PS -> JL WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3250-SET-REASON.
           EVALUATE CCOM-REASON-CD
               WHEN 'JL'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LW' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LW'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT STRAP YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3260-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = CCOM-STRAP-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CCOM-STRAP-YTD - (WS-WORK-AMT * 3)
           COMPUTE CCOM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3270-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CCOM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3280-DESCRIBE-STATUS.
           EVALUATE CCOM-STATUS-CD
               WHEN 'ADH'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FVV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GCB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UZP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'VXB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WTJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XNG'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COIN GRID TIER FOR THE STRAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3290-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-GRID-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CCOM-STRAP-YTD NOT < WS-COIN-GRID-LOW(WS-TX)
                   MOVE WS-COIN-GRID-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E684 CONTACT REQUIRED
      * E439 SHIPMENT AMT NEGATIVE
      * E815 SHIPMENT AMT OVER 5000
      * E528 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3300-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CONTACT = SPACES
                   MOVE 'E684' TO WS-REJECT-CODE
               WHEN CCOM-SHIPMENT-AMT < 0
                   MOVE 'E439' TO WS-REJECT-CODE
               WHEN CCOM-SHIPMENT-AMT > 5000.00
                   MOVE 'E815' TO WS-REJECT-CODE
               WHEN CCOT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E528' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COIN AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3310-ACCRUE-COIN.
           IF CCOM-COIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-COIN-AMT * CCOM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-STRAP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHIPMENT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3320-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = CCOT-SHIPMENT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CCOT-SHIPMENT-AMT - (WS-WORK-AMT *
               12)
           COMPUTE CCOT-VARIANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE VARIANCE BAND TIER FOR THE COIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VARIANCE-BAND-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF CCOM-COIN-YTD NOT < WS-VARIANCE-BAND-LOW(WS-TX)
                   MOVE WS-VARIANCE-BAND-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3340-SET-CHANNEL.
           EVALUATE CCOT-CHANNEL-CD
               WHEN 'BRN'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3350-ACCRUE-VARIANCE.
           IF CCOT-VARIANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-VARIANCE-AMT * CCOM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CCOT-SHIPMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3360-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CURRENCY YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3370-FLAG-CURRENCY.
           IF CCOM-CURRENCY-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-CURRENCY-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3380-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO CCOM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STRAP AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3390-ACCRUE-STRAP.
           IF CCOT-STRAP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-STRAP-AMT * CCOM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOT-SHIPMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STRAP AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = CCOM-STRAP-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-STRAP-AMT - (WS-WORK-AMT * 12)
           COMPUTE CCOT-COIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STRAP AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3410-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = CCOM-STRAP-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CCOM-STRAP-AMT - (WS-WORK-AMT * 4)
           COMPUTE CCOT-VARIANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3420-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO CCOM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CURRENCY AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3430-SPLIT-CURRENCY.
           COMPUTE WS-WORK-AMT = CCOM-CURRENCY-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CCOM-CURRENCY-AMT - (WS-WORK-AMT * 2)
           COMPUTE CCOT-STRAP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3440-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STRAP FEE ON THE VARIANCE AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3450-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-STRAP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   COMPUTE CCOM-STRAP-AMT ROUNDED = CCOM-VARIANCE-AMT *
                       CCOM-RATE / 200
               WHEN CCOM-SOURCE-CD = 'CFR'
                   MOVE 3.00 TO CCOM-STRAP-AMT
               WHEN CCOM-SOURCE-CD = 'DTX'
                   MOVE 7.00 TO CCOM-STRAP-AMT
               WHEN OTHER
                   COMPUTE CCOM-STRAP-AMT ROUNDED = CCOM-VARIANCE-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-STRAP-AMT > 0 AND CCOM-STRAP-AMT < 2.00
               MOVE 2.00 TO CCOM-STRAP-AMT
           END-IF
           IF CCOM-STRAP-AMT > 50.00
               MOVE 50.00 TO CCOM-STRAP-AMT
           END-IF
           ADD CCOM-STRAP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3460-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO CCOM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COIN AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3470-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = CCOT-COIN-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CCOT-COIN-AMT - (WS-WORK-AMT * 3)
           COMPUTE CCOT-VARIANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E821 CUST ID REQUIRED
      * E269 STRAP YTD NEGATIVE
      * E807 STRAP YTD OVER 10000
      * E505 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3480-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CUST-ID = SPACES
                   MOVE 'E821' TO WS-REJECT-CODE
               WHEN CCOM-STRAP-YTD < 0
                   MOVE 'E269' TO WS-REJECT-CODE
               WHEN CCOM-STRAP-YTD > 10000.00
                   MOVE 'E807' TO WS-REJECT-CODE
               WHEN CCOM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E505' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'L'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'M'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'X'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE YTD FEE ON THE FEE YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3500-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-VARIANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-HOLD-FLAG = 'Y'
                   COMPUTE CCOM-VARIANCE-YTD ROUNDED = CCOM-FEE-YTD *
                       CCOM-FEE-PCT / 200
               WHEN CCOM-SOURCE-CD = 'CFR'
                   MOVE 12.00 TO CCOM-VARIANCE-YTD
               WHEN CCOM-SOURCE-CD = 'DTX'
                   MOVE 20.00 TO CCOM-VARIANCE-YTD
               WHEN OTHER
                   COMPUTE CCOM-VARIANCE-YTD ROUNDED = CCOM-FEE-YTD *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-VARIANCE-YTD > 0 AND CCOM-VARIANCE-YTD < 10.00
               MOVE 10.00 TO CCOM-VARIANCE-YTD
           END-IF
           IF CCOM-VARIANCE-YTD > 100.00
               MOVE 100.00 TO CCOM-VARIANCE-YTD
           END-IF
           ADD CCOM-VARIANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SHIPMENT AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3510-LIMIT-SHIPMENT.
           IF CCOT-SHIPMENT-AMT < 25.00
               MOVE 25.00 TO CCOT-SHIPMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-SHIPMENT-AMT > 500.00
               MOVE 500.00 TO CCOT-SHIPMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COIN AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3520-ACCRUE-COIN.
           IF CCOT-COIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-COIN-AMT * CCOM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-SHIPMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3530-ACCRUE-SHIPMENT.
           IF CCOM-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-SHIPMENT-AMT * CCOM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-VARIANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3540-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-SHIPMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-SHIPMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-SHIPMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-SHIPMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-SHIPMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CURRENCY YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3550-SPLIT-CURRENCY.
           COMPUTE WS-WORK-AMT = CCOM-CURRENCY-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CCOM-CURRENCY-YTD - (WS-WORK-AMT * 3)
           COMPUTE CCOM-VARIANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3560-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO CCOM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT FEE ON THE COIN AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3570-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CCOM-SHIPMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOT-COIN-AMT *
                       CCOM-FEE-PCT / 200
               WHEN CCOM-CHANNEL-CD = 'AW'
                   MOVE 7.00 TO CCOM-SHIPMENT-AMT
               WHEN CCOM-CHANNEL-CD = 'DW'
                   MOVE 5.00 TO CCOM-SHIPMENT-AMT
               WHEN OTHER
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOT-COIN-AMT *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-SHIPMENT-AMT > 0 AND CCOM-SHIPMENT-AMT < 1.00
               MOVE 1.00 TO CCOM-SHIPMENT-AMT
           END-IF
           IF CCOM-SHIPMENT-AMT > 75.00
               MOVE 75.00 TO CCOM-SHIPMENT-AMT
           END-IF
           ADD CCOM-SHIPMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AW -> DW WHEN NEXT DUE DATE IS PAST
      * DW -> GN WHEN NEXT DUE DATE IS PAST
      * GN -> RS WHEN NEXT DUE DATE IS PAST
      * RS -> UZ WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3580-SET-CHANNEL.
           EVALUATE CCOM-CHANNEL-CD
               WHEN 'AW'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DW' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DW'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GN' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GN'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RS' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RS'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UZ' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COIN GRID TIER FOR THE COIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-GRID-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CCOT-COIN-AMT NOT < WS-COIN-GRID-LOW(WS-TX)
                   MOVE WS-COIN-GRID-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STRAP FEE ON THE WORK AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3600-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CCOT-STRAP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CCOT-STRAP-AMT ROUNDED = WS-WORK-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOM-CARRIER-CD = 'J'
                   MOVE 15.00 TO CCOT-STRAP-AMT
               WHEN CCOM-CARRIER-CD = 'L'
                   MOVE 15.00 TO CCOT-STRAP-AMT
               WHEN OTHER
                   COMPUTE CCOT-STRAP-AMT ROUNDED = WS-WORK-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOT-STRAP-AMT > 0 AND CCOT-STRAP-AMT < 2.00
               MOVE 2.00 TO CCOT-STRAP-AMT
           END-IF
           IF CCOT-STRAP-AMT > 25.00
               MOVE 25.00 TO CCOT-STRAP-AMT
           END-IF
           ADD CCOT-STRAP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SHIPMENT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3610-FLAG-SHIPMENT.
           IF CCOT-SHIPMENT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOT-SHIPMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-RETRY.
           IF CCOM-RETRY-CNT < 9999999
               ADD 1 TO CCOM-RETRY-CNT
           END-IF
           IF CCOM-RETRY-CNT NOT < 10
               MOVE 'Y' TO CCOM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3630-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE FEE ON THE FEE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3640-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CCOT-VARIANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-HOLD-FLAG = 'Y'
                   COMPUTE CCOT-VARIANCE-AMT ROUNDED = CCOM-FEE-YTD *
                       CCOM-RATE / 200
               WHEN CCOM-SOURCE-CD = 'CFR'
                   MOVE 15.00 TO CCOT-VARIANCE-AMT
               WHEN CCOM-SOURCE-CD = 'DTX'
                   MOVE 10.00 TO CCOT-VARIANCE-AMT
               WHEN OTHER
                   COMPUTE CCOT-VARIANCE-AMT ROUNDED = CCOM-FEE-YTD *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOT-VARIANCE-AMT > 0 AND CCOT-VARIANCE-AMT < 5.00
               MOVE 5.00 TO CCOT-VARIANCE-AMT
           END-IF
           IF CCOT-VARIANCE-AMT > 25.00
               MOVE 25.00 TO CCOT-VARIANCE-AMT
           END-IF
           ADD CCOT-VARIANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE CURRENCY AMT BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3650-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-DENOM-CD
                   ADD CCOM-CURRENCY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-CURRENCY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CURRENCY FEE ON THE VARIANCE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3660-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CCOM-CURRENCY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-STOP-FLAG = 'Y'
                   COMPUTE CCOM-CURRENCY-AMT ROUNDED = CCOM-VARIANCE-YTD
                       * CCOM-RATE / 200
               WHEN CCOT-TRAN-CD = 'OR'
                   MOVE 10.00 TO CCOM-CURRENCY-AMT
               WHEN CCOT-TRAN-CD = 'SH'
                   MOVE 12.00 TO CCOM-CURRENCY-AMT
               WHEN OTHER
                   COMPUTE CCOM-CURRENCY-AMT ROUNDED = CCOM-VARIANCE-YTD
                       * CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-CURRENCY-AMT > 0 AND CCOM-CURRENCY-AMT < 2.00
               MOVE 2.00 TO CCOM-CURRENCY-AMT
           END-IF
           IF CCOM-CURRENCY-AMT > 150.00
               MOVE 150.00 TO CCOM-CURRENCY-AMT
           END-IF
           ADD CCOM-CURRENCY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E433 CONTACT REQUIRED
      * E697 COIN AMT NEGATIVE
      * E783 COIN AMT OVER 250000
      * E140 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3670-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CONTACT = SPACES
                   MOVE 'E433' TO WS-REJECT-CODE
               WHEN CCOM-COIN-AMT < 0
                   MOVE 'E697' TO WS-REJECT-CODE
               WHEN CCOM-COIN-AMT > 250000.00
                   MOVE 'E783' TO WS-REJECT-CODE
               WHEN CCOM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E140' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3680-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = CCOM-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CCOM-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE CCOM-SHIPMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-RETRY.
           IF CCOM-RETRY-CNT < 9999999
               ADD 1 TO CCOM-RETRY-CNT
           END-IF
           IF CCOM-RETRY-CNT NOT < 3
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3700-FLAG-SHIPMENT.
           IF CCOM-SHIPMENT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOM-SHIPMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * VARIANCE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3710-FLAG-VARIANCE.
           IF CCOM-VARIANCE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-VARIANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3720-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO CCOM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CURRENCY FEE ON THE SHIPMENT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3730-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CCOM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO CCOM-CURRENCY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-HOLD-FLAG = 'Y'
                   COMPUTE CCOM-CURRENCY-AMT ROUNDED = CCOM-SHIPMENT-AMT
                       * CCOM-FEE-PCT / 200
               WHEN CCOM-CARRIER-CD = 'J'
                   MOVE 3.00 TO CCOM-CURRENCY-AMT
               WHEN CCOM-CARRIER-CD = 'L'
                   MOVE 3.00 TO CCOM-CURRENCY-AMT
               WHEN OTHER
                   COMPUTE CCOM-CURRENCY-AMT ROUNDED = CCOM-SHIPMENT-AMT
                       * CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-CURRENCY-AMT > 0 AND CCOM-CURRENCY-AMT < 10.00
               MOVE 10.00 TO CCOM-CURRENCY-AMT
           END-IF
           IF CCOM-CURRENCY-AMT > 75.00
               MOVE 75.00 TO CCOM-CURRENCY-AMT
           END-IF
           ADD CCOM-CURRENCY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E661 NAME REQUIRED
      * E349 FEE YTD NEGATIVE
      * E395 FEE YTD OVER 50000
      * E889 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3740-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-NAME = SPACES
                   MOVE 'E661' TO WS-REJECT-CODE
               WHEN CCOM-FEE-YTD < 0
                   MOVE 'E349' TO WS-REJECT-CODE
               WHEN CCOM-FEE-YTD > 50000.00
                   MOVE 'E395' TO WS-REJECT-CODE
               WHEN CCOM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E889' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-REVIEW-DATE > 0 AND CCOM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARRIER CD:
      * J -> L WHEN LAST ACT DATE IS PAST
      * L -> M WHEN LAST ACT DATE IS PAST
      * M -> X WHEN LAST ACT DATE IS PAST
      * X -> J WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-TRAN.
           EVALUATE CCOT-TRAN-CD
               WHEN 'OR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'SH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RC'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'VR'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'AD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3780-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CCOM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CCOM-VARIANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   COMPUTE CCOM-VARIANCE-YTD ROUNDED = WS-WORK-AMT *
                       CCOM-RATE / 200
               WHEN CCOM-CARRIER-CD = 'J'
                   MOVE 7.00 TO CCOM-VARIANCE-YTD
               WHEN CCOM-CARRIER-CD = 'L'
                   MOVE 3.00 TO CCOM-VARIANCE-YTD
               WHEN OTHER
                   COMPUTE CCOM-VARIANCE-YTD ROUNDED = WS-WORK-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-VARIANCE-YTD > 0 AND CCOM-VARIANCE-YTD < 1.00
               MOVE 1.00 TO CCOM-VARIANCE-YTD
           END-IF
           IF CCOM-VARIANCE-YTD > 75.00
               MOVE 75.00 TO CCOM-VARIANCE-YTD
           END-IF
           ADD CCOM-VARIANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3790-DESCRIBE-STATUS.
           EVALUATE CCOM-STATUS-CD
               WHEN 'ADH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FVV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GCB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UZP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VXB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WTJ'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'XNG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CCOM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CCOM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT FEE ON THE STRAP AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3810-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CCOT-SHIPMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-STOP-FLAG = 'Y'
                   COMPUTE CCOT-SHIPMENT-AMT ROUNDED = CCOM-STRAP-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOM-STATUS-CD = 'ADH'
                   MOVE 7.00 TO CCOT-SHIPMENT-AMT
               WHEN CCOM-STATUS-CD = 'FVV'
                   MOVE 3.00 TO CCOT-SHIPMENT-AMT
               WHEN OTHER
                   COMPUTE CCOT-SHIPMENT-AMT ROUNDED = CCOM-STRAP-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOT-SHIPMENT-AMT > 0 AND CCOT-SHIPMENT-AMT < 1.00
               MOVE 1.00 TO CCOT-SHIPMENT-AMT
           END-IF
           IF CCOT-SHIPMENT-AMT > 35.00
               MOVE 35.00 TO CCOT-SHIPMENT-AMT
           END-IF
           ADD CCOT-SHIPMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E525 CONTACT REQUIRED
      * E424 CURRENCY AMT NEGATIVE
      * E591 CURRENCY AMT OVER 10000
      * E674 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3820-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CONTACT = SPACES
                   MOVE 'E525' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT < 0
                   MOVE 'E424' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT > 10000.00
                   MOVE 'E591' TO WS-REJECT-CODE
               WHEN CCOM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E674' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VARIANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO VARIANCE AMT.
      *----------------------------------------------------------------*
       3830-PRORATE-VARIANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-VARIANCE-AMT ROUNDED =
                   CCOM-VARIANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOM-VARIANCE-AMT
           END-IF
           ADD CCOM-VARIANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT FEE ON THE FEE YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3840-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CCOM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO CCOM-SHIPMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-STOP-FLAG = 'Y'
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOM-FEE-YTD *
                       CCOM-RATE / 200
               WHEN CCOT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO CCOM-SHIPMENT-AMT
               WHEN CCOT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO CCOM-SHIPMENT-AMT
               WHEN OTHER
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOM-FEE-YTD *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-SHIPMENT-AMT > 0 AND CCOM-SHIPMENT-AMT < 5.00
               MOVE 5.00 TO CCOM-SHIPMENT-AMT
           END-IF
           IF CCOM-SHIPMENT-AMT > 150.00
               MOVE 150.00 TO CCOM-SHIPMENT-AMT
           END-IF
           ADD CCOM-SHIPMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E471 CONTACT REQUIRED
      * E797 SHIPMENT AMT NEGATIVE
      * E092 SHIPMENT AMT OVER 5000
      * E204 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CONTACT = SPACES
                   MOVE 'E471' TO WS-REJECT-CODE
               WHEN CCOT-SHIPMENT-AMT < 0
                   MOVE 'E797' TO WS-REJECT-CODE
               WHEN CCOT-SHIPMENT-AMT > 5000.00
                   MOVE 'E092' TO WS-REJECT-CODE
               WHEN CCOT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E204' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COIN GRID TIER FOR THE SHIPMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3870-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-GRID-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CCOM-SHIPMENT-AMT NOT < WS-COIN-GRID-LOW(WS-TX)
                   MOVE WS-COIN-GRID-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * ADH -> FVV WHEN EXPIRY DATE IS PAST
      * FVV -> GCB WHEN EXPIRY DATE IS PAST
      * GCB -> UZP WHEN EXPIRY DATE IS PAST
      * UZP -> VXB WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-STATUS.
           EVALUATE CCOM-STATUS-CD
               WHEN 'ADH'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FVV' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FVV'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GCB' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GCB'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UZP' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UZP'
                   IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VXB' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VARIANCE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3890-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = CCOM-VARIANCE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CCOM-VARIANCE-YTD - (WS-WORK-AMT * 4)
           COMPUTE CCOM-SHIPMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VARIANCE YTD BY CARRIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3900-ACCUM-CARRIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CARRIER-CD
                   ADD CCOM-VARIANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CARRIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-VARIANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-VARIANCE.
           IF CCOT-VARIANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-VARIANCE-AMT * CCOM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-CURRENCY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3920-DESCRIBE-REASON.
           EVALUATE CCOM-REASON-CD
               WHEN 'JL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'LW'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3930-COUNT-ITEM.
           IF CCOM-ITEM-CNT < 9999999
               ADD 1 TO CCOM-ITEM-CNT
           END-IF
           IF CCOM-ITEM-CNT NOT < 3
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3940-FLAG-FEE.
           IF CCOM-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * JL -> LW WHEN NEXT DUE DATE IS PAST
      * LW -> PS WHEN NEXT DUE DATE IS PAST
      * PS -> JL WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3950-SET-REASON.
           EVALUATE CCOM-REASON-CD
               WHEN 'JL'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LW' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LW'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE FEE ON THE VARIANCE YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3960-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CCOM-VARIANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   COMPUTE CCOM-VARIANCE-AMT ROUNDED = CCOM-VARIANCE-YTD
                       * CCOM-RATE / 200
               WHEN CCOT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO CCOM-VARIANCE-AMT
               WHEN CCOT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO CCOM-VARIANCE-AMT
               WHEN OTHER
                   COMPUTE CCOM-VARIANCE-AMT ROUNDED = CCOM-VARIANCE-YTD
                       * CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-VARIANCE-AMT > 0 AND CCOM-VARIANCE-AMT < 2.00
               MOVE 2.00 TO CCOM-VARIANCE-AMT
           END-IF
           IF CCOM-VARIANCE-AMT > 75.00
               MOVE 75.00 TO CCOM-VARIANCE-AMT
           END-IF
           ADD CCOM-VARIANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3970-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CURRENCY AMT BY CARRIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3980-ACCUM-CARRIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CARRIER-CD
                   ADD CCOM-CURRENCY-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CARRIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-CURRENCY-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3990-ACCRUE-SHIPMENT.
           IF CCOT-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-SHIPMENT-AMT * CCOM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CCOM-STRAP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4000-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE CCOT-STRAP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4010-LIMIT-SHIPMENT.
           IF CCOM-SHIPMENT-YTD < 100.00
               MOVE 100.00 TO CCOM-SHIPMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-SHIPMENT-YTD > 2500.00
               MOVE 2500.00 TO CCOM-SHIPMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STRAP AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4020-FLAG-STRAP.
           IF CCOM-STRAP-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-STRAP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4030-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COIN GRID TIER FOR THE VARIANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4040-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-GRID-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CCOM-VARIANCE-YTD NOT < WS-COIN-GRID-LOW(WS-TX)
                   MOVE WS-COIN-GRID-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STRAP AMT.
      *----------------------------------------------------------------*
       4050-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-STRAP-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOT-STRAP-AMT
           END-IF
           ADD CCOT-STRAP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CURRENCY YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4060-LIMIT-CURRENCY.
           IF CCOM-CURRENCY-YTD < 0.00
               MOVE 0.00 TO CCOM-CURRENCY-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-CURRENCY-YTD > 500.00
               MOVE 500.00 TO CCOM-CURRENCY-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CARRIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4070-DESCRIBE-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'M'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * OR -> SH WHEN EFF DATE IS PAST
      * SH -> RC WHEN EFF DATE IS PAST
      * RC -> VR WHEN EFF DATE IS PAST
      * VR -> AD WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4080-SET-TRAN.
           EVALUATE CCOT-TRAN-CD
               WHEN 'OR'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4090-FLAG-FEE.
           IF CCOM-FEE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT FEE ON THE SHIPMENT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4100-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CCOM-SHIPMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOT-SHIPMENT-AMT
                       * CCOM-PENALTY-PCT / 200
               WHEN CCOM-STATUS-CD = 'ADH'
                   MOVE 15.00 TO CCOM-SHIPMENT-AMT
               WHEN CCOM-STATUS-CD = 'FVV'
                   MOVE 12.00 TO CCOM-SHIPMENT-AMT
               WHEN OTHER
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOT-SHIPMENT-AMT
                       * CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-SHIPMENT-AMT > 0 AND CCOM-SHIPMENT-AMT < 5.00
               MOVE 5.00 TO CCOM-SHIPMENT-AMT
           END-IF
           IF CCOM-SHIPMENT-AMT > 150.00
               MOVE 150.00 TO CCOM-SHIPMENT-AMT
           END-IF
           ADD CCOM-SHIPMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE COIN TIER TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4110-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-TIER-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CCOM-FEE-AMT NOT < WS-COIN-TIER-LOW(WS-TX)
                   MOVE WS-COIN-TIER-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STRAP YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4120-FLAG-STRAP.
           IF CCOM-STRAP-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOM-STRAP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STRAP YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4130-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = CCOM-STRAP-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-STRAP-YTD - (WS-WORK-AMT * 12)
           COMPUTE CCOT-SHIPMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPMENT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4140-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CHANNEL-CD
                   ADD CCOT-SHIPMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOT-SHIPMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4150-COUNT-RETRY.
           IF CCOM-RETRY-CNT < 9999999
               ADD 1 TO CCOM-RETRY-CNT
           END-IF
           IF CCOM-RETRY-CNT NOT < 6
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE YTD FEE ON THE FEE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4160-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CCOM-VARIANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CCOM-VARIANCE-YTD ROUNDED = CCOM-FEE-YTD *
                       CCOM-FEE-PCT / 200
               WHEN CCOM-REASON-CD = 'JL'
                   MOVE 3.00 TO CCOM-VARIANCE-YTD
               WHEN CCOM-REASON-CD = 'LW'
                   MOVE 12.00 TO CCOM-VARIANCE-YTD
               WHEN OTHER
                   COMPUTE CCOM-VARIANCE-YTD ROUNDED = CCOM-FEE-YTD *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-VARIANCE-YTD > 0 AND CCOM-VARIANCE-YTD < 2.00
               MOVE 2.00 TO CCOM-VARIANCE-YTD
           END-IF
           IF CCOM-VARIANCE-YTD > 250.00
               MOVE 250.00 TO CCOM-VARIANCE-YTD
           END-IF
           ADD CCOM-VARIANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E694 CONTACT REQUIRED
      * E267 COIN AMT NEGATIVE
      * E915 COIN AMT OVER 10000
      * E742 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4170-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-CONTACT = SPACES
                   MOVE 'E694' TO WS-REJECT-CODE
               WHEN CCOM-COIN-AMT < 0
                   MOVE 'E267' TO WS-REJECT-CODE
               WHEN CCOM-COIN-AMT > 10000.00
                   MOVE 'E915' TO WS-REJECT-CODE
               WHEN CCOM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E742' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COIN TIER TIER FOR THE CURRENCY YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4180-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-TIER-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CCOM-CURRENCY-YTD NOT < WS-COIN-TIER-LOW(WS-TX)
                   MOVE WS-COIN-TIER-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4190-DESCRIBE-STATUS.
           EVALUATE CCOM-STATUS-CD
               WHEN 'ADH'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FVV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GCB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UZP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'VXB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WTJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'XNG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4200-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF CCOM-OPEN-DATE > 0 AND CCOM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4210-DESCRIBE-TRAN.
           EVALUATE CCOT-TRAN-CD
               WHEN 'OR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RC'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'VR'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'AD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE DENOM CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4220-DESCRIBE-DENOM.
           EVALUATE CCOM-DENOM-CD
               WHEN 'B'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'P'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'S'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * JL -> LW WHEN MATURITY DATE IS PAST
      * LW -> PS WHEN MATURITY DATE IS PAST
      * PS -> JL WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4230-SET-REASON.
           EVALUATE CCOM-REASON-CD
               WHEN 'JL'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LW' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LW'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4240-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-REVIEW-DATE > 0 AND CCOM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4250-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CCOM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CCOM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 30 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4260-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CCOM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * VARIANCE AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4270-LIMIT-VARIANCE.
           IF CCOM-VARIANCE-AMT < 50.00
               MOVE 50.00 TO CCOM-VARIANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-VARIANCE-AMT > 5000.00
               MOVE 5000.00 TO CCOM-VARIANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4280-ACCRUE-SHIPMENT.
           IF CCOM-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-SHIPMENT-AMT * CCOM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-CURRENCY-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4290-ACCRUE-VARIANCE.
           IF CCOT-VARIANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-VARIANCE-AMT * CCOM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CCOM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VARIANCE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4300-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = CCOM-VARIANCE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = CCOM-VARIANCE-AMT - (WS-WORK-AMT * 2)
           COMPUTE CCOM-VARIANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4310-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4320-DESCRIBE-CHANNEL.
           EVALUATE CCOM-CHANNEL-CD
               WHEN 'AW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DW'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT VARIANCE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4330-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = CCOM-VARIANCE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-VARIANCE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE CCOM-SHIPMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COIN TIER TIER FOR THE SHIPMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4340-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-TIER-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 8
               IF CCOM-SHIPMENT-YTD NOT < WS-COIN-TIER-LOW(WS-TX)
                   MOVE WS-COIN-TIER-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COIN YTD FEE ON THE STRAP AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4350-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO CCOM-COIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-HOLD-FLAG = 'Y'
                   COMPUTE CCOM-COIN-YTD ROUNDED = CCOM-STRAP-AMT *
                       CCOM-RATE / 200
               WHEN CCOM-STATUS-CD = 'ADH'
                   MOVE 20.00 TO CCOM-COIN-YTD
               WHEN CCOM-STATUS-CD = 'FVV'
                   MOVE 20.00 TO CCOM-COIN-YTD
               WHEN OTHER
                   COMPUTE CCOM-COIN-YTD ROUNDED = CCOM-STRAP-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-COIN-YTD > 0 AND CCOM-COIN-YTD < 5.00
               MOVE 5.00 TO CCOM-COIN-YTD
           END-IF
           IF CCOM-COIN-YTD > 75.00
               MOVE 75.00 TO CCOM-COIN-YTD
           END-IF
           ADD CCOM-COIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE CURRENCY AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VARIANCE YTD.
      *----------------------------------------------------------------*
       4360-PRORATE-VARIANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-VARIANCE-YTD ROUNDED =
                   CCOM-CURRENCY-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-VARIANCE-YTD
           END-IF
           ADD CCOM-VARIANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4370-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE CCOM-VARIANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHIPMENT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4380-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-REASON-CD
                   ADD CCOM-SHIPMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-SHIPMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CURRENCY SCHED TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4390-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CURRENCY-SCHED-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF CCOM-FEE-YTD NOT < WS-CURRENCY-SCHED-LOW(WS-TX)
                   MOVE WS-CURRENCY-SCHED-RATE(WS-TX) TO
                       CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CURRENCY YTD.
      *----------------------------------------------------------------*
       4400-PRORATE-CURRENCY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-CURRENCY-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOM-CURRENCY-YTD
           END-IF
           ADD CCOM-CURRENCY-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4410-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STRAP AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4420-ACCRUE-STRAP.
           IF CCOM-STRAP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-STRAP-AMT * CCOM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CCOT-VARIANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * JL -> LW WHEN LAST ACT DATE IS PAST
      * LW -> PS WHEN LAST ACT DATE IS PAST
      * PS -> JL WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4430-SET-REASON.
           EVALUATE CCOM-REASON-CD
               WHEN 'JL'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LW' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LW'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VARIANCE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4440-FLAG-VARIANCE.
           IF CCOM-VARIANCE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-VARIANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DENOM CD:
      * B -> D WHEN NEXT REVIEW DATE IS PAST
      * D -> P WHEN NEXT REVIEW DATE IS PAST
      * P -> S WHEN NEXT REVIEW DATE IS PAST
      * S -> W WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4450-SET-DENOM.
           EVALUATE CCOM-DENOM-CD
               WHEN 'B'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'D' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'P' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'S' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF CCOM-NEXT-REVIEW-DATE > 0 AND
                       CCOM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'W' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4460-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4470-COUNT-SEQ.
           IF CCOT-SEQ < 9999999
               ADD 1 TO CCOT-SEQ
           END-IF
           IF CCOT-SEQ NOT < 10
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE FEE ON THE SHIPMENT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4480-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CCOT-VARIANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CCOT-VARIANCE-AMT ROUNDED = CCOM-SHIPMENT-YTD
                       * CCOM-RATE / 200
               WHEN CCOT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO CCOT-VARIANCE-AMT
               WHEN CCOT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO CCOT-VARIANCE-AMT
               WHEN OTHER
                   COMPUTE CCOT-VARIANCE-AMT ROUNDED = CCOM-SHIPMENT-YTD
                       * CCOM-RATE / 100
           END-EVALUATE
           IF CCOT-VARIANCE-AMT > 0 AND CCOT-VARIANCE-AMT < 2.00
               MOVE 2.00 TO CCOT-VARIANCE-AMT
           END-IF
           IF CCOT-VARIANCE-AMT > 250.00
               MOVE 250.00 TO CCOT-VARIANCE-AMT
           END-IF
           ADD CCOT-VARIANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * VARIANCE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4490-FLAG-VARIANCE.
           IF CCOM-VARIANCE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-VARIANCE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4500-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPMENT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4510-FLAG-SHIPMENT.
           IF CCOM-SHIPMENT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-SHIPMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * ADH -> FVV WHEN NEXT DUE DATE IS PAST
      * FVV -> GCB WHEN NEXT DUE DATE IS PAST
      * GCB -> UZP WHEN NEXT DUE DATE IS PAST
      * UZP -> VXB WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4520-SET-STATUS.
           EVALUATE CCOM-STATUS-CD
               WHEN 'ADH'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FVV' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FVV'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GCB' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GCB'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UZP' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UZP'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VXB' TO CCOM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE CCO FIELDS TO CNPEN AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-CNPEN.
           INITIALIZE CNPEN-PARMS
           MOVE CCOM-SHIPMENT-AMT
             TO CNPEN-SHIPMENT-AMT
           MOVE CCOM-COIN-AMT
             TO CNPEN-COIN-AMT
           MOVE CCOM-STRAP-YTD
             TO CNPEN-STRAP-YTD
           MOVE CCOM-SHIPMENT-YTD
             TO CNPEN-SHIPMENT-YTD
           MOVE CCOM-CURRENCY-YTD
             TO CNPEN-CURRENCY-YTD
           MOVE CCOM-FEE-AMT
             TO CNPEN-FEE-AMT
           MOVE CCOM-NOTICE-DATE
             TO CNPEN-NOTICE-DATE
           MOVE CCOM-NEXT-REVIEW-DATE
             TO CNPEN-NEXT-REVIEW-DATE
           MOVE CCOM-LAST-PAY-DATE
             TO CNPEN-LAST-PAY-DATE
           MOVE CCOM-MATURITY-DATE
             TO CNPEN-MATURITY-DATE
           MOVE CCOM-CARRIER-CD
             TO CNPEN-CARRIER-CD
           MOVE CCOM-REASON-CD
             TO CNPEN-REASON-CD
           MOVE CCOM-SOURCE-CD
             TO CNPEN-SOURCE-CD
           MOVE CCOM-CHANNEL-CD
             TO CNPEN-CHANNEL-CD
           MOVE CCOM-STOP-FLAG
             TO CNPEN-STOP-FLAG
           MOVE CCOM-HOLD-FLAG
             TO CNPEN-HOLD-FLAG
           MOVE CCOM-AUTO-RENEW-FLAG
             TO CNPEN-AUTO-RENEW-FLAG
           MOVE CCOM-RETRY-CNT
             TO CNPEN-RETRY-CNT
           MOVE CCOM-ITEM-CNT
             TO CNPEN-ITEM-CNT
           MOVE CCOM-RATE
             TO CNPEN-RATE
           MOVE CCOM-PENALTY-PCT
             TO CNPEN-PENALTY-PCT
           MOVE CCOM-REF-NO
             TO CNPEN-REF-NO
           MOVE CCOM-CONTACT
             TO CNPEN-CONTACT
           CALL 'CNPEN' USING CNPEN-PARMS
           EVALUATE CNPEN-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CNPEN-SHIPMENT-AMT
                     TO CCOM-SHIPMENT-AMT
                   MOVE CNPEN-STRAP-YTD
                     TO CCOM-STRAP-YTD
                   MOVE CNPEN-CURRENCY-YTD
                     TO CCOM-CURRENCY-YTD
                   MOVE CNPEN-FEE-AMT
                     TO CCOM-FEE-AMT
                   MOVE CNPEN-STOP-FLAG
                     TO CCOM-STOP-FLAG
                   MOVE CNPEN-HOLD-FLAG
                     TO CCOM-HOLD-FLAG
                   MOVE CNPEN-AUTO-RENEW-FLAG
                     TO CCOM-AUTO-RENEW-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-CNPEN' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE CCO FIELDS TO CNDTE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-CNDTE.
           INITIALIZE CNDTE-PARMS
           MOVE CCOM-SHIPMENT-AMT
             TO CNDTE-SHIPMENT-AMT
           MOVE CCOM-FEE-AMT
             TO CNDTE-FEE-AMT
           MOVE CCOM-COIN-YTD
             TO CNDTE-COIN-YTD
           MOVE CCOM-FEE-YTD
             TO CNDTE-FEE-YTD
           MOVE CCOM-COIN-AMT
             TO CNDTE-COIN-AMT
           MOVE CCOM-CURRENCY-YTD
             TO CNDTE-CURRENCY-YTD
           MOVE CCOM-EFFECTIVE-DATE
             TO CNDTE-EFFECTIVE-DATE
           MOVE CCOM-LAST-ACT-DATE
             TO CNDTE-LAST-ACT-DATE
           MOVE CCOM-NEXT-REVIEW-DATE
             TO CNDTE-NEXT-REVIEW-DATE
           MOVE CCOM-MATURITY-DATE
             TO CNDTE-MATURITY-DATE
           MOVE CCOM-CHANNEL-CD
             TO CNDTE-CHANNEL-CD
           MOVE CCOM-DENOM-CD
             TO CNDTE-DENOM-CD
           MOVE CCOM-STATUS-CD
             TO CNDTE-STATUS-CD
           MOVE CCOM-CARRIER-CD
             TO CNDTE-CARRIER-CD
           MOVE CCOM-NOTICE-SENT-FLAG
             TO CNDTE-NOTICE-SENT-FLAG
           MOVE CCOM-HOLD-FLAG
             TO CNDTE-HOLD-FLAG
           MOVE CCOM-AUTO-RENEW-FLAG
             TO CNDTE-AUTO-RENEW-FLAG
           MOVE CCOM-ITEM-CNT
             TO CNDTE-ITEM-CNT
           MOVE CCOM-NOTICE-CNT
             TO CNDTE-NOTICE-CNT
           MOVE CCOM-PENALTY-PCT
             TO CNDTE-PENALTY-PCT
           MOVE CCOM-FEE-PCT
             TO CNDTE-FEE-PCT
           MOVE CCOM-CUST-ID
             TO CNDTE-CUST-ID
           MOVE CCOM-DESC
             TO CNDTE-DESC
           CALL 'CNDTE' USING CNDTE-PARMS
           EVALUATE CNDTE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CNDTE-COIN-AMT
                     TO CCOM-COIN-AMT
                   MOVE CNDTE-CURRENCY-YTD
                     TO CCOM-CURRENCY-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RDTE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-CNDTE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE CCOT-KEY TO RD-KEY
           MOVE CCOT-TRAN-CD TO RD-CODE
           MOVE CCOT-COIN-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE CCOT-KEY TO RD-KEY
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
           DISPLAY 'CNORCON READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CNORCON' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
