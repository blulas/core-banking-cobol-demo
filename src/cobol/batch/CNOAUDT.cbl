      *================================================================*
      * PROGRAM  : CNOAUDT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : COIN AND CURRENCY - AUDIT TRAIL. READS THE CCO      *
      *            TRANSACTIONS, UPDATES THE CCO MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. BRANCH COIN AND CURRENCY ORDERS *
      *            AND ARMORED CARRIER SHIPMENT                        *
      * INPUT    : CCOMST (KSDS), CCOTRN, BANKCTL                      *
      * OUTPUT   : CCOOUT, CNOAURPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CNCAL, CNPEN            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-01-07 RJT  ORIGINAL                                       *
      * 1986-04-27 JLM  COIN CALCULATION CORRECTED PRB8321             *
      * 1988-09-16 PQR  FEE CALCULATION CORRECTED PRB1236              *
      * 1992-05-25 JLM  NEW VARIANCE REPORT COLUMNS PRB2343            *
      * 1993-06-01 MKL  VARIANCE ROUNDING - HALF UP PRB9248            *
      * 2000-03-04 LMB  STRAP CUTOFF MOVED INC5443                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CNOAUDT.
       AUTHOR. T W HOLT.

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
           SELECT REPORT-FILE ASSIGN TO "CNOAURPT"
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
                                           VALUE 19.000000.
           05  WS-DAYS                     PIC S9(07) VALUE 0.
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 31.
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
      * WS-CURRENCY-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CURRENCY-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000702000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000708000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000535000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000197000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008100000000184500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008350000000559500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008600000000326500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011100000000240500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013600000000078000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013700000000430500'.
       01  WS-CURRENCY-TIER-TBL REDEFINES WS-CURRENCY-TIER-VALUES.
           05  WS-CURRENCY-TIER-ROW OCCURS 10 TIMES.
               10  WS-CURRENCY-TIER-LOW    PIC 9(09)V99.
               10  WS-CURRENCY-TIER-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-CURRENCY-TIERX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-CURRENCY-TIERX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000826000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000054000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000117500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003100000000662000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005600000000342500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005850000000744500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010850000000311000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010900000000282500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020900000000613000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000344500'.
       01  WS-CURRENCY-TIERX-TBL REDEFINES WS-CURRENCY-TIERX-VALUES.
           05  WS-CURRENCY-TIERX-ROW OCCURS 10 TIMES.
               10  WS-CURRENCY-TIERX-LOW   PIC 9(09)V99.
               10  WS-CURRENCY-TIERX-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-VARIANCE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-VARIANCE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000802000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000653000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000426000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000364000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000610000'.
       01  WS-VARIANCE-SCHED-TBL REDEFINES WS-VARIANCE-SCHED-VALUES.
           05  WS-VARIANCE-SCHED-ROW OCCURS 5 TIMES.
               10  WS-VARIANCE-SCHED-LOW   PIC 9(09)V99.
               10  WS-VARIANCE-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COIN-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COIN-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000529000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000568000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000342000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021000000000482000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031000000000442500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031250000000791500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031350000000545500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00041350000000717000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00046350000000696500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00046600000000728000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00056600000000340500'.
       01  WS-COIN-BAND-TBL REDEFINES WS-COIN-BAND-VALUES.
           05  WS-COIN-BAND-ROW OCCURS 11 TIMES.
               10  WS-COIN-BAND-LOW        PIC 9(09)V99.
               10  WS-COIN-BAND-RATE       PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CNOAUDT  COIN AND CURRENCY'.
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
       COPY CNCALP.
       COPY CNPENP.

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
               PERFORM 3000-PRORATE-VARIANCE
               PERFORM 3010-LIMIT-VARIANCE
               PERFORM 3020-ASSESS-AUTO-RENEW
               PERFORM 3030-SET-DENOM
               PERFORM 3040-LIMIT-SHIPMENT
               PERFORM 3050-PRORATE-STRAP
               PERFORM 3060-SELECT-FEE-PCT
               PERFORM 3070-ASSESS-AUTO-RENEW
               PERFORM 3080-FLAG-FEE
               PERFORM 3090-ASSESS-HOLD
               PERFORM 3100-ASSESS-AUTO-RENEW
               PERFORM 3110-PRORATE-STRAP
               PERFORM 3120-DESCRIBE-CHANNEL
               PERFORM 3130-PRORATE-SHIPMENT
               PERFORM 3140-AGE-NOTICE
               PERFORM 3150-AGE-LAST-PAY
               PERFORM 3160-ACCRUE-FEE
               PERFORM 3170-CHECK-OPEN
               PERFORM 3180-SET-CHANNEL
               PERFORM 3190-SELECT-RATE
               PERFORM 3200-SPLIT-STRAP
               PERFORM 3210-COUNT-ITEM
               PERFORM 3220-EDIT-ADDR
               PERFORM 3230-ASSESS-AUTO-RENEW
               PERFORM 3240-ACCUM-STATUS
               PERFORM 3250-ACCUM-TRAN
               PERFORM 3260-SET-REASON
               PERFORM 3270-PRORATE-SHIPMENT
               PERFORM 3280-COUNT-SEQ
               PERFORM 3290-COUNT-ITEM
               PERFORM 3300-AGE-NEXT-DUE
               PERFORM 3310-FLAG-ITEM
               PERFORM 3320-AGE-LAST-PAY
               PERFORM 3330-DESCRIBE-DENOM
               PERFORM 3340-SELECT-RATE
               PERFORM 3350-LIMIT-FEE
               PERFORM 3360-SPLIT-CURRENCY
               PERFORM 3370-DESCRIBE-CHANNEL
               PERFORM 3380-COUNT-NOTICE
               PERFORM 3390-PRORATE-FEE
               PERFORM 3400-LIMIT-COIN
               PERFORM 3410-CHECK-NEXT-DUE
               PERFORM 3420-ASSESS-AUTO-RENEW
               PERFORM 3430-FLAG-SHIPMENT
               PERFORM 3440-SET-TRAN
               PERFORM 3450-SET-TRAN
               PERFORM 3460-DESCRIBE-SOURCE
               PERFORM 3470-ASSESS-EXEMPT
               PERFORM 3480-SELECT-RATE
               PERFORM 3490-ACCRUE-SHIPMENT
               PERFORM 3500-EDIT-OPERATOR
               PERFORM 3510-ACCRUE-COIN
               PERFORM 3520-COUNT-SEQ
               PERFORM 3530-DESCRIBE-CHANNEL
               PERFORM 3540-FLAG-SHIPMENT
               PERFORM 3550-FLAG-COIN
               PERFORM 3560-ACCRUE-STRAP
               PERFORM 3570-SPLIT-SHIPMENT
               PERFORM 3580-DESCRIBE-STATUS
               PERFORM 3590-ACCUM-CHANNEL
               PERFORM 3600-EDIT-ADDR
               PERFORM 3610-LIMIT-SHIPMENT
               PERFORM 3620-PRORATE-COIN
               PERFORM 3630-SELECT-RATE
               PERFORM 3640-CHECK-EFF
               PERFORM 3650-ACCRUE-VARIANCE
               PERFORM 3660-SELECT-PENALTY-PCT
               PERFORM 3670-FLAG-COIN
               PERFORM 3680-LIMIT-VARIANCE
               PERFORM 3690-DESCRIBE-SOURCE
               PERFORM 3700-ASSESS-STOP
               PERFORM 3710-SELECT-RATE
               PERFORM 3720-SET-CHANNEL
               PERFORM 3730-DESCRIBE-SOURCE
               PERFORM 3740-CHECK-OPEN
               PERFORM 3750-COUNT-NOTICE
               PERFORM 3760-ASSESS-REVERSAL
               PERFORM 3770-ASSESS-NOTICE-SENT
               PERFORM 3780-CHECK-EFF
               PERFORM 3790-DESCRIBE-STATUS
               PERFORM 3800-ASSESS-REVERSAL
               PERFORM 3810-ACCRUE-RATE
               PERFORM 3820-PRORATE-SHIPMENT
               PERFORM 3830-DESCRIBE-DENOM
               PERFORM 3840-DESCRIBE-DENOM
               PERFORM 3850-SPLIT-STRAP
               PERFORM 3860-ACCRUE-VARIANCE
               PERFORM 3870-ACCRUE-FEE
               PERFORM 3880-COUNT-RETRY
               PERFORM 3890-ACCUM-CARRIER
               PERFORM 3900-FLAG-STRAP
               PERFORM 3910-COUNT-ITEM
               PERFORM 3920-AGE-OPEN
               PERFORM 3930-SELECT-RATE
               PERFORM 3940-PRORATE-SHIPMENT
               PERFORM 3950-ACCRUE-CURRENCY
               PERFORM 3960-LIMIT-SHIPMENT
               PERFORM 3970-LIMIT-STRAP
               PERFORM 3980-ACCRUE-SHIPMENT
               PERFORM 3990-EDIT-DESC
               PERFORM 4000-AGE-EXPIRY
               PERFORM 4010-EDIT-OPERATOR
               PERFORM 4020-DESCRIBE-CHANNEL
               PERFORM 4030-AGE-EXPIRY
               PERFORM 4040-AGE-OPEN
               PERFORM 4050-EDIT-REF-NO
               PERFORM 4060-ACCUM-REASON
               PERFORM 4070-AGE-EFF
               PERFORM 4080-SELECT-PENALTY-PCT
               PERFORM 4090-CHECK-EFF
               PERFORM 4100-SET-CHANNEL
               PERFORM 4110-AGE-NEXT-REVIEW
               PERFORM 4120-SELECT-RATE
               PERFORM 4130-ASSESS-EXEMPT
               PERFORM 4140-COUNT-ITEM
               PERFORM 4150-AGE-EFF
               PERFORM 4160-COUNT-NOTICE
               PERFORM 4170-COUNT-SEQ
               PERFORM 4180-CHECK-NEXT-DUE
               PERFORM 4190-EDIT-REF-NO
               PERFORM 4200-LIMIT-ITEM
               PERFORM 4210-DESCRIBE-CHANNEL
               PERFORM 4220-ACCRUE-SHIPMENT
               PERFORM 4230-LIMIT-STRAP
               PERFORM 4240-SET-REASON
               PERFORM 4250-AGE-EFFECTIVE
               PERFORM 4260-FLAG-VARIANCE
               PERFORM 4270-ACCRUE-SHIPMENT
               PERFORM 4280-ACCRUE-SHIPMENT
               PERFORM 4290-CHECK-NEXT-REVIEW
               PERFORM 4300-PRORATE-CURRENCY
               PERFORM 4310-LIMIT-STRAP
               PERFORM 4320-SPLIT-COIN
               PERFORM 4330-ACCRUE-COIN
               PERFORM 4340-AGE-EFF
               PERFORM 4350-ASSESS-EXEMPT
               PERFORM 4360-PRORATE-STRAP
               PERFORM 4370-COUNT-RETRY
               PERFORM 4380-SPLIT-FEE
               PERFORM 4390-LIMIT-SHIPMENT
               PERFORM 4400-ACCUM-CARRIER
               PERFORM 4410-CHECK-LAST-ACT
               PERFORM 4420-CHECK-NOTICE
               PERFORM 4430-SELECT-FEE-PCT
               PERFORM 4440-EDIT-OPERATOR
               PERFORM 4450-ACCRUE-SHIPMENT
               PERFORM 4460-COUNT-CYCLE
               PERFORM 4470-ACCUM-CHANNEL
               PERFORM 4480-COUNT-CYCLE
               PERFORM 4490-SET-CHANNEL
               PERFORM 4500-AGE-EXPIRY
               PERFORM 4510-CHECK-LAST-ACT
               PERFORM 4520-FLAG-COIN
               PERFORM 4530-CHECK-NOTICE
               PERFORM 4540-LIMIT-ITEM
               PERFORM 4550-ACCRUE-FEE
               PERFORM 4560-DESCRIBE-CHANNEL
               PERFORM 7020-CALL-CNCAL
               PERFORM 7010-CALL-CNPEN
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
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO VARIANCE AMT.
      *----------------------------------------------------------------*
       3000-PRORATE-VARIANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-VARIANCE-AMT ROUNDED =
                   CCOM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-VARIANCE-AMT
           END-IF
           ADD CCOM-VARIANCE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * VARIANCE AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-VARIANCE.
           IF CCOT-VARIANCE-AMT < 0.00
               MOVE 0.00 TO CCOT-VARIANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-VARIANCE-AMT > 10000.00
               MOVE 10000.00 TO CCOT-VARIANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COIN FEE ON THE SHIPMENT AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3020-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-COIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   COMPUTE CCOM-COIN-AMT ROUNDED = CCOM-SHIPMENT-AMT *
                       CCOM-RATE / 200
               WHEN CCOM-CHANNEL-CD = 'AW'
                   MOVE 20.00 TO CCOM-COIN-AMT
               WHEN CCOM-CHANNEL-CD = 'DW'
                   MOVE 20.00 TO CCOM-COIN-AMT
               WHEN OTHER
                   COMPUTE CCOM-COIN-AMT ROUNDED = CCOM-SHIPMENT-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-COIN-AMT > 0 AND CCOM-COIN-AMT < 5.00
               MOVE 5.00 TO CCOM-COIN-AMT
           END-IF
           IF CCOM-COIN-AMT > 35.00
               MOVE 35.00 TO CCOM-COIN-AMT
           END-IF
           ADD CCOM-COIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DENOM CD:
      * B -> D WHEN EFF DATE IS PAST
      * D -> P WHEN EFF DATE IS PAST
      * P -> S WHEN EFF DATE IS PAST
      * S -> W WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3030-SET-DENOM.
           EVALUATE CCOM-DENOM-CD
               WHEN 'B'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'P' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'P'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO CCOM-DENOM-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPMENT AMT IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3040-LIMIT-SHIPMENT.
           IF CCOT-SHIPMENT-AMT < 0.00
               MOVE 0.00 TO CCOT-SHIPMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-SHIPMENT-AMT > 10000.00
               MOVE 10000.00 TO CCOT-SHIPMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE STRAP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STRAP YTD.
      *----------------------------------------------------------------*
       3050-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-STRAP-YTD ROUNDED =
                   CCOT-STRAP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOM-STRAP-YTD
           END-IF
           ADD CCOM-STRAP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CURRENCY TIER TIER FOR THE CURRENCY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3060-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CURRENCY-TIER-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CCOM-CURRENCY-AMT NOT < WS-CURRENCY-TIER-LOW(WS-TX)
                   MOVE WS-CURRENCY-TIER-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CURRENCY YTD FEE ON THE SHIPMENT YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3070-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-CURRENCY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   COMPUTE CCOM-CURRENCY-YTD ROUNDED = CCOM-SHIPMENT-YTD
                       * CCOM-PENALTY-PCT / 200
               WHEN CCOM-CARRIER-CD = 'J'
                   MOVE 10.00 TO CCOM-CURRENCY-YTD
               WHEN CCOM-CARRIER-CD = 'L'
                   MOVE 5.00 TO CCOM-CURRENCY-YTD
               WHEN OTHER
                   COMPUTE CCOM-CURRENCY-YTD ROUNDED = CCOM-SHIPMENT-YTD
                       * CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-CURRENCY-YTD > 0 AND CCOM-CURRENCY-YTD < 10.00
               MOVE 10.00 TO CCOM-CURRENCY-YTD
           END-IF
           IF CCOM-CURRENCY-YTD > 35.00
               MOVE 35.00 TO CCOM-CURRENCY-YTD
           END-IF
           ADD CCOM-CURRENCY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3080-FLAG-FEE.
           IF CCOM-FEE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT FEE ON THE STRAP YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3090-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CCOM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CCOT-SHIPMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   COMPUTE CCOT-SHIPMENT-AMT ROUNDED = CCOM-STRAP-YTD *
                       CCOM-RATE / 200
               WHEN CCOM-CARRIER-CD = 'J'
                   MOVE 12.00 TO CCOT-SHIPMENT-AMT
               WHEN CCOM-CARRIER-CD = 'L'
                   MOVE 15.00 TO CCOT-SHIPMENT-AMT
               WHEN OTHER
                   COMPUTE CCOT-SHIPMENT-AMT ROUNDED = CCOM-STRAP-YTD *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOT-SHIPMENT-AMT > 0 AND CCOT-SHIPMENT-AMT < 10.00
               MOVE 10.00 TO CCOT-SHIPMENT-AMT
           END-IF
           IF CCOT-SHIPMENT-AMT > 25.00
               MOVE 25.00 TO CCOT-SHIPMENT-AMT
           END-IF
           ADD CCOT-SHIPMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT FEE ON THE WORK AMT 2.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3100-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-SHIPMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       CCOM-RATE / 200
               WHEN CCOM-CARRIER-CD = 'J'
                   MOVE 3.00 TO CCOM-SHIPMENT-AMT
               WHEN CCOM-CARRIER-CD = 'L'
                   MOVE 10.00 TO CCOM-SHIPMENT-AMT
               WHEN OTHER
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = WS-WORK-AMT-2 *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-SHIPMENT-AMT > 0 AND CCOM-SHIPMENT-AMT < 2.00
               MOVE 2.00 TO CCOM-SHIPMENT-AMT
           END-IF
           IF CCOM-SHIPMENT-AMT > 100.00
               MOVE 100.00 TO CCOM-SHIPMENT-AMT
           END-IF
           ADD CCOM-SHIPMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE VARIANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STRAP AMT.
      *----------------------------------------------------------------*
       3110-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-STRAP-AMT ROUNDED =
                   CCOM-VARIANCE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOT-STRAP-AMT
           END-IF
           ADD CCOT-STRAP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3120-DESCRIBE-CHANNEL.
           EVALUATE CCOM-CHANNEL-CD
               WHEN 'AW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YR'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHIPMENT AMT.
      *----------------------------------------------------------------*
       3130-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-SHIPMENT-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOT-SHIPMENT-AMT
           END-IF
           ADD CCOT-SHIPMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-NOTICE.
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
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3150-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOT-SHIPMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD CCOT-SHIPMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CCOT-SHIPMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOT-SHIPMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOT-SHIPMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-FEE.
           IF CCOM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-FEE-YTD * CCOM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CCOT-VARIANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3170-CHECK-OPEN.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST ACT DATE IS PAST
      * BAT -> MAL WHEN LAST ACT DATE IS PAST
      * MAL -> TEL WHEN LAST ACT DATE IS PAST
      * TEL -> ONL WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3180-SET-CHANNEL.
           EVALUATE CCOT-CHANNEL-CD
               WHEN 'BRN'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO CCOT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CURRENCY TIERX TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3190-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CURRENCY-TIERX-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-CURRENCY-TIERX-LOW(WS-TX)
                   MOVE WS-CURRENCY-TIERX-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT STRAP YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3200-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = CCOM-STRAP-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CCOM-STRAP-YTD - (WS-WORK-AMT * 3)
           COMPUTE CCOM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3210-COUNT-ITEM.
           IF CCOM-ITEM-CNT < 9999999
               ADD 1 TO CCOM-ITEM-CNT
           END-IF
           IF CCOM-ITEM-CNT NOT < 3
               MOVE 'Y' TO CCOM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E532 ADDR REQUIRED
      * E650 FEE YTD NEGATIVE
      * E735 FEE YTD OVER 250000
      * E359 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3220-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-ADDR = SPACES
                   MOVE 'E532' TO WS-REJECT-CODE
               WHEN CCOM-FEE-YTD < 0
                   MOVE 'E650' TO WS-REJECT-CODE
               WHEN CCOM-FEE-YTD > 250000.00
                   MOVE 'E735' TO WS-REJECT-CODE
               WHEN CCOM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E359' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CURRENCY YTD FEE ON THE STRAP AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3230-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-CURRENCY-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-HOLD-FLAG = 'Y'
                   COMPUTE CCOM-CURRENCY-YTD ROUNDED = CCOM-STRAP-AMT *
                       CCOM-RATE / 200
               WHEN CCOM-DENOM-CD = 'B'
                   MOVE 10.00 TO CCOM-CURRENCY-YTD
               WHEN CCOM-DENOM-CD = 'D'
                   MOVE 7.00 TO CCOM-CURRENCY-YTD
               WHEN OTHER
                   COMPUTE CCOM-CURRENCY-YTD ROUNDED = CCOM-STRAP-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-CURRENCY-YTD > 0 AND CCOM-CURRENCY-YTD < 1.00
               MOVE 1.00 TO CCOM-CURRENCY-YTD
           END-IF
           IF CCOM-CURRENCY-YTD > 250.00
               MOVE 250.00 TO CCOM-CURRENCY-YTD
           END-IF
           ADD CCOM-CURRENCY-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3240-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-STATUS-CD
                   ADD CCOM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VARIANCE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3250-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOT-TRAN-CD
                   ADD CCOM-VARIANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-VARIANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * JL -> LW WHEN EFF DATE IS PAST
      * LW -> PS WHEN EFF DATE IS PAST
      * PS -> JL WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3260-SET-REASON.
           EVALUATE CCOM-REASON-CD
               WHEN 'JL'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LW' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LW'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PS' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PS'
                   IF CCOT-EFF-DATE > 0 AND CCOT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JL' TO CCOM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE SHIPMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3270-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-SHIPMENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-SEQ.
           IF CCOT-SEQ < 9999999
               ADD 1 TO CCOT-SEQ
           END-IF
           IF CCOT-SEQ NOT < 6
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3290-COUNT-ITEM.
           IF CCOM-ITEM-CNT < 9999999
               ADD 1 TO CCOM-ITEM-CNT
           END-IF
           IF CCOM-ITEM-CNT NOT < 10
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3310-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-LAST-PAY-DATE > 0 AND CCOM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-LAST-PAY-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 90
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE DENOM CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3330-DESCRIBE-DENOM.
           EVALUATE CCOM-DENOM-CD
               WHEN 'B'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'D'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'P'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CURRENCY TIERX TIER FOR THE CURRENCY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3340-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CURRENCY-TIERX-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CCOM-CURRENCY-AMT NOT < WS-CURRENCY-TIERX-LOW(WS-TX)
                   MOVE WS-CURRENCY-TIERX-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3350-LIMIT-FEE.
           IF CCOM-FEE-YTD < 25.00
               MOVE 25.00 TO CCOM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-FEE-YTD > 1000.00
               MOVE 1000.00 TO CCOM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3360-SPLIT-CURRENCY.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 3)
           COMPUTE CCOM-CURRENCY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3370-DESCRIBE-CHANNEL.
           EVALUATE CCOM-CHANNEL-CD
               WHEN 'AW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'DW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RS'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3380-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3390-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-FEE-AMT ROUNDED =
                   CCOM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-FEE-AMT
           END-IF
           ADD CCOM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3400-LIMIT-COIN.
           IF CCOT-COIN-AMT < 25.00
               MOVE 25.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-COIN-AMT > 2500.00
               MOVE 2500.00 TO CCOT-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3410-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO CCOM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STRAP FEE ON THE CURRENCY AMT.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3420-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO CCOM-STRAP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-HOLD-FLAG = 'Y'
                   COMPUTE CCOM-STRAP-AMT ROUNDED = CCOM-CURRENCY-AMT *
                       CCOM-RATE / 200
               WHEN CCOM-STATUS-CD = 'ADH'
                   MOVE 20.00 TO CCOM-STRAP-AMT
               WHEN CCOM-STATUS-CD = 'FVV'
                   MOVE 20.00 TO CCOM-STRAP-AMT
               WHEN OTHER
                   COMPUTE CCOM-STRAP-AMT ROUNDED = CCOM-CURRENCY-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-STRAP-AMT > 0 AND CCOM-STRAP-AMT < 5.00
               MOVE 5.00 TO CCOM-STRAP-AMT
           END-IF
           IF CCOM-STRAP-AMT > 75.00
               MOVE 75.00 TO CCOM-STRAP-AMT
           END-IF
           ADD CCOM-STRAP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SHIPMENT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3430-FLAG-SHIPMENT.
           IF CCOM-SHIPMENT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-SHIPMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * OR -> SH WHEN EFFECTIVE DATE IS PAST
      * SH -> RC WHEN EFFECTIVE DATE IS PAST
      * RC -> VR WHEN EFFECTIVE DATE IS PAST
      * VR -> AD WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3440-SET-TRAN.
           EVALUATE CCOT-TRAN-CD
               WHEN 'OR'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * OR -> SH WHEN NEXT DUE DATE IS PAST
      * SH -> RC WHEN NEXT DUE DATE IS PAST
      * RC -> VR WHEN NEXT DUE DATE IS PAST
      * VR -> AD WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3450-SET-TRAN.
           EVALUATE CCOT-TRAN-CD
               WHEN 'OR'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF CCOM-NEXT-DUE-DATE > 0 AND CCOM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3460-DESCRIBE-SOURCE.
           EVALUATE CCOM-SOURCE-CD
               WHEN 'CFR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'DTX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'EEA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EUA'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TPB'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'WVN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YTT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE COIN YTD FEE ON THE STRAP AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3470-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CCOM-COIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   COMPUTE CCOM-COIN-YTD ROUNDED = CCOT-STRAP-AMT *
                       CCOM-RATE / 200
               WHEN CCOM-CARRIER-CD = 'J'
                   MOVE 10.00 TO CCOM-COIN-YTD
               WHEN CCOM-CARRIER-CD = 'L'
                   MOVE 7.00 TO CCOM-COIN-YTD
               WHEN OTHER
                   COMPUTE CCOM-COIN-YTD ROUNDED = CCOT-STRAP-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-COIN-YTD > 0 AND CCOM-COIN-YTD < 2.00
               MOVE 2.00 TO CCOM-COIN-YTD
           END-IF
           IF CCOM-COIN-YTD > 25.00
               MOVE 25.00 TO CCOM-COIN-YTD
           END-IF
           ADD CCOM-COIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE COIN BAND TIER FOR THE VARIANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3480-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-BAND-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CCOM-VARIANCE-YTD NOT < WS-COIN-BAND-LOW(WS-TX)
                   MOVE WS-COIN-BAND-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3490-ACCRUE-SHIPMENT.
           IF CCOM-SHIPMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-SHIPMENT-YTD * CCOM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOT-VARIANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E450 OPERATOR REQUIRED
      * E892 VARIANCE AMT NEGATIVE
      * E619 VARIANCE AMT OVER 1000000
      * E266 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3500-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOT-OPERATOR = SPACES
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN CCOM-VARIANCE-AMT < 0
                   MOVE 'E892' TO WS-REJECT-CODE
               WHEN CCOM-VARIANCE-AMT > 1000000.00
                   MOVE 'E619' TO WS-REJECT-CODE
               WHEN CCOM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E266' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COIN AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3510-ACCRUE-COIN.
           IF CCOT-COIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-COIN-AMT * CCOM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3520-COUNT-SEQ.
           IF CCOT-SEQ < 9999999
               ADD 1 TO CCOT-SEQ
           END-IF
           IF CCOT-SEQ NOT < 3
               MOVE 'Y' TO CCOM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3530-DESCRIBE-CHANNEL.
           EVALUATE CCOM-CHANNEL-CD
               WHEN 'AW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RS'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UZ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'YR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPMENT YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3540-FLAG-SHIPMENT.
           IF CCOM-SHIPMENT-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-SHIPMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3550-FLAG-COIN.
           IF CCOT-COIN-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOT-COIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STRAP AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-STRAP.
           IF CCOM-STRAP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-STRAP-AMT * CCOM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-COIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHIPMENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3570-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = CCOT-SHIPMENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CCOT-SHIPMENT-AMT - (WS-WORK-AMT * 3)
           COMPUTE CCOM-CURRENCY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3580-DESCRIBE-STATUS.
           EVALUATE CCOM-STATUS-CD
               WHEN 'ADH'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FVV'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'GCB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'UZP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'VXB'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WTJ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'XNG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE STRAP YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3590-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOT-CHANNEL-CD
                   ADD CCOM-STRAP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-STRAP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E203 ADDR REQUIRED
      * E393 VARIANCE AMT NEGATIVE
      * E504 VARIANCE AMT OVER 50000
      * E910 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3600-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-ADDR = SPACES
                   MOVE 'E203' TO WS-REJECT-CODE
               WHEN CCOM-VARIANCE-AMT < 0
                   MOVE 'E393' TO WS-REJECT-CODE
               WHEN CCOM-VARIANCE-AMT > 50000.00
                   MOVE 'E504' TO WS-REJECT-CODE
               WHEN CCOM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E910' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3610-LIMIT-SHIPMENT.
           IF CCOM-SHIPMENT-YTD < 0.00
               MOVE 0.00 TO CCOM-SHIPMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-SHIPMENT-YTD > 50000.00
               MOVE 50000.00 TO CCOM-SHIPMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3620-PRORATE-COIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   CCOM-COIN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE CURRENCY TIERX TIER FOR THE STRAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3630-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CURRENCY-TIERX-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CCOM-STRAP-AMT NOT < WS-CURRENCY-TIERX-LOW(WS-TX)
                   MOVE WS-CURRENCY-TIERX-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3640-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF CCOT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CCOM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3650-ACCRUE-VARIANCE.
           IF CCOM-VARIANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-VARIANCE-AMT * CCOM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-COIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CURRENCY TIER TIER FOR THE STRAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3660-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CURRENCY-TIER-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CCOM-STRAP-AMT NOT < WS-CURRENCY-TIER-LOW(WS-TX)
                   MOVE WS-CURRENCY-TIER-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3670-FLAG-COIN.
           IF CCOM-COIN-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-COIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * VARIANCE YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3680-LIMIT-VARIANCE.
           IF CCOM-VARIANCE-YTD < 10.00
               MOVE 10.00 TO CCOM-VARIANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-VARIANCE-YTD > 5000.00
               MOVE 5000.00 TO CCOM-VARIANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3690-DESCRIBE-SOURCE.
           EVALUATE CCOM-SOURCE-CD
               WHEN 'CFR'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DTX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'EEA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EUA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TPB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'WVN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'YTT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CURRENCY FEE ON THE VARIANCE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3700-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CCOM-STOP-FLAG = 'Y'
                   MOVE ZERO TO CCOM-CURRENCY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   COMPUTE CCOM-CURRENCY-AMT ROUNDED = CCOT-VARIANCE-AMT
                       * CCOM-FEE-PCT / 200
               WHEN CCOM-CHANNEL-CD = 'AW'
                   MOVE 20.00 TO CCOM-CURRENCY-AMT
               WHEN CCOM-CHANNEL-CD = 'DW'
                   MOVE 12.00 TO CCOM-CURRENCY-AMT
               WHEN OTHER
                   COMPUTE CCOM-CURRENCY-AMT ROUNDED = CCOT-VARIANCE-AMT
                       * CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-CURRENCY-AMT > 0 AND CCOM-CURRENCY-AMT < 2.00
               MOVE 2.00 TO CCOM-CURRENCY-AMT
           END-IF
           IF CCOM-CURRENCY-AMT > 25.00
               MOVE 25.00 TO CCOM-CURRENCY-AMT
           END-IF
           ADD CCOM-CURRENCY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE CURRENCY TIERX TIER FOR THE STRAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3710-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CURRENCY-TIERX-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CCOT-STRAP-AMT NOT < WS-CURRENCY-TIERX-LOW(WS-TX)
                   MOVE WS-CURRENCY-TIERX-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AW -> DW WHEN LAST ACT DATE IS PAST
      * DW -> GN WHEN LAST ACT DATE IS PAST
      * GN -> RS WHEN LAST ACT DATE IS PAST
      * RS -> UZ WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-CHANNEL.
           EVALUATE CCOM-CHANNEL-CD
               WHEN 'AW'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DW' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DW'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GN' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GN'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RS' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RS'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UZ' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-SOURCE.
           EVALUATE CCOM-SOURCE-CD
               WHEN 'CFR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'DTX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'EEA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'EUA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TPB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WVN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YTT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 15 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3740-CHECK-OPEN.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3750-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CURRENCY FEE ON THE COIN AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3760-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CCOM-CURRENCY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE CCOM-CURRENCY-AMT ROUNDED = CCOM-COIN-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO CCOM-CURRENCY-AMT
               WHEN CCOT-CHANNEL-CD = 'BAT'
                   MOVE 5.00 TO CCOM-CURRENCY-AMT
               WHEN OTHER
                   COMPUTE CCOM-CURRENCY-AMT ROUNDED = CCOM-COIN-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-CURRENCY-AMT > 0 AND CCOM-CURRENCY-AMT < 5.00
               MOVE 5.00 TO CCOM-CURRENCY-AMT
           END-IF
           IF CCOM-CURRENCY-AMT > 35.00
               MOVE 35.00 TO CCOM-CURRENCY-AMT
           END-IF
           ADD CCOM-CURRENCY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE CURRENCY FEE ON THE SHIPMENT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3770-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN CCOM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO CCOM-CURRENCY-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CCOM-CURRENCY-AMT ROUNDED = CCOT-SHIPMENT-AMT
                       * CCOM-RATE / 200
               WHEN CCOM-CHANNEL-CD = 'AW'
                   MOVE 3.00 TO CCOM-CURRENCY-AMT
               WHEN CCOM-CHANNEL-CD = 'DW'
                   MOVE 3.00 TO CCOM-CURRENCY-AMT
               WHEN OTHER
                   COMPUTE CCOM-CURRENCY-AMT ROUNDED = CCOT-SHIPMENT-AMT
                       * CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-CURRENCY-AMT > 0 AND CCOM-CURRENCY-AMT < 2.00
               MOVE 2.00 TO CCOM-CURRENCY-AMT
           END-IF
           IF CCOM-CURRENCY-AMT > 25.00
               MOVE 25.00 TO CCOM-CURRENCY-AMT
           END-IF
           ADD CCOM-CURRENCY-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3780-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF CCOT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO CCOM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3790-DESCRIBE-STATUS.
           EVALUATE CCOM-STATUS-CD
               WHEN 'ADH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FVV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'GCB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'UZP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'VXB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WTJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XNG'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE FEE ON THE FEE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3800-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CCOM-VARIANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CCOM-VARIANCE-AMT ROUNDED = CCOM-FEE-YTD *
                       CCOM-RATE / 200
               WHEN CCOM-CARRIER-CD = 'J'
                   MOVE 7.00 TO CCOM-VARIANCE-AMT
               WHEN CCOM-CARRIER-CD = 'L'
                   MOVE 20.00 TO CCOM-VARIANCE-AMT
               WHEN OTHER
                   COMPUTE CCOM-VARIANCE-AMT ROUNDED = CCOM-FEE-YTD *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-VARIANCE-AMT > 0 AND CCOM-VARIANCE-AMT < 2.00
               MOVE 2.00 TO CCOM-VARIANCE-AMT
           END-IF
           IF CCOM-VARIANCE-AMT > 75.00
               MOVE 75.00 TO CCOM-VARIANCE-AMT
           END-IF
           ADD CCOM-VARIANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * CCOM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CCOM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VARIANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPMENT AMT.
      *----------------------------------------------------------------*
       3820-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-SHIPMENT-AMT ROUNDED =
                   CCOT-VARIANCE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOT-SHIPMENT-AMT
           END-IF
           ADD CCOT-SHIPMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE DENOM CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3830-DESCRIBE-DENOM.
           EVALUATE CCOM-DENOM-CD
               WHEN 'B'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE DENOM CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3840-DESCRIBE-DENOM.
           EVALUATE CCOM-DENOM-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'D'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'P'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'S'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT STRAP AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3850-SPLIT-STRAP.
           COMPUTE WS-WORK-AMT = CCOT-STRAP-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CCOT-STRAP-AMT - (WS-WORK-AMT * 4)
           COMPUTE CCOM-SHIPMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3860-ACCRUE-VARIANCE.
           IF CCOM-VARIANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-VARIANCE-YTD * CCOM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3870-ACCRUE-FEE.
           IF CCOM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-FEE-AMT * CCOM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3880-COUNT-RETRY.
           IF CCOM-RETRY-CNT < 9999999
               ADD 1 TO CCOM-RETRY-CNT
           END-IF
           IF CCOM-RETRY-CNT NOT < 3
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CARRIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-CARRIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CARRIER-CD
                   ADD CCOM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CARRIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STRAP YTD OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3900-FLAG-STRAP.
           IF CCOM-STRAP-YTD > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-STRAP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3910-COUNT-ITEM.
           IF CCOM-ITEM-CNT < 9999999
               ADD 1 TO CCOM-ITEM-CNT
           END-IF
           IF CCOM-ITEM-CNT NOT < 3
               MOVE 'Y' TO CCOM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3920-AGE-OPEN.
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
                   ADD CCOT-STRAP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CCOT-STRAP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CCOT-STRAP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOT-STRAP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOT-STRAP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CURRENCY TIERX TIER FOR THE SHIPMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3930-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-CURRENCY-TIERX-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CCOT-SHIPMENT-AMT NOT < WS-CURRENCY-TIERX-LOW(WS-TX)
                   MOVE WS-CURRENCY-TIERX-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CURRENCY YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHIPMENT AMT.
      *----------------------------------------------------------------*
       3940-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-SHIPMENT-AMT ROUNDED =
                   CCOM-CURRENCY-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOT-SHIPMENT-AMT
           END-IF
           ADD CCOT-SHIPMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CURRENCY YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3950-ACCRUE-CURRENCY.
           IF CCOM-CURRENCY-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-CURRENCY-YTD * CCOM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOT-SHIPMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT AMT IS FLOORED AT $100.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3960-LIMIT-SHIPMENT.
           IF CCOT-SHIPMENT-AMT < 100.00
               MOVE 100.00 TO CCOT-SHIPMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-SHIPMENT-AMT > 5000.00
               MOVE 5000.00 TO CCOT-SHIPMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STRAP AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3970-LIMIT-STRAP.
           IF CCOM-STRAP-AMT < 100.00
               MOVE 100.00 TO CCOM-STRAP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-STRAP-AMT > 1000.00
               MOVE 1000.00 TO CCOM-STRAP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3980-ACCRUE-SHIPMENT.
           IF CCOT-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-SHIPMENT-AMT * CCOM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-VARIANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E542 DESC REQUIRED
      * E372 SHIPMENT AMT NEGATIVE
      * E934 SHIPMENT AMT OVER 50000
      * E884 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3990-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-DESC = SPACES
                   MOVE 'E542' TO WS-REJECT-CODE
               WHEN CCOT-SHIPMENT-AMT < 0
                   MOVE 'E372' TO WS-REJECT-CODE
               WHEN CCOT-SHIPMENT-AMT > 50000.00
                   MOVE 'E934' TO WS-REJECT-CODE
               WHEN CCOT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E884' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4000-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E264 OPERATOR REQUIRED
      * E904 FEE YTD NEGATIVE
      * E576 FEE YTD OVER 1000000
      * E464 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOT-OPERATOR = SPACES
                   MOVE 'E264' TO WS-REJECT-CODE
               WHEN CCOM-FEE-YTD < 0
                   MOVE 'E904' TO WS-REJECT-CODE
               WHEN CCOM-FEE-YTD > 1000000.00
                   MOVE 'E576' TO WS-REJECT-CODE
               WHEN CCOT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E464' TO WS-REJECT-CODE
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
       4020-DESCRIBE-CHANNEL.
           EVALUATE CCOT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4030-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-COIN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-COIN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-COIN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-COIN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-COIN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4040-AGE-OPEN.
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
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-STRAP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E285 REF NO REQUIRED
      * E902 SHIPMENT AMT NEGATIVE
      * E268 SHIPMENT AMT OVER 10000
      * E060 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4050-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-REF-NO = SPACES
                   MOVE 'E285' TO WS-REJECT-CODE
               WHEN CCOT-SHIPMENT-AMT < 0
                   MOVE 'E902' TO WS-REJECT-CODE
               WHEN CCOT-SHIPMENT-AMT > 10000.00
                   MOVE 'E268' TO WS-REJECT-CODE
               WHEN CCOM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E060' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COIN AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4060-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-REASON-CD
                   ADD CCOM-COIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-COIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4070-AGE-EFF.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE CURRENCY TIERX TIER FOR THE VARIANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4080-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CURRENCY-TIERX-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CCOM-VARIANCE-AMT NOT < WS-CURRENCY-TIERX-LOW(WS-TX)
                   MOVE WS-CURRENCY-TIERX-RATE(WS-TX) TO
                       CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4090-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF CCOT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-EFF-DATE TO DTU-DATE-1
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
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AW -> DW WHEN EFFECTIVE DATE IS PAST
      * DW -> GN WHEN EFFECTIVE DATE IS PAST
      * GN -> RS WHEN EFFECTIVE DATE IS PAST
      * RS -> UZ WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4100-SET-CHANNEL.
           EVALUATE CCOM-CHANNEL-CD
               WHEN 'AW'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DW' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DW'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GN' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GN'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RS' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RS'
                   IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UZ' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4110-AGE-NEXT-REVIEW.
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
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-COIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE VARIANCE SCHED TIER FOR THE COIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4120-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-VARIANCE-SCHED-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 5
               IF CCOM-COIN-YTD NOT < WS-VARIANCE-SCHED-LOW(WS-TX)
                   MOVE WS-VARIANCE-SCHED-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE VARIANCE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4130-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-HOLD-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CCOT-VARIANCE-AMT *
                       CCOM-FEE-PCT / 200
               WHEN CCOT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN CCOT-CHANNEL-CD = 'BAT'
                   MOVE 7.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = CCOT-VARIANCE-AMT *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 2.00
               MOVE 2.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 75.00
               MOVE 75.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4140-COUNT-ITEM.
           IF CCOM-ITEM-CNT < 9999999
               ADD 1 TO CCOM-ITEM-CNT
           END-IF
           IF CCOM-ITEM-CNT NOT < 10
               MOVE 'Y' TO CCOM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4150-AGE-EFF.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4160-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO CCOM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-SEQ.
           IF CCOT-SEQ < 9999999
               ADD 1 TO CCOT-SEQ
           END-IF
           IF CCOT-SEQ NOT < 6
               MOVE 'Y' TO CCOM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4180-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO CCOM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E399 REF NO REQUIRED
      * E173 FEE AMT NEGATIVE
      * E739 FEE AMT OVER 99999
      * E830 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4190-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-REF-NO = SPACES
                   MOVE 'E399' TO WS-REJECT-CODE
               WHEN CCOM-FEE-AMT < 0
                   MOVE 'E173' TO WS-REJECT-CODE
               WHEN CCOM-FEE-AMT > 99999.00
                   MOVE 'E739' TO WS-REJECT-CODE
               WHEN CCOM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E830' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $100.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4200-LIMIT-ITEM.
           IF WS-WORK-AMT < 100.00
               MOVE 100.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4210-DESCRIBE-CHANNEL.
           EVALUATE CCOT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4220-ACCRUE-SHIPMENT.
           IF CCOT-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-SHIPMENT-AMT * CCOM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-STRAP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STRAP AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4230-LIMIT-STRAP.
           IF CCOM-STRAP-AMT < 50.00
               MOVE 50.00 TO CCOM-STRAP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-STRAP-AMT > 1000.00
               MOVE 1000.00 TO CCOM-STRAP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * JL -> LW WHEN LAST ACT DATE IS PAST
      * LW -> PS WHEN LAST ACT DATE IS PAST
      * PS -> JL WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4240-SET-REASON.
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
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4250-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF CCOM-EFFECTIVE-DATE > 0 AND CCOM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOT-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * VARIANCE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4260-FLAG-VARIANCE.
           IF CCOT-VARIANCE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOT-VARIANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4270-ACCRUE-SHIPMENT.
           IF CCOT-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-SHIPMENT-AMT * CCOM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-VARIANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4280-ACCRUE-SHIPMENT.
           IF CCOM-SHIPMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-SHIPMENT-YTD * CCOM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-COIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4290-CHECK-NEXT-REVIEW.
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
      * PRORATE COIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CURRENCY AMT.
      *----------------------------------------------------------------*
       4300-PRORATE-CURRENCY.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-CURRENCY-AMT ROUNDED =
                   CCOM-COIN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOM-CURRENCY-AMT
           END-IF
           ADD CCOM-CURRENCY-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STRAP AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4310-LIMIT-STRAP.
           IF CCOT-STRAP-AMT < 50.00
               MOVE 50.00 TO CCOT-STRAP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOT-STRAP-AMT > 500.00
               MOVE 500.00 TO CCOT-STRAP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COIN AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4320-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = CCOT-COIN-AMT / 4
           COMPUTE WS-WORK-AMT-2 = CCOT-COIN-AMT - (WS-WORK-AMT * 4)
           COMPUTE CCOM-CURRENCY-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COIN YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4330-ACCRUE-COIN.
           IF CCOM-COIN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-COIN-YTD * CCOM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4340-AGE-EFF.
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
                   ADD CCOM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STRAP FEE ON THE SHIPMENT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4350-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO CCOT-STRAP-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CCOT-STRAP-AMT ROUNDED = CCOM-SHIPMENT-AMT *
                       CCOM-RATE / 200
               WHEN CCOM-DENOM-CD = 'B'
                   MOVE 7.00 TO CCOT-STRAP-AMT
               WHEN CCOM-DENOM-CD = 'D'
                   MOVE 5.00 TO CCOT-STRAP-AMT
               WHEN OTHER
                   COMPUTE CCOT-STRAP-AMT ROUNDED = CCOM-SHIPMENT-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOT-STRAP-AMT > 0 AND CCOT-STRAP-AMT < 5.00
               MOVE 5.00 TO CCOT-STRAP-AMT
           END-IF
           IF CCOT-STRAP-AMT > 100.00
               MOVE 100.00 TO CCOT-STRAP-AMT
           END-IF
           ADD CCOT-STRAP-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO STRAP YTD.
      *----------------------------------------------------------------*
       4360-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-STRAP-YTD ROUNDED =
                   CCOM-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOM-STRAP-YTD
           END-IF
           ADD CCOM-STRAP-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       4370-COUNT-RETRY.
           IF CCOM-RETRY-CNT < 9999999
               ADD 1 TO CCOM-RETRY-CNT
           END-IF
           IF CCOM-RETRY-CNT NOT < 10
               MOVE 'Y' TO CCOM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4380-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = CCOM-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = CCOM-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE CCOM-SHIPMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT AMT IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4390-LIMIT-SHIPMENT.
           IF CCOM-SHIPMENT-AMT < 25.00
               MOVE 25.00 TO CCOM-SHIPMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-SHIPMENT-AMT > 500.00
               MOVE 500.00 TO CCOM-SHIPMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STRAP YTD BY CARRIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4400-ACCUM-CARRIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CARRIER-CD
                   ADD CCOM-STRAP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CARRIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-STRAP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4420-CHECK-NOTICE.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO CCOM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE CURRENCY TIERX TIER FOR THE STRAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4430-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-CURRENCY-TIERX-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF CCOM-STRAP-AMT NOT < WS-CURRENCY-TIERX-LOW(WS-TX)
                   MOVE WS-CURRENCY-TIERX-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E443 OPERATOR REQUIRED
      * E424 CURRENCY AMT NEGATIVE
      * E980 CURRENCY AMT OVER 5000
      * E467 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4440-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOT-OPERATOR = SPACES
                   MOVE 'E443' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT < 0
                   MOVE 'E424' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT > 5000.00
                   MOVE 'E980' TO WS-REJECT-CODE
               WHEN CCOT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E467' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4450-ACCRUE-SHIPMENT.
           IF CCOM-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-SHIPMENT-AMT * CCOM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CCOT-SHIPMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4460-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO CCOM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CURRENCY YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4470-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOT-CHANNEL-CD
                   ADD CCOM-CURRENCY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-CURRENCY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4480-COUNT-CYCLE.
           IF CCOM-CYCLE-CNT < 9999999
               ADD 1 TO CCOM-CYCLE-CNT
           END-IF
           IF CCOM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO CCOM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AW -> DW WHEN LAST ACT DATE IS PAST
      * DW -> GN WHEN LAST ACT DATE IS PAST
      * GN -> RS WHEN LAST ACT DATE IS PAST
      * RS -> UZ WHEN LAST ACT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4490-SET-CHANNEL.
           EVALUATE CCOM-CHANNEL-CD
               WHEN 'AW'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DW' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DW'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GN' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GN'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RS' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RS'
                   IF CCOM-LAST-ACT-DATE > 0 AND CCOM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UZ' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4500-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF CCOM-EXPIRY-DATE > 0 AND CCOM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD CCOM-SHIPMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CCOM-SHIPMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CCOM-SHIPMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-SHIPMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-SHIPMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4510-CHECK-LAST-ACT.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO CCOM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4520-FLAG-COIN.
           IF CCOM-COIN-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-COIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4530-CHECK-NOTICE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO CCOM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4540-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4550-ACCRUE-FEE.
           IF CCOM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-FEE-YTD * CCOM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-VARIANCE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4560-DESCRIBE-CHANNEL.
           EVALUATE CCOT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE CCO FIELDS TO CNCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-CNCAL.
           INITIALIZE CNCAL-PARMS
           MOVE CCOM-STRAP-YTD
             TO CNCAL-STRAP-YTD
           MOVE CCOM-VARIANCE-AMT
             TO CNCAL-VARIANCE-AMT
           MOVE CCOM-FEE-YTD
             TO CNCAL-FEE-YTD
           MOVE CCOM-SHIPMENT-AMT
             TO CNCAL-SHIPMENT-AMT
           MOVE CCOM-SHIPMENT-YTD
             TO CNCAL-SHIPMENT-YTD
           MOVE CCOM-COIN-AMT
             TO CNCAL-COIN-AMT
           MOVE CCOM-EXPIRY-DATE
             TO CNCAL-EXPIRY-DATE
           MOVE CCOM-MATURITY-DATE
             TO CNCAL-MATURITY-DATE
           MOVE CCOM-OPEN-DATE
             TO CNCAL-OPEN-DATE
           MOVE CCOM-NEXT-REVIEW-DATE
             TO CNCAL-NEXT-REVIEW-DATE
           MOVE CCOM-STATUS-CD
             TO CNCAL-STATUS-CD
           MOVE CCOM-CARRIER-CD
             TO CNCAL-CARRIER-CD
           MOVE CCOM-SOURCE-CD
             TO CNCAL-SOURCE-CD
           MOVE CCOM-CHANNEL-CD
             TO CNCAL-CHANNEL-CD
           MOVE CCOM-OVERRIDE-FLAG
             TO CNCAL-OVERRIDE-FLAG
           MOVE CCOM-EXEMPT-FLAG
             TO CNCAL-EXEMPT-FLAG
           MOVE CCOM-NOTICE-SENT-FLAG
             TO CNCAL-NOTICE-SENT-FLAG
           MOVE CCOM-NOTICE-CNT
             TO CNCAL-NOTICE-CNT
           MOVE CCOM-RETRY-CNT
             TO CNCAL-RETRY-CNT
           MOVE CCOM-FEE-PCT
             TO CNCAL-FEE-PCT
           MOVE CCOM-RATE
             TO CNCAL-RATE
           MOVE CCOM-NAME
             TO CNCAL-NAME
           MOVE CCOM-DESC
             TO CNCAL-DESC
           CALL 'CNCAL' USING CNCAL-PARMS
           EVALUATE CNCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE CNCAL-SHIPMENT-AMT
                     TO CCOM-SHIPMENT-AMT
                   MOVE CNCAL-SHIPMENT-YTD
                     TO CCOM-SHIPMENT-YTD
                   MOVE CNCAL-COIN-AMT
                     TO CCOM-COIN-AMT
                   MOVE CNCAL-NOTICE-SENT-FLAG
                     TO CCOM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-CNCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
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
                   MOVE CNPEN-COIN-AMT
                     TO CCOM-COIN-AMT
                   MOVE CNPEN-STRAP-YTD
                     TO CCOM-STRAP-YTD
                   MOVE CNPEN-SHIPMENT-YTD
                     TO CCOM-SHIPMENT-YTD
                   CONTINUE
               WHEN 08
                   MOVE 'RPEN' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-CNPEN' TO ABND-PARAGRAPH
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
           DISPLAY 'CNOAUDT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CNOAUDT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
