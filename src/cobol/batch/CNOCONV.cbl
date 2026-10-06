      *================================================================*
      * PROGRAM  : CNOCONV                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : COIN AND CURRENCY - FILE CONVERSION. READS THE CCO  *
      *            TRANSACTIONS, UPDATES THE CCO MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. BRANCH COIN AND CURRENCY ORDERS *
      *            AND ARMORED CARRIER SHIPMENT                        *
      * INPUT    : CCOMST (KSDS), CCOTRN, BANKCTL                      *
      * OUTPUT   : CCOOUT, CNOCORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, CNCAL, CNPEN, CNDTE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-09-09 PQR  ORIGINAL                                       *
      * 1989-07-13 MKL  SHIPMENT EXCEPTIONS TO OPS QUEUE REQ6063       *
      * 1992-03-02 MKL  STRAP LIMIT REVISED PRB3105                    *
      * 1997-11-16 DPS  STRAP EXCEPTIONS TO OPS QUEUE PRB8337          *
      * 2002-04-15 PQR  SHIPMENT CALCULATION CORRECTED CHG9135         *
      * 2004-08-01 MKL  VARIANCE TABLE EXPANDED CHG8898                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CNOCONV.
       AUTHOR. D P SHAW.

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
           SELECT REPORT-FILE ASSIGN TO "CNOCORPT"
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
      * WS-COIN-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COIN-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000068000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000606000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000815500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000850000000900000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000950000000653500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001050000000337500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000209000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004550000000801000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00004650000000364000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007150000000167000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017150000000209000'.
       01  WS-COIN-SCHED-TBL REDEFINES WS-COIN-SCHED-VALUES.
           05  WS-COIN-SCHED-ROW OCCURS 11 TIMES.
               10  WS-COIN-SCHED-LOW       PIC 9(09)V99.
               10  WS-COIN-SCHED-RATE      PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-COIN-SCHEDX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-COIN-SCHEDX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000363000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000319000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000750000000128000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001750000000664000'.
       01  WS-COIN-SCHEDX-TBL REDEFINES WS-COIN-SCHEDX-VALUES.
           05  WS-COIN-SCHEDX-ROW OCCURS 4 TIMES.
               10  WS-COIN-SCHEDX-LOW      PIC 9(09)V99.
               10  WS-COIN-SCHEDX-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  CNOCONV  COIN AND CURRENCY'.
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
               PERFORM 3000-AGE-NEXT-REVIEW
               PERFORM 3010-LIMIT-SHIPMENT
               PERFORM 3020-ASSESS-STOP
               PERFORM 3030-SPLIT-COIN
               PERFORM 3040-PRORATE-SHIPMENT
               PERFORM 3050-FLAG-CURRENCY
               PERFORM 3060-EDIT-NAME
               PERFORM 3070-CHECK-NEXT-DUE
               PERFORM 3080-ACCRUE-CURRENCY
               PERFORM 3090-FLAG-COIN
               PERFORM 3100-EDIT-MEMO
               PERFORM 3110-COUNT-NOTICE
               PERFORM 3120-SELECT-PENALTY-PCT
               PERFORM 3130-SET-STATUS
               PERFORM 3140-ACCUM-STATUS
               PERFORM 3150-SELECT-PENALTY-PCT
               PERFORM 3160-ACCRUE-VARIANCE
               PERFORM 3170-SPLIT-SHIPMENT
               PERFORM 3180-COUNT-NOTICE
               PERFORM 3190-LIMIT-VARIANCE
               PERFORM 3200-AGE-EFFECTIVE
               PERFORM 3210-PRORATE-FEE
               PERFORM 3220-ASSESS-REVERSAL
               PERFORM 3230-ACCRUE-STRAP
               PERFORM 3240-DESCRIBE-CHANNEL
               PERFORM 3250-ACCRUE-SHIPMENT
               PERFORM 3260-FLAG-FEE
               PERFORM 3270-FLAG-STRAP
               PERFORM 3280-ACCRUE-VARIANCE
               PERFORM 3290-FLAG-FEE
               PERFORM 3300-PRORATE-STRAP
               PERFORM 3310-ACCRUE-VARIANCE
               PERFORM 3320-SELECT-RATE
               PERFORM 3330-SELECT-PENALTY-PCT
               PERFORM 3340-CHECK-OPEN
               PERFORM 3350-SELECT-RATE
               PERFORM 3360-EDIT-DESC
               PERFORM 3370-ACCUM-DENOM
               PERFORM 3380-SPLIT-VARIANCE
               PERFORM 3390-ASSESS-REVERSAL
               PERFORM 3400-SPLIT-COIN
               PERFORM 3410-ACCUM-CHANNEL
               PERFORM 3420-LIMIT-SHIPMENT
               PERFORM 3430-DESCRIBE-REASON
               PERFORM 3440-AGE-EXPIRY
               PERFORM 3450-LIMIT-FEE
               PERFORM 3460-SELECT-FEE-PCT
               PERFORM 3470-SET-REASON
               PERFORM 3480-SELECT-PENALTY-PCT
               PERFORM 3490-DESCRIBE-SOURCE
               PERFORM 3500-SELECT-FEE-PCT
               PERFORM 3510-EDIT-MEMO
               PERFORM 3520-FLAG-STRAP
               PERFORM 3530-ACCUM-CHANNEL
               PERFORM 3540-ASSESS-HOLD
               PERFORM 3550-CHECK-NEXT-REVIEW
               PERFORM 3560-PRORATE-COIN
               PERFORM 3570-FLAG-STRAP
               PERFORM 3580-SPLIT-CURRENCY
               PERFORM 3590-CHECK-NOTICE
               PERFORM 3600-COUNT-RETRY
               PERFORM 3610-LIMIT-COIN
               PERFORM 3620-PRORATE-SHIPMENT
               PERFORM 3630-DESCRIBE-SOURCE
               PERFORM 3640-PRORATE-SHIPMENT
               PERFORM 3650-SPLIT-COIN
               PERFORM 3660-EDIT-NAME
               PERFORM 3670-CHECK-EFF
               PERFORM 3680-EDIT-REF-NO
               PERFORM 3690-AGE-OPEN
               PERFORM 3700-EDIT-DESC
               PERFORM 3710-ACCUM-CHANNEL
               PERFORM 3720-PRORATE-FEE
               PERFORM 3730-ACCRUE-COIN
               PERFORM 3740-LIMIT-STRAP
               PERFORM 3750-SPLIT-COIN
               PERFORM 3760-ACCUM-CHANNEL
               PERFORM 3770-DESCRIBE-DENOM
               PERFORM 3780-FLAG-COIN
               PERFORM 3790-ASSESS-HOLD
               PERFORM 3800-CHECK-NEXT-DUE
               PERFORM 3810-ASSESS-HOLD
               PERFORM 3820-SELECT-PENALTY-PCT
               PERFORM 3830-SPLIT-VARIANCE
               PERFORM 3840-AGE-NOTICE
               PERFORM 3850-ACCUM-STATUS
               PERFORM 3860-COUNT-NOTICE
               PERFORM 3870-SET-TRAN
               PERFORM 3880-PRORATE-FEE
               PERFORM 3890-CHECK-MATURITY
               PERFORM 3900-LIMIT-FEE
               PERFORM 3910-CHECK-POST
               PERFORM 3920-AGE-OPEN
               PERFORM 3930-ASSESS-STOP
               PERFORM 3940-SELECT-RATE
               PERFORM 3950-ASSESS-REVERSAL
               PERFORM 3960-EDIT-NAME
               PERFORM 3970-LIMIT-ITEM
               PERFORM 3980-ASSESS-HOLD
               PERFORM 3990-CHECK-NEXT-REVIEW
               PERFORM 4000-ACCRUE-FEE
               PERFORM 4010-LIMIT-STRAP
               PERFORM 4020-SPLIT-VARIANCE
               PERFORM 4030-AGE-EFF
               PERFORM 4040-SPLIT-CURRENCY
               PERFORM 4050-ACCRUE-FEE-PCT
               PERFORM 4060-EDIT-DESC
               PERFORM 4070-SET-CARRIER
               PERFORM 4080-CHECK-EFFECTIVE
               PERFORM 4090-SELECT-PENALTY-PCT
               PERFORM 4100-CHECK-NEXT-REVIEW
               PERFORM 4110-SET-CARRIER
               PERFORM 4120-ASSESS-HOLD
               PERFORM 4130-SET-CHANNEL
               PERFORM 4140-ASSESS-STOP
               PERFORM 7020-CALL-CNCAL
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
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3000-AGE-NEXT-REVIEW.
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
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHIPMENT AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-SHIPMENT.
           IF CCOM-SHIPMENT-AMT < 10.00
               MOVE 10.00 TO CCOM-SHIPMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-SHIPMENT-AMT > 1000.00
               MOVE 1000.00 TO CCOM-SHIPMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT FEE ON THE COIN AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3020-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CCOM-STOP-FLAG = 'Y'
                   MOVE ZERO TO CCOM-SHIPMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOT-COIN-AMT *
                       CCOM-FEE-PCT / 200
               WHEN CCOM-STATUS-CD = 'ADH'
                   MOVE 5.00 TO CCOM-SHIPMENT-AMT
               WHEN CCOM-STATUS-CD = 'FVV'
                   MOVE 7.00 TO CCOM-SHIPMENT-AMT
               WHEN OTHER
                   COMPUTE CCOM-SHIPMENT-AMT ROUNDED = CCOT-COIN-AMT *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-SHIPMENT-AMT > 0 AND CCOM-SHIPMENT-AMT < 1.00
               MOVE 1.00 TO CCOM-SHIPMENT-AMT
           END-IF
           IF CCOM-SHIPMENT-AMT > 50.00
               MOVE 50.00 TO CCOM-SHIPMENT-AMT
           END-IF
           ADD CCOM-SHIPMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT COIN AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3030-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = CCOM-COIN-AMT / 3
           COMPUTE WS-WORK-AMT-2 = CCOM-COIN-AMT - (WS-WORK-AMT * 3)
           COMPUTE CCOM-SHIPMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHIPMENT AMT.
      *----------------------------------------------------------------*
       3040-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-SHIPMENT-AMT ROUNDED =
                   CCOM-COIN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-SHIPMENT-AMT
           END-IF
           ADD CCOM-SHIPMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CURRENCY AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3050-FLAG-CURRENCY.
           IF CCOM-CURRENCY-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE CCOM-CURRENCY-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E164 NAME REQUIRED
      * E779 COIN AMT NEGATIVE
      * E217 COIN AMT OVER 10000
      * E243 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3060-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-NAME = SPACES
                   MOVE 'E164' TO WS-REJECT-CODE
               WHEN CCOT-COIN-AMT < 0
                   MOVE 'E779' TO WS-REJECT-CODE
               WHEN CCOT-COIN-AMT > 10000.00
                   MOVE 'E217' TO WS-REJECT-CODE
               WHEN CCOM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E243' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 120 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3070-CHECK-NEXT-DUE.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CURRENCY AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3080-ACCRUE-CURRENCY.
           IF CCOM-CURRENCY-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-CURRENCY-AMT * CCOM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CCOM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3090-FLAG-COIN.
           IF CCOM-COIN-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOM-COIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E346 MEMO REQUIRED
      * E459 STRAP YTD NEGATIVE
      * E878 STRAP YTD OVER 25000
      * E102 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3100-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOT-MEMO = SPACES
                   MOVE 'E346' TO WS-REJECT-CODE
               WHEN CCOM-STRAP-YTD < 0
                   MOVE 'E459' TO WS-REJECT-CODE
               WHEN CCOM-STRAP-YTD > 25000.00
                   MOVE 'E878' TO WS-REJECT-CODE
               WHEN CCOT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E102' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CCOM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COIN SCHEDX TIER FOR THE VARIANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3120-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHEDX-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CCOM-VARIANCE-YTD NOT < WS-COIN-SCHEDX-LOW(WS-TX)
                   MOVE WS-COIN-SCHEDX-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * ADH -> FVV WHEN EXPIRY DATE IS PAST
      * FVV -> GCB WHEN EXPIRY DATE IS PAST
      * GCB -> UZP WHEN EXPIRY DATE IS PAST
      * UZP -> VXB WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3130-SET-STATUS.
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
      * ACCUMULATE FEE YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3140-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-STATUS-CD
                   ADD CCOM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COIN SCHEDX TIER FOR THE VARIANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHEDX-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CCOM-VARIANCE-AMT NOT < WS-COIN-SCHEDX-LOW(WS-TX)
                   MOVE WS-COIN-SCHEDX-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-VARIANCE.
           IF CCOM-VARIANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-VARIANCE-YTD * CCOM-RATE / 100 / 360
               ADD WS-WORK-AMT TO CCOM-SHIPMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3170-SPLIT-SHIPMENT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 3
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 3)
           COMPUTE CCOM-SHIPMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3180-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CCOM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * VARIANCE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3190-LIMIT-VARIANCE.
           IF CCOM-VARIANCE-AMT < 10.00
               MOVE 10.00 TO CCOM-VARIANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-VARIANCE-AMT > 50000.00
               MOVE 50000.00 TO CCOM-VARIANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3200-AGE-EFFECTIVE.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3210-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-FEE-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-FEE-AMT
           END-IF
           ADD CCOM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE SHIPMENT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3220-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = CCOT-SHIPMENT-AMT *
                       CCOM-RATE / 200
               WHEN CCOM-DENOM-CD = 'B'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN CCOM-DENOM-CD = 'D'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = CCOT-SHIPMENT-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON STRAP AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-STRAP.
           IF CCOM-STRAP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-STRAP-AMT * CCOM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3240-DESCRIBE-CHANNEL.
           EVALUATE CCOT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHIPMENT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-SHIPMENT.
           IF CCOT-SHIPMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-SHIPMENT-AMT * CCOM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-SHIPMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3260-FLAG-FEE.
           IF CCOM-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STRAP AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3270-FLAG-STRAP.
           IF CCOT-STRAP-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOT-STRAP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3280-ACCRUE-VARIANCE.
           IF CCOT-VARIANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-VARIANCE-AMT * CCOM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-SHIPMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3290-FLAG-FEE.
           IF CCOM-FEE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO STRAP AMT.
      *----------------------------------------------------------------*
       3300-PRORATE-STRAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-STRAP-AMT ROUNDED =
                   CCOM-FEE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOT-STRAP-AMT
           END-IF
           ADD CCOT-STRAP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON VARIANCE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3310-ACCRUE-VARIANCE.
           IF CCOT-VARIANCE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOT-VARIANCE-AMT * CCOM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO CCOM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COIN SCHED TIER FOR THE STRAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHED-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CCOM-STRAP-YTD NOT < WS-COIN-SCHED-LOW(WS-TX)
                   MOVE WS-COIN-SCHED-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COIN SCHED TIER FOR THE CURRENCY AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHED-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CCOM-CURRENCY-AMT NOT < WS-COIN-SCHED-LOW(WS-TX)
                   MOVE WS-COIN-SCHED-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 90 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-OPEN.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COIN SCHED TIER FOR THE SHIPMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHED-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CCOT-SHIPMENT-AMT NOT < WS-COIN-SCHED-LOW(WS-TX)
                   MOVE WS-COIN-SCHED-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E175 DESC REQUIRED
      * E109 COIN AMT NEGATIVE
      * E304 COIN AMT OVER 10000
      * E245 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3360-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-DESC = SPACES
                   MOVE 'E175' TO WS-REJECT-CODE
               WHEN CCOT-COIN-AMT < 0
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN CCOT-COIN-AMT > 10000.00
                   MOVE 'E304' TO WS-REJECT-CODE
               WHEN CCOM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E245' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY DENOM CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-DENOM.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-DENOM-CD
                   ADD CCOM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-DENOM-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VARIANCE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3380-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = CCOM-VARIANCE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CCOM-VARIANCE-YTD - (WS-WORK-AMT * 3)
           COMPUTE CCOT-COIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE COIN YTD FEE ON THE CURRENCY YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3390-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CCOM-COIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CCOM-COIN-YTD ROUNDED = CCOM-CURRENCY-YTD *
                       CCOM-RATE / 200
               WHEN CCOM-DENOM-CD = 'B'
                   MOVE 5.00 TO CCOM-COIN-YTD
               WHEN CCOM-DENOM-CD = 'D'
                   MOVE 12.00 TO CCOM-COIN-YTD
               WHEN OTHER
                   COMPUTE CCOM-COIN-YTD ROUNDED = CCOM-CURRENCY-YTD *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-COIN-YTD > 0 AND CCOM-COIN-YTD < 2.00
               MOVE 2.00 TO CCOM-COIN-YTD
           END-IF
           IF CCOM-COIN-YTD > 100.00
               MOVE 100.00 TO CCOM-COIN-YTD
           END-IF
           ADD CCOM-COIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT COIN YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = CCOM-COIN-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-COIN-YTD - (WS-WORK-AMT * 12)
           COMPUTE CCOM-VARIANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE VARIANCE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3410-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CHANNEL-CD
                   ADD CCOM-VARIANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-VARIANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SHIPMENT YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3420-LIMIT-SHIPMENT.
           IF CCOM-SHIPMENT-YTD < 0.00
               MOVE 0.00 TO CCOM-SHIPMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-SHIPMENT-YTD > 50000.00
               MOVE 50000.00 TO CCOM-SHIPMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3430-DESCRIBE-REASON.
           EVALUATE CCOM-REASON-CD
               WHEN 'JL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'LW'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'PS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-EXPIRY.
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
                   ADD CCOM-CURRENCY-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-CURRENCY-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-CURRENCY-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD CCOM-CURRENCY-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-CURRENCY-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3450-LIMIT-FEE.
           IF CCOM-FEE-YTD < 10.00
               MOVE 10.00 TO CCOM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-FEE-YTD > 1000.00
               MOVE 1000.00 TO CCOM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COIN SCHEDX TIER FOR THE COIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3460-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHEDX-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF CCOM-COIN-YTD NOT < WS-COIN-SCHEDX-LOW(WS-TX)
                   MOVE WS-COIN-SCHEDX-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * JL -> LW WHEN LAST ACT DATE IS PAST
      * LW -> PS WHEN LAST ACT DATE IS PAST
      * PS -> JL WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3470-SET-REASON.
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
      * SELECT THE COIN SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3480-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHED-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF WS-WORK-AMT NOT < WS-COIN-SCHED-LOW(WS-TX)
                   MOVE WS-COIN-SCHED-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3490-DESCRIBE-SOURCE.
           EVALUATE CCOM-SOURCE-CD
               WHEN 'CFR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'DTX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'EEA'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'EUA'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TPB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WVN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'YTT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE COIN SCHED TIER FOR THE VARIANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3500-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHED-RATE(1) TO CCOM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CCOT-VARIANCE-AMT NOT < WS-COIN-SCHED-LOW(WS-TX)
                   MOVE WS-COIN-SCHED-RATE(WS-TX) TO CCOM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E879 MEMO REQUIRED
      * E827 CURRENCY AMT NEGATIVE
      * E273 CURRENCY AMT OVER 99999
      * E241 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3510-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOT-MEMO = SPACES
                   MOVE 'E879' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT < 0
                   MOVE 'E827' TO WS-REJECT-CODE
               WHEN CCOM-CURRENCY-AMT > 99999.00
                   MOVE 'E273' TO WS-REJECT-CODE
               WHEN CCOM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E241' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STRAP AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3520-FLAG-STRAP.
           IF CCOM-STRAP-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOM-STRAP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CURRENCY YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3530-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-CHANNEL-CD
                   ADD CCOM-CURRENCY-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-CURRENCY-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE STRAP YTD FEE ON THE VARIANCE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3540-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CCOM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CCOM-STRAP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CCOM-STRAP-YTD ROUNDED = CCOM-VARIANCE-AMT *
                       CCOM-FEE-PCT / 200
               WHEN CCOM-SOURCE-CD = 'CFR'
                   MOVE 20.00 TO CCOM-STRAP-YTD
               WHEN CCOM-SOURCE-CD = 'DTX'
                   MOVE 12.00 TO CCOM-STRAP-YTD
               WHEN OTHER
                   COMPUTE CCOM-STRAP-YTD ROUNDED = CCOM-VARIANCE-AMT *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-STRAP-YTD > 0 AND CCOM-STRAP-YTD < 10.00
               MOVE 10.00 TO CCOM-STRAP-YTD
           END-IF
           IF CCOM-STRAP-YTD > 35.00
               MOVE 35.00 TO CCOM-STRAP-YTD
           END-IF
           ADD CCOM-STRAP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3550-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO CCOM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO COIN YTD.
      *----------------------------------------------------------------*
       3560-PRORATE-COIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-COIN-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-COIN-YTD
           END-IF
           ADD CCOM-COIN-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STRAP YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3570-FLAG-STRAP.
           IF CCOM-STRAP-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE CCOM-STRAP-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CURRENCY YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3580-SPLIT-CURRENCY.
           COMPUTE WS-WORK-AMT = CCOM-CURRENCY-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-CURRENCY-YTD - (WS-WORK-AMT *
               12)
           COMPUTE CCOM-CURRENCY-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3590-CHECK-NOTICE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3600-COUNT-RETRY.
           IF CCOM-RETRY-CNT < 9999999
               ADD 1 TO CCOM-RETRY-CNT
           END-IF
           IF CCOM-RETRY-CNT NOT < 3
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * COIN AMT IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3610-LIMIT-COIN.
           IF CCOM-COIN-AMT < 100.00
               MOVE 100.00 TO CCOM-COIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-COIN-AMT > 1000.00
               MOVE 1000.00 TO CCOM-COIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE COIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHIPMENT AMT.
      *----------------------------------------------------------------*
       3620-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-SHIPMENT-AMT ROUNDED =
                   CCOT-COIN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOT-SHIPMENT-AMT
           END-IF
           ADD CCOT-SHIPMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-SOURCE.
           EVALUATE CCOM-SOURCE-CD
               WHEN 'CFR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'DTX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'EEA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'EUA'
                   MOVE 'REVIEW' TO WS-DESC
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
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SHIPMENT AMT.
      *----------------------------------------------------------------*
       3640-PRORATE-SHIPMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOT-SHIPMENT-AMT ROUNDED =
                   CCOM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOT-SHIPMENT-AMT
           END-IF
           ADD CCOT-SHIPMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT COIN YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3650-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = CCOM-COIN-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-COIN-YTD - (WS-WORK-AMT * 12)
           COMPUTE CCOM-SHIPMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E468 NAME REQUIRED
      * E626 VARIANCE AMT NEGATIVE
      * E133 VARIANCE AMT OVER 250000
      * E827 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3660-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-NAME = SPACES
                   MOVE 'E468' TO WS-REJECT-CODE
               WHEN CCOT-VARIANCE-AMT < 0
                   MOVE 'E626' TO WS-REJECT-CODE
               WHEN CCOT-VARIANCE-AMT > 250000.00
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN CCOM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E827' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3670-CHECK-EFF.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E953 REF NO REQUIRED
      * E699 VARIANCE YTD NEGATIVE
      * E120 VARIANCE YTD OVER 99999
      * E187 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-REF-NO = SPACES
                   MOVE 'E953' TO WS-REJECT-CODE
               WHEN CCOM-VARIANCE-YTD < 0
                   MOVE 'E699' TO WS-REJECT-CODE
               WHEN CCOM-VARIANCE-YTD > 99999.00
                   MOVE 'E120' TO WS-REJECT-CODE
               WHEN CCOM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E187' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3690-AGE-OPEN.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E970 DESC REQUIRED
      * E723 SHIPMENT YTD NEGATIVE
      * E919 SHIPMENT YTD OVER 1000000
      * E860 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-DESC = SPACES
                   MOVE 'E970' TO WS-REJECT-CODE
               WHEN CCOM-SHIPMENT-YTD < 0
                   MOVE 'E723' TO WS-REJECT-CODE
               WHEN CCOM-SHIPMENT-YTD > 1000000.00
                   MOVE 'E919' TO WS-REJECT-CODE
               WHEN CCOM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E860' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE STRAP AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3710-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOT-CHANNEL-CD
                   ADD CCOM-STRAP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-STRAP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE VARIANCE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3720-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-FEE-AMT ROUNDED =
                   CCOM-VARIANCE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO CCOM-FEE-AMT
           END-IF
           ADD CCOM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON COIN AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3730-ACCRUE-COIN.
           IF CCOM-COIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-COIN-AMT * CCOM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CCOT-SHIPMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STRAP AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3740-LIMIT-STRAP.
           IF CCOM-STRAP-AMT < 50.00
               MOVE 50.00 TO CCOM-STRAP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-STRAP-AMT > 1000.00
               MOVE 1000.00 TO CCOM-STRAP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT COIN YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3750-SPLIT-COIN.
           COMPUTE WS-WORK-AMT = CCOM-COIN-YTD / 4
           COMPUTE WS-WORK-AMT-2 = CCOM-COIN-YTD - (WS-WORK-AMT * 4)
           COMPUTE CCOM-VARIANCE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE COIN YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3760-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOT-CHANNEL-CD
                   ADD CCOM-COIN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-COIN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DENOM CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3770-DESCRIBE-DENOM.
           EVALUATE CCOM-DENOM-CD
               WHEN 'B'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'D'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'P'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'W'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * COIN AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3780-FLAG-COIN.
           IF CCOT-COIN-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE CCOT-COIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE STRAP AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3790-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CCOM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CCOM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CCOM-FEE-YTD ROUNDED = CCOT-STRAP-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOM-CHANNEL-CD = 'AW'
                   MOVE 20.00 TO CCOM-FEE-YTD
               WHEN CCOM-CHANNEL-CD = 'DW'
                   MOVE 20.00 TO CCOM-FEE-YTD
               WHEN OTHER
                   COMPUTE CCOM-FEE-YTD ROUNDED = CCOT-STRAP-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-FEE-YTD > 0 AND CCOM-FEE-YTD < 1.00
               MOVE 1.00 TO CCOM-FEE-YTD
           END-IF
           IF CCOM-FEE-YTD > 150.00
               MOVE 150.00 TO CCOM-FEE-YTD
           END-IF
           ADD CCOM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-NEXT-DUE.
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
               MOVE 'Y' TO CCOM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE FEE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3810-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CCOM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CCOM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-STOP-FLAG = 'Y'
                   COMPUTE CCOM-FEE-YTD ROUNDED = CCOM-FEE-AMT *
                       CCOM-PENALTY-PCT / 200
               WHEN CCOM-SOURCE-CD = 'CFR'
                   MOVE 10.00 TO CCOM-FEE-YTD
               WHEN CCOM-SOURCE-CD = 'DTX'
                   MOVE 3.00 TO CCOM-FEE-YTD
               WHEN OTHER
                   COMPUTE CCOM-FEE-YTD ROUNDED = CCOM-FEE-AMT *
                       CCOM-PENALTY-PCT / 100
           END-EVALUATE
           IF CCOM-FEE-YTD > 0 AND CCOM-FEE-YTD < 10.00
               MOVE 10.00 TO CCOM-FEE-YTD
           END-IF
           IF CCOM-FEE-YTD > 150.00
               MOVE 150.00 TO CCOM-FEE-YTD
           END-IF
           ADD CCOM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE COIN SCHED TIER FOR THE VARIANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3820-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHED-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CCOT-VARIANCE-AMT NOT < WS-COIN-SCHED-LOW(WS-TX)
                   MOVE WS-COIN-SCHED-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VARIANCE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3830-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = CCOM-VARIANCE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-VARIANCE-YTD - (WS-WORK-AMT *
               12)
           COMPUTE CCOM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3840-AGE-NOTICE.
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
                   ADD CCOM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD CCOM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE COIN YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3850-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = CCOM-STATUS-CD
                   ADD CCOM-COIN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE CCOM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE CCOM-COIN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3860-COUNT-NOTICE.
           IF CCOM-NOTICE-CNT < 9999999
               ADD 1 TO CCOM-NOTICE-CNT
           END-IF
           IF CCOM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO CCOT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * OR -> SH WHEN NOTICE DATE IS PAST
      * SH -> RC WHEN NOTICE DATE IS PAST
      * RC -> VR WHEN NOTICE DATE IS PAST
      * VR -> AD WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3870-SET-TRAN.
           EVALUATE CCOT-TRAN-CD
               WHEN 'OR'
                   IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'SH'
                   IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RC' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RC'
                   IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF CCOM-NOTICE-DATE > 0 AND CCOM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AD' TO CCOT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE STRAP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3880-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE CCOM-FEE-YTD ROUNDED =
                   CCOT-STRAP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO CCOM-FEE-YTD
           END-IF
           ADD CCOM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3890-CHECK-MATURITY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO CCOM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3900-LIMIT-FEE.
           IF CCOM-FEE-YTD < 0.00
               MOVE 0.00 TO CCOM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-FEE-YTD > 500.00
               MOVE 500.00 TO CCOM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3910-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF CCOT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE CCOT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO CCOM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-90 AND OVER 90 DAYS.
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
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-VARIANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE FEE ON THE CURRENCY AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3930-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CCOM-STOP-FLAG = 'Y'
                   MOVE ZERO TO CCOT-VARIANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE CCOT-VARIANCE-AMT ROUNDED = CCOM-CURRENCY-AMT
                       * CCOM-RATE / 200
               WHEN CCOM-SOURCE-CD = 'CFR'
                   MOVE 3.00 TO CCOT-VARIANCE-AMT
               WHEN CCOM-SOURCE-CD = 'DTX'
                   MOVE 12.00 TO CCOT-VARIANCE-AMT
               WHEN OTHER
                   COMPUTE CCOT-VARIANCE-AMT ROUNDED = CCOM-CURRENCY-AMT
                       * CCOM-RATE / 100
           END-EVALUATE
           IF CCOT-VARIANCE-AMT > 0 AND CCOT-VARIANCE-AMT < 1.00
               MOVE 1.00 TO CCOT-VARIANCE-AMT
           END-IF
           IF CCOT-VARIANCE-AMT > 150.00
               MOVE 150.00 TO CCOT-VARIANCE-AMT
           END-IF
           ADD CCOT-VARIANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE COIN SCHED TIER FOR THE COIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHED-RATE(1) TO CCOM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CCOM-COIN-AMT NOT < WS-COIN-SCHED-LOW(WS-TX)
                   MOVE WS-COIN-SCHED-RATE(WS-TX) TO CCOM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE COIN AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3950-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN CCOT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO CCOM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-HOLD-FLAG = 'Y'
                   COMPUTE CCOM-FEE-YTD ROUNDED = CCOM-COIN-AMT *
                       CCOM-RATE / 200
               WHEN CCOM-CHANNEL-CD = 'AW'
                   MOVE 15.00 TO CCOM-FEE-YTD
               WHEN CCOM-CHANNEL-CD = 'DW'
                   MOVE 12.00 TO CCOM-FEE-YTD
               WHEN OTHER
                   COMPUTE CCOM-FEE-YTD ROUNDED = CCOM-COIN-AMT *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-FEE-YTD > 0 AND CCOM-FEE-YTD < 10.00
               MOVE 10.00 TO CCOM-FEE-YTD
           END-IF
           IF CCOM-FEE-YTD > 75.00
               MOVE 75.00 TO CCOM-FEE-YTD
           END-IF
           ADD CCOM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E919 NAME REQUIRED
      * E282 SHIPMENT YTD NEGATIVE
      * E110 SHIPMENT YTD OVER 50000
      * E702 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-NAME = SPACES
                   MOVE 'E919' TO WS-REJECT-CODE
               WHEN CCOM-SHIPMENT-YTD < 0
                   MOVE 'E282' TO WS-REJECT-CODE
               WHEN CCOM-SHIPMENT-YTD > 50000.00
                   MOVE 'E110' TO WS-REJECT-CODE
               WHEN CCOT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E702' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3970-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 2500.00
               MOVE 2500.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHIPMENT FEE ON THE SHIPMENT YTD.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3980-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CCOM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CCOT-SHIPMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-EXEMPT-FLAG = 'Y'
                   COMPUTE CCOT-SHIPMENT-AMT ROUNDED = CCOM-SHIPMENT-YTD
                       * CCOM-FEE-PCT / 200
               WHEN CCOM-STATUS-CD = 'ADH'
                   MOVE 15.00 TO CCOT-SHIPMENT-AMT
               WHEN CCOM-STATUS-CD = 'FVV'
                   MOVE 12.00 TO CCOT-SHIPMENT-AMT
               WHEN OTHER
                   COMPUTE CCOT-SHIPMENT-AMT ROUNDED = CCOM-SHIPMENT-YTD
                       * CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOT-SHIPMENT-AMT > 0 AND CCOT-SHIPMENT-AMT < 10.00
               MOVE 10.00 TO CCOT-SHIPMENT-AMT
           END-IF
           IF CCOT-SHIPMENT-AMT > 35.00
               MOVE 35.00 TO CCOT-SHIPMENT-AMT
           END-IF
           ADD CCOT-SHIPMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3990-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4000-ACCRUE-FEE.
           IF CCOM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   CCOM-FEE-YTD * CCOM-RATE / 100 / 365
               ADD WS-WORK-AMT TO CCOM-COIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STRAP AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4010-LIMIT-STRAP.
           IF CCOM-STRAP-AMT < 10.00
               MOVE 10.00 TO CCOM-STRAP-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF CCOM-STRAP-AMT > 50000.00
               MOVE 50000.00 TO CCOM-STRAP-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT VARIANCE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4020-SPLIT-VARIANCE.
           COMPUTE WS-WORK-AMT = CCOM-VARIANCE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = CCOM-VARIANCE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4030-AGE-EFF.
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
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD CCOM-CURRENCY-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CURRENCY YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4040-SPLIT-CURRENCY.
           COMPUTE WS-WORK-AMT = CCOM-CURRENCY-YTD / 3
           COMPUTE WS-WORK-AMT-2 = CCOM-CURRENCY-YTD - (WS-WORK-AMT * 3)
           COMPUTE CCOM-SHIPMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4050-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * CCOM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO CCOM-COIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E418 DESC REQUIRED
      * E332 STRAP YTD NEGATIVE
      * E112 STRAP YTD OVER 50000
      * E280 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       4060-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN CCOM-DESC = SPACES
                   MOVE 'E418' TO WS-REJECT-CODE
               WHEN CCOM-STRAP-YTD < 0
                   MOVE 'E332' TO WS-REJECT-CODE
               WHEN CCOM-STRAP-YTD > 50000.00
                   MOVE 'E112' TO WS-REJECT-CODE
               WHEN CCOM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E280' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARRIER CD:
      * J -> L WHEN MATURITY DATE IS PAST
      * L -> M WHEN MATURITY DATE IS PAST
      * M -> X WHEN MATURITY DATE IS PAST
      * X -> J WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4070-SET-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4080-CHECK-EFFECTIVE.
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
               MOVE 'Y' TO CCOM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE COIN SCHED TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4090-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-COIN-SCHED-RATE(1) TO CCOM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF CCOM-FEE-YTD NOT < WS-COIN-SCHED-LOW(WS-TX)
                   MOVE WS-COIN-SCHED-RATE(WS-TX) TO CCOM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF CCOM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO CCOM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4100-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO CCOM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CARRIER CD:
      * J -> L WHEN MATURITY DATE IS PAST
      * L -> M WHEN MATURITY DATE IS PAST
      * M -> X WHEN MATURITY DATE IS PAST
      * X -> J WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4110-SET-CARRIER.
           EVALUATE CCOM-CARRIER-CD
               WHEN 'J'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'M' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'M'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'X' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'X'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO CCOM-CARRIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE STRAP YTD FEE ON THE VARIANCE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4120-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN CCOM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO CCOM-STRAP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CCOM-STRAP-YTD ROUNDED = CCOT-VARIANCE-AMT *
                       CCOM-FEE-PCT / 200
               WHEN CCOM-REASON-CD = 'JL'
                   MOVE 3.00 TO CCOM-STRAP-YTD
               WHEN CCOM-REASON-CD = 'LW'
                   MOVE 3.00 TO CCOM-STRAP-YTD
               WHEN OTHER
                   COMPUTE CCOM-STRAP-YTD ROUNDED = CCOT-VARIANCE-AMT *
                       CCOM-FEE-PCT / 100
           END-EVALUATE
           IF CCOM-STRAP-YTD > 0 AND CCOM-STRAP-YTD < 2.00
               MOVE 2.00 TO CCOM-STRAP-YTD
           END-IF
           IF CCOM-STRAP-YTD > 150.00
               MOVE 150.00 TO CCOM-STRAP-YTD
           END-IF
           ADD CCOM-STRAP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * AW -> DW WHEN MATURITY DATE IS PAST
      * DW -> GN WHEN MATURITY DATE IS PAST
      * GN -> RS WHEN MATURITY DATE IS PAST
      * RS -> UZ WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4130-SET-CHANNEL.
           EVALUATE CCOM-CHANNEL-CD
               WHEN 'AW'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DW' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'DW'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GN' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GN'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RS' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RS'
                   IF CCOM-MATURITY-DATE > 0 AND CCOM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UZ' TO CCOM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE VARIANCE YTD FEE ON THE COIN YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4140-ASSESS-STOP.
           EVALUATE TRUE
               WHEN CCOM-STOP-FLAG = 'Y'
                   MOVE ZERO TO CCOM-VARIANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN CCOM-OVERRIDE-FLAG = 'Y'
                   COMPUTE CCOM-VARIANCE-YTD ROUNDED = CCOM-COIN-YTD *
                       CCOM-RATE / 200
               WHEN CCOM-CHANNEL-CD = 'AW'
                   MOVE 10.00 TO CCOM-VARIANCE-YTD
               WHEN CCOM-CHANNEL-CD = 'DW'
                   MOVE 15.00 TO CCOM-VARIANCE-YTD
               WHEN OTHER
                   COMPUTE CCOM-VARIANCE-YTD ROUNDED = CCOM-COIN-YTD *
                       CCOM-RATE / 100
           END-EVALUATE
           IF CCOM-VARIANCE-YTD > 0 AND CCOM-VARIANCE-YTD < 1.00
               MOVE 1.00 TO CCOM-VARIANCE-YTD
           END-IF
           IF CCOM-VARIANCE-YTD > 25.00
               MOVE 25.00 TO CCOM-VARIANCE-YTD
           END-IF
           ADD CCOM-VARIANCE-YTD TO WS-TOTAL-FEES.

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
                   MOVE CNCAL-FEE-YTD
                     TO CCOM-FEE-YTD
                   MOVE CNCAL-SHIPMENT-AMT
                     TO CCOM-SHIPMENT-AMT
                   MOVE CNCAL-EXEMPT-FLAG
                     TO CCOM-EXEMPT-FLAG
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
                   MOVE CNPEN-STRAP-YTD
                     TO CCOM-STRAP-YTD
                   MOVE CNPEN-SHIPMENT-YTD
                     TO CCOM-SHIPMENT-YTD
                   MOVE CNPEN-CURRENCY-YTD
                     TO CCOM-CURRENCY-YTD
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
                   MOVE CNDTE-FEE-AMT
                     TO CCOM-FEE-AMT
                   MOVE CNDTE-NOTICE-SENT-FLAG
                     TO CCOM-NOTICE-SENT-FLAG
                   MOVE CNDTE-HOLD-FLAG
                     TO CCOM-HOLD-FLAG
                   MOVE CNDTE-AUTO-RENEW-FLAG
                     TO CCOM-AUTO-RENEW-FLAG
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
           DISPLAY 'CNOCONV READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'CNOCONV' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
