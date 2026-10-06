      *================================================================*
      * PROGRAM  : ESCSTMT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MORTGAGE ESCROW - STATEMENT PREPARATION. READS THE  *
      *            ESC TRANSACTIONS, UPDATES THE ESC MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. ESCROW ANALYSIS, TAX   *
      *            AND INSURANCE DISBURSEMENT AND SHORTAGE             *
      * INPUT    : ESCMST (KSDS), ESCTRN, BANKCTL                      *
      * OUTPUT   : ESCOUT, ESCSTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ESVAL, ESRISK           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-05-28 SKN  ORIGINAL                                       *
      * 1998-09-06 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-12-22 GAF  CUSHION TABLE EXPANDED CHG5990                 *
      * 2002-09-06 LMB  CUSHION NOTICE TEXT PER LEGAL INC7975          *
      * 2003-04-11 TWH  SHORTAGE NOTICE TEXT PER LEGAL REQ9126         *
      * 2009-08-19 JLM  INSURANCE ROUNDING - HALF UP REQ2655           *
      * 2010-05-19 EWS  SHORTAGE CALCULATION CORRECTED CHG3072         *
      * 2011-01-04 EWS  AUDIT FINDING - PAYMENT CONTROL TOTALS PRB1318 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESCSTMT.
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
           SELECT REPORT-FILE ASSIGN TO "ESCSTRPT"
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
                                           VALUE 11.000000.
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
      * WS-SHORTAGE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-SHORTAGE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000165500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000055000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000126500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005500000000714500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000599000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018000000000659500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020500000000333500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020600000000879000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020700000000863500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021200000000074000'.
       01  WS-SHORTAGE-SCHED-TBL REDEFINES WS-SHORTAGE-SCHED-VALUES.
           05  WS-SHORTAGE-SCHED-ROW OCCURS 10 TIMES.
               10  WS-SHORTAGE-SCHED-LOW   PIC 9(09)V99.
               10  WS-SHORTAGE-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYMENT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000814500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000519500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010050000000207000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015050000000306000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017550000000134500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020050000000846500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022550000000092500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022650000000523000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032650000000690000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00032750000000061500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033250000000436500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033750000000020000'.
       01  WS-PAYMENT-BAND-TBL REDEFINES WS-PAYMENT-BAND-VALUES.
           05  WS-PAYMENT-BAND-ROW OCCURS 12 TIMES.
               10  WS-PAYMENT-BAND-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ESCSTMT  MORTGAGE ESCROW'.
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
       COPY ESRISKP.

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
               PERFORM 3000-ACCRUE-TAX
               PERFORM 3010-ACCRUE-SURPLUS
               PERFORM 3020-AGE-CLOSE
               PERFORM 3030-SELECT-RATE
               PERFORM 3040-CHECK-NEXT-REVIEW
               PERFORM 3050-COUNT-EXCEPT
               PERFORM 3060-SELECT-RATE
               PERFORM 3070-ACCUM-TRAN
               PERFORM 3080-ACCUM-STATUS
               PERFORM 3090-SPLIT-INSURANCE
               PERFORM 3100-COUNT-CYCLE
               PERFORM 3110-CHECK-NEXT-REVIEW
               PERFORM 3120-DESCRIBE-REGION
               PERFORM 3130-PRORATE-PAYMENT
               PERFORM 3140-ACCRUE-SURPLUS
               PERFORM 3150-SET-PAYEE-TYPE
               PERFORM 3160-ASSESS-SUSPEND
               PERFORM 3170-SELECT-RATE
               PERFORM 3180-CHECK-EFF
               PERFORM 3190-SELECT-RATE
               PERFORM 3200-ACCRUE-SHORTAGE
               PERFORM 3210-EDIT-DESC
               PERFORM 3220-PRORATE-SHORTAGE
               PERFORM 3230-FLAG-CUSHION
               PERFORM 3240-SPLIT-SURPLUS
               PERFORM 3250-ACCRUE-DISBURSE
               PERFORM 3260-EDIT-NAME
               PERFORM 3270-PRORATE-TAX
               PERFORM 3280-COUNT-CYCLE
               PERFORM 3290-LIMIT-INSURANCE
               PERFORM 3300-FLAG-PAYMENT
               PERFORM 3310-SPLIT-DISBURSE
               PERFORM 3320-ASSESS-REVERSAL
               PERFORM 3330-DESCRIBE-PAYEE-TYPE
               PERFORM 3340-LIMIT-TAX
               PERFORM 3350-SELECT-RATE
               PERFORM 3360-ACCRUE-PAYMENT
               PERFORM 3370-FLAG-ITEM
               PERFORM 3380-PRORATE-INSURANCE
               PERFORM 3390-AGE-EFF
               PERFORM 3400-ACCUM-REASON
               PERFORM 3410-ACCUM-STATUS
               PERFORM 3420-SPLIT-SHORTAGE
               PERFORM 3430-FLAG-INSURANCE
               PERFORM 3440-PRORATE-PAYMENT
               PERFORM 3450-EDIT-NAME
               PERFORM 3460-LIMIT-PAYMENT
               PERFORM 3470-LIMIT-INSURANCE
               PERFORM 3480-SET-CHANNEL
               PERFORM 3490-COUNT-NOTICE
               PERFORM 3500-ACCRUE-SURPLUS
               PERFORM 3510-ACCUM-ANALYSIS
               PERFORM 3520-ASSESS-STOP
               PERFORM 3530-ACCRUE-FEE-PCT
               PERFORM 3540-AGE-EFFECTIVE
               PERFORM 3550-ASSESS-OVERRIDE
               PERFORM 3560-PRORATE-CUSHION
               PERFORM 3570-PRORATE-INSURANCE
               PERFORM 3580-ACCRUE-PAYMENT
               PERFORM 3590-AGE-CLOSE
               PERFORM 3600-DESCRIBE-REASON
               PERFORM 3610-ACCUM-PAYEE-TYPE
               PERFORM 3620-AGE-EFF
               PERFORM 3630-EDIT-REF-NO
               PERFORM 3640-EDIT-CUST-ID
               PERFORM 3650-SPLIT-CUSHION
               PERFORM 3660-SPLIT-PAYMENT
               PERFORM 3670-ASSESS-OVERRIDE
               PERFORM 3680-SET-ANALYSIS
               PERFORM 3690-COUNT-SEQ
               PERFORM 3700-ACCRUE-CUSHION
               PERFORM 3710-ACCRUE-PAYMENT
               PERFORM 3720-SET-ANALYSIS
               PERFORM 3730-PRORATE-DISBURSE
               PERFORM 3740-COUNT-NOTICE
               PERFORM 3750-EDIT-ADDR
               PERFORM 3760-COUNT-NOTICE
               PERFORM 3770-EDIT-ADDR
               PERFORM 3780-FLAG-SURPLUS
               PERFORM 3790-EDIT-DESC
               PERFORM 3800-ACCUM-REASON
               PERFORM 3810-AGE-EXPIRY
               PERFORM 3820-DESCRIBE-PAYEE-TYPE
               PERFORM 3830-FLAG-ITEM
               PERFORM 3840-SELECT-RATE
               PERFORM 3850-SET-PAYEE-TYPE
               PERFORM 3860-ASSESS-STOP
               PERFORM 3870-EDIT-NAME
               PERFORM 3880-ACCUM-REGION
               PERFORM 3890-LIMIT-INSURANCE
               PERFORM 3900-ASSESS-SUSPEND
               PERFORM 3910-AGE-EFFECTIVE
               PERFORM 3920-FLAG-SHORTAGE
               PERFORM 3930-FLAG-INSURANCE
               PERFORM 3940-COUNT-RETRY
               PERFORM 3950-ACCRUE-PAYMENT
               PERFORM 3960-ASSESS-PREFERRED
               PERFORM 3970-ACCRUE-SHORTAGE
               PERFORM 3980-LIMIT-SHORTAGE
               PERFORM 3990-ASSESS-OVERRIDE
               PERFORM 4000-PRORATE-CUSHION
               PERFORM 4010-LIMIT-DISBURSE
               PERFORM 4020-ACCRUE-TAX
               PERFORM 4030-DESCRIBE-CHANNEL
               PERFORM 4040-SELECT-RATE
               PERFORM 4050-ACCUM-TRAN
               PERFORM 4060-EDIT-CONTACT
               PERFORM 4070-AGE-EFF
               PERFORM 4080-ACCUM-TYPE
               PERFORM 4090-CHECK-LAST-STMT
               PERFORM 4100-SPLIT-SHORTAGE
               PERFORM 4110-SELECT-DISCOUNT
               PERFORM 4120-ACCUM-REASON
               PERFORM 4130-AGE-LAST-PAY
               PERFORM 4140-ASSESS-NOTICE-SENT
               PERFORM 4150-SELECT-RATE
               PERFORM 4160-PRORATE-SHORTAGE
               PERFORM 4170-ACCRUE-SURPLUS
               PERFORM 4180-PRORATE-CUSHION
               PERFORM 4190-ACCUM-PAYEE-TYPE
               PERFORM 4200-SPLIT-INSURANCE
               PERFORM 4210-LIMIT-PAYMENT
               PERFORM 4220-AGE-POST
               PERFORM 4230-SPLIT-INSURANCE
               PERFORM 4240-COUNT-RETRY
               PERFORM 4250-SELECT-FEE-PCT
               PERFORM 4260-LIMIT-TAX
               PERFORM 4270-FLAG-PAYMENT
               PERFORM 4280-PRORATE-PAYMENT
               PERFORM 4290-LIMIT-SHORTAGE
               PERFORM 4300-DESCRIBE-STATUS
               PERFORM 4310-EDIT-DESC
               PERFORM 4320-ACCUM-ANALYSIS
               PERFORM 7020-CALL-ESVAL
               PERFORM 7030-CALL-ESRISK
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
      * DAILY INTEREST ON TAX YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3000-ACCRUE-TAX.
           IF ESCM-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-TAX-YTD * ESCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESCT-CUSHION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURPLUS YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3010-ACCRUE-SURPLUS.
           IF ESCM-SURPLUS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SURPLUS-YTD * ESCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-CUSHION-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3020-AGE-CLOSE.
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
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE CUSHION YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3030-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ESCM-CUSHION-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 90 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3040-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO ESCM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3050-COUNT-EXCEPT.
           IF ESCM-EXCEPT-CNT < 9999999
               ADD 1 TO ESCM-EXCEPT-CNT
           END-IF
           IF ESCM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE INSURANCE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3060-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ESCM-INSURANCE-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3070-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCT-TRAN-CD
                   ADD ESCM-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3080-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-STATUS-CD
                   ADD ESCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3090-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESCM-INSURANCE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ESCM-INSURANCE-YTD - (WS-WORK-AMT *
               4)
           COMPUTE ESCM-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3100-COUNT-CYCLE.
           IF ESCM-CYCLE-CNT < 9999999
               ADD 1 TO ESCM-CYCLE-CNT
           END-IF
           IF ESCM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO ESCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3110-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3120-DESCRIBE-REGION.
           EVALUATE ESCM-REGION-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'G'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'Y'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3130-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-PAYMENT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-PAYMENT-AMT
           END-IF
           ADD ESCM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURPLUS AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3140-ACCRUE-SURPLUS.
           IF ESCM-SURPLUS-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SURPLUS-AMT * ESCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * AM -> UE WHEN POST DATE IS PAST
      * UE -> UP WHEN POST DATE IS PAST
      * UP -> AM WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3150-SET-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UE' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UE'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UP' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UP'
                   IF ESCT-POST-DATE > 0 AND ESCT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AM' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SURPLUS FEE ON THE DISBURSE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3160-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SURPLUS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   COMPUTE ESCM-SURPLUS-AMT ROUNDED = ESCM-DISBURSE-AMT
                       * ESCM-DISCOUNT / 200
               WHEN ESCT-TRAN-CD = 'DS'
                   MOVE 7.00 TO ESCM-SURPLUS-AMT
               WHEN ESCT-TRAN-CD = 'PY'
                   MOVE 10.00 TO ESCM-SURPLUS-AMT
               WHEN OTHER
                   COMPUTE ESCM-SURPLUS-AMT ROUNDED = ESCM-DISBURSE-AMT
                       * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-SURPLUS-AMT > 0 AND ESCM-SURPLUS-AMT < 10.00
               MOVE 10.00 TO ESCM-SURPLUS-AMT
           END-IF
           IF ESCM-SURPLUS-AMT > 250.00
               MOVE 250.00 TO ESCM-SURPLUS-AMT
           END-IF
           ADD ESCM-SURPLUS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE CUSHION AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3170-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ESCT-CUSHION-AMT NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3180-CHECK-EFF.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO ESCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE SCHED TIER FOR THE SURPLUS AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3190-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-SCHED-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ESCM-SURPLUS-AMT NOT < WS-SHORTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-SHORTAGE-SCHED-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3200-ACCRUE-SHORTAGE.
           IF ESCM-SHORTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SHORTAGE-AMT * ESCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E252 DESC REQUIRED
      * E710 DISBURSE AMT NEGATIVE
      * E276 DISBURSE AMT OVER 50000
      * E785 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3210-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-DESC = SPACES
                   MOVE 'E252' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-AMT < 0
                   MOVE 'E710' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-AMT > 50000.00
                   MOVE 'E276' TO WS-REJECT-CODE
               WHEN ESCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E785' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INSURANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHORTAGE AMT.
      *----------------------------------------------------------------*
       3220-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCT-SHORTAGE-AMT ROUNDED =
                   ESCT-INSURANCE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCT-SHORTAGE-AMT
           END-IF
           ADD ESCT-SHORTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CUSHION AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3230-FLAG-CUSHION.
           IF ESCT-CUSHION-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCT-CUSHION-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SURPLUS YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3240-SPLIT-SURPLUS.
           COMPUTE WS-WORK-AMT = ESCM-SURPLUS-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-SURPLUS-YTD - (WS-WORK-AMT * 3)
           COMPUTE ESCM-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON DISBURSE YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-DISBURSE.
           IF ESCM-DISBURSE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-DISBURSE-YTD * ESCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E527 NAME REQUIRED
      * E393 CUSHION AMT NEGATIVE
      * E431 CUSHION AMT OVER 99999
      * E133 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-NAME = SPACES
                   MOVE 'E527' TO WS-REJECT-CODE
               WHEN ESCT-CUSHION-AMT < 0
                   MOVE 'E393' TO WS-REJECT-CODE
               WHEN ESCT-CUSHION-AMT > 99999.00
                   MOVE 'E431' TO WS-REJECT-CODE
               WHEN ESCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E133' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO TAX YTD.
      *----------------------------------------------------------------*
       3270-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-TAX-YTD ROUNDED =
                   ESCM-DISBURSE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-TAX-YTD
           END-IF
           ADD ESCM-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3280-COUNT-CYCLE.
           IF ESCM-CYCLE-CNT < 9999999
               ADD 1 TO ESCM-CYCLE-CNT
           END-IF
           IF ESCM-CYCLE-CNT NOT < 3
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE AMT IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3290-LIMIT-INSURANCE.
           IF ESCT-INSURANCE-AMT < 25.00
               MOVE 25.00 TO ESCT-INSURANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCT-INSURANCE-AMT > 5000.00
               MOVE 5000.00 TO ESCT-INSURANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3300-FLAG-PAYMENT.
           IF ESCT-PAYMENT-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3310-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE ESCM-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE INSURANCE AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3320-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ESCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ESCM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   COMPUTE ESCM-PAYMENT-YTD ROUNDED = ESCT-INSURANCE-AMT
                       * ESCM-DISCOUNT / 200
               WHEN ESCM-TYPE-CD = 'AKX'
                   MOVE 10.00 TO ESCM-PAYMENT-YTD
               WHEN ESCM-TYPE-CD = 'REP'
                   MOVE 3.00 TO ESCM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE ESCM-PAYMENT-YTD ROUNDED = ESCT-INSURANCE-AMT
                       * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-PAYMENT-YTD > 0 AND ESCM-PAYMENT-YTD < 5.00
               MOVE 5.00 TO ESCM-PAYMENT-YTD
           END-IF
           IF ESCM-PAYMENT-YTD > 50.00
               MOVE 50.00 TO ESCM-PAYMENT-YTD
           END-IF
           ADD ESCM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3330-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UE'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'UP'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TAX YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3340-LIMIT-TAX.
           IF ESCM-TAX-YTD < 0.00
               MOVE 0.00 TO ESCM-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-TAX-YTD > 2500.00
               MOVE 2500.00 TO ESCM-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-SCHED-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT-2 NOT < WS-SHORTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-SHORTAGE-SCHED-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3360-ACCRUE-PAYMENT.
           IF ESCT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCT-PAYMENT-AMT * ESCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3370-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO INSURANCE YTD.
      *----------------------------------------------------------------*
       3380-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-INSURANCE-YTD ROUNDED =
                   ESCM-SHORTAGE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-INSURANCE-YTD
           END-IF
           ADD ESCM-INSURANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3390-AGE-EFF.
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
                   ADD ESCM-INSURANCE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-INSURANCE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-INSURANCE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-INSURANCE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-INSURANCE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3400-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REASON-CD
                   ADD ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3410-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-STATUS-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHORTAGE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3420-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = ESCT-SHORTAGE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ESCT-SHORTAGE-AMT - (WS-WORK-AMT * 2)
           COMPUTE ESCM-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3430-FLAG-INSURANCE.
           IF ESCT-INSURANCE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCT-INSURANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       3440-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-PAYMENT-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-PAYMENT-YTD
           END-IF
           ADD ESCM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E483 NAME REQUIRED
      * E055 WORK AMT 2 NEGATIVE
      * E398 WORK AMT 2 OVER 5000
      * E955 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       3450-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-NAME = SPACES
                   MOVE 'E483' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E055' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E398' TO WS-REJECT-CODE
               WHEN ESCT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E955' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3460-LIMIT-PAYMENT.
           IF ESCM-PAYMENT-YTD < 50.00
               MOVE 50.00 TO ESCM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-PAYMENT-YTD > 1000.00
               MOVE 1000.00 TO ESCM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3470-LIMIT-INSURANCE.
           IF ESCM-INSURANCE-AMT < 50.00
               MOVE 50.00 TO ESCM-INSURANCE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-INSURANCE-AMT > 500.00
               MOVE 500.00 TO ESCM-INSURANCE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST ACT DATE IS PAST
      * BAT -> MAL WHEN LAST ACT DATE IS PAST
      * MAL -> TEL WHEN LAST ACT DATE IS PAST
      * TEL -> ONL WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3480-SET-CHANNEL.
           EVALUATE ESCT-CHANNEL-CD
               WHEN 'BRN'
                   IF ESCM-LAST-ACT-DATE > 0 AND ESCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ESCM-LAST-ACT-DATE > 0 AND ESCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ESCM-LAST-ACT-DATE > 0 AND ESCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ESCM-LAST-ACT-DATE > 0 AND ESCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3490-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 3
               MOVE 'Y' TO ESCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURPLUS YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3500-ACCRUE-SURPLUS.
           IF ESCM-SURPLUS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SURPLUS-YTD * ESCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHORTAGE AMT BY ANALYSIS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-ANALYSIS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-ANALYSIS-CD
                   ADD ESCM-SHORTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-ANALYSIS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-SHORTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DISBURSE FEE ON THE PAYMENT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3520-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ESCM-DISBURSE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   COMPUTE ESCM-DISBURSE-AMT ROUNDED = ESCT-PAYMENT-AMT
                       * ESCM-FEE-PCT / 200
               WHEN ESCT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO ESCM-DISBURSE-AMT
               WHEN ESCT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO ESCM-DISBURSE-AMT
               WHEN OTHER
                   COMPUTE ESCM-DISBURSE-AMT ROUNDED = ESCT-PAYMENT-AMT
                       * ESCM-FEE-PCT / 100
           END-EVALUATE
           IF ESCM-DISBURSE-AMT > 0 AND ESCM-DISBURSE-AMT < 1.00
               MOVE 1.00 TO ESCM-DISBURSE-AMT
           END-IF
           IF ESCM-DISBURSE-AMT > 250.00
               MOVE 250.00 TO ESCM-DISBURSE-AMT
           END-IF
           ADD ESCM-DISBURSE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3530-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ESCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-CUSHION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3540-AGE-EFFECTIVE.
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
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CUSHION FEE ON THE DISBURSE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3550-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ESCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ESCT-CUSHION-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-STOP-FLAG = 'Y'
                   COMPUTE ESCT-CUSHION-AMT ROUNDED = ESCM-DISBURSE-AMT
                       * ESCM-RATE / 200
               WHEN ESCM-STATUS-CD = 'A'
                   MOVE 7.00 TO ESCT-CUSHION-AMT
               WHEN ESCM-STATUS-CD = 'E'
                   MOVE 3.00 TO ESCT-CUSHION-AMT
               WHEN OTHER
                   COMPUTE ESCT-CUSHION-AMT ROUNDED = ESCM-DISBURSE-AMT
                       * ESCM-RATE / 100
           END-EVALUATE
           IF ESCT-CUSHION-AMT > 0 AND ESCT-CUSHION-AMT < 10.00
               MOVE 10.00 TO ESCT-CUSHION-AMT
           END-IF
           IF ESCT-CUSHION-AMT > 150.00
               MOVE 150.00 TO ESCT-CUSHION-AMT
           END-IF
           ADD ESCT-CUSHION-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE CUSHION AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CUSHION AMT.
      *----------------------------------------------------------------*
       3560-PRORATE-CUSHION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-CUSHION-AMT ROUNDED =
                   ESCT-CUSHION-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-CUSHION-AMT
           END-IF
           ADD ESCM-CUSHION-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO INSURANCE YTD.
      *----------------------------------------------------------------*
       3570-PRORATE-INSURANCE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-INSURANCE-YTD ROUNDED =
                   ESCM-SHORTAGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-INSURANCE-YTD
           END-IF
           ADD ESCM-INSURANCE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3580-ACCRUE-PAYMENT.
           IF ESCT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCT-PAYMENT-AMT * ESCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-CLOSE.
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
                   ADD ESCM-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3600-DESCRIBE-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'F'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'K'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'L'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'S'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'X'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE AMT BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3610-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-PAYEE-TYPE-CD
                   ADD ESCM-DISBURSE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-DISBURSE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3620-AGE-EFF.
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
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SURPLUS-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E109 REF NO REQUIRED
      * E772 PAYMENT AMT NEGATIVE
      * E711 PAYMENT AMT OVER 50000
      * E512 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-REF-NO = SPACES
                   MOVE 'E109' TO WS-REJECT-CODE
               WHEN ESCT-PAYMENT-AMT < 0
                   MOVE 'E772' TO WS-REJECT-CODE
               WHEN ESCT-PAYMENT-AMT > 50000.00
                   MOVE 'E711' TO WS-REJECT-CODE
               WHEN ESCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E512' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E948 CUST ID REQUIRED
      * E692 SHORTAGE YTD NEGATIVE
      * E186 SHORTAGE YTD OVER 25000
      * E848 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       3640-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CUST-ID = SPACES
                   MOVE 'E948' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-YTD < 0
                   MOVE 'E692' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-YTD > 25000.00
                   MOVE 'E186' TO WS-REJECT-CODE
               WHEN ESCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E848' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CUSHION AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3650-SPLIT-CUSHION.
           COMPUTE WS-WORK-AMT = ESCM-CUSHION-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ESCM-CUSHION-AMT - (WS-WORK-AMT * 2)
           COMPUTE ESCM-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3660-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = ESCT-PAYMENT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ESCT-PAYMENT-AMT - (WS-WORK-AMT * 4)
           COMPUTE ESCT-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE FEE ON THE SHORTAGE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3670-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ESCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SHORTAGE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ESCM-SHORTAGE-AMT ROUNDED = ESCT-SHORTAGE-AMT
                       * ESCM-RATE / 200
               WHEN ESCM-STATUS-CD = 'A'
                   MOVE 10.00 TO ESCM-SHORTAGE-AMT
               WHEN ESCM-STATUS-CD = 'E'
                   MOVE 5.00 TO ESCM-SHORTAGE-AMT
               WHEN OTHER
                   COMPUTE ESCM-SHORTAGE-AMT ROUNDED = ESCT-SHORTAGE-AMT
                       * ESCM-RATE / 100
           END-EVALUATE
           IF ESCM-SHORTAGE-AMT > 0 AND ESCM-SHORTAGE-AMT < 2.00
               MOVE 2.00 TO ESCM-SHORTAGE-AMT
           END-IF
           IF ESCM-SHORTAGE-AMT > 75.00
               MOVE 75.00 TO ESCM-SHORTAGE-AMT
           END-IF
           ADD ESCM-SHORTAGE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ANALYSIS CD:
      * GD -> LY WHEN EFFECTIVE DATE IS PAST
      * LY -> XJ WHEN EFFECTIVE DATE IS PAST
      * XJ -> GD WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3680-SET-ANALYSIS.
           EVALUATE ESCM-ANALYSIS-CD
               WHEN 'GD'
                   IF ESCM-EFFECTIVE-DATE > 0 AND ESCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LY' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LY'
                   IF ESCM-EFFECTIVE-DATE > 0 AND ESCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XJ'
                   IF ESCM-EFFECTIVE-DATE > 0 AND ESCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GD' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-SEQ.
           IF ESCT-SEQ < 9999999
               ADD 1 TO ESCT-SEQ
           END-IF
           IF ESCT-SEQ NOT < 6
               MOVE 'Y' TO ESCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CUSHION AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3700-ACCRUE-CUSHION.
           IF ESCM-CUSHION-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-CUSHION-AMT * ESCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3710-ACCRUE-PAYMENT.
           IF ESCM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-PAYMENT-AMT * ESCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ANALYSIS CD:
      * GD -> LY WHEN EFFECTIVE DATE IS PAST
      * LY -> XJ WHEN EFFECTIVE DATE IS PAST
      * XJ -> GD WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3720-SET-ANALYSIS.
           EVALUATE ESCM-ANALYSIS-CD
               WHEN 'GD'
                   IF ESCM-EFFECTIVE-DATE > 0 AND ESCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LY' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LY'
                   IF ESCM-EFFECTIVE-DATE > 0 AND ESCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XJ'
                   IF ESCM-EFFECTIVE-DATE > 0 AND ESCM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GD' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TAX AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE AMT.
      *----------------------------------------------------------------*
       3730-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-DISBURSE-AMT ROUNDED =
                   ESCM-TAX-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-DISBURSE-AMT
           END-IF
           ADD ESCM-DISBURSE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3740-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 10
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E828 ADDR REQUIRED
      * E397 WORK AMT 2 NEGATIVE
      * E031 WORK AMT 2 OVER 5000
      * E194 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3750-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-ADDR = SPACES
                   MOVE 'E828' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E397' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 5000.00
                   MOVE 'E031' TO WS-REJECT-CODE
               WHEN ESCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E194' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3760-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 6
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E981 ADDR REQUIRED
      * E799 SHORTAGE AMT NEGATIVE
      * E520 SHORTAGE AMT OVER 25000
      * E765 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3770-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-ADDR = SPACES
                   MOVE 'E981' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-AMT < 0
                   MOVE 'E799' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-AMT > 25000.00
                   MOVE 'E520' TO WS-REJECT-CODE
               WHEN ESCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E765' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3780-FLAG-SURPLUS.
           IF ESCM-SURPLUS-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-SURPLUS-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E715 DESC REQUIRED
      * E695 SURPLUS YTD NEGATIVE
      * E547 SURPLUS YTD OVER 250000
      * E309 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3790-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-DESC = SPACES
                   MOVE 'E715' TO WS-REJECT-CODE
               WHEN ESCM-SURPLUS-YTD < 0
                   MOVE 'E695' TO WS-REJECT-CODE
               WHEN ESCM-SURPLUS-YTD > 250000.00
                   MOVE 'E547' TO WS-REJECT-CODE
               WHEN ESCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E309' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3800-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REASON-CD
                   ADD ESCT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3810-AGE-EXPIRY.
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
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3830-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE SCHED TIER FOR THE SHORTAGE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-SCHED-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ESCM-SHORTAGE-YTD NOT < WS-SHORTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-SHORTAGE-SCHED-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * AM -> UE WHEN NEXT REVIEW DATE IS PAST
      * UE -> UP WHEN NEXT REVIEW DATE IS PAST
      * UP -> AM WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3850-SET-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UE' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UE'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UP' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UP'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'AM' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE SURPLUS FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3860-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SURPLUS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ESCM-SURPLUS-AMT ROUNDED = ESCM-SHORTAGE-YTD
                       * ESCM-DISCOUNT / 200
               WHEN ESCT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO ESCM-SURPLUS-AMT
               WHEN ESCT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO ESCM-SURPLUS-AMT
               WHEN OTHER
                   COMPUTE ESCM-SURPLUS-AMT ROUNDED = ESCM-SHORTAGE-YTD
                       * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-SURPLUS-AMT > 0 AND ESCM-SURPLUS-AMT < 2.00
               MOVE 2.00 TO ESCM-SURPLUS-AMT
           END-IF
           IF ESCM-SURPLUS-AMT > 250.00
               MOVE 250.00 TO ESCM-SURPLUS-AMT
           END-IF
           ADD ESCM-SURPLUS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E703 NAME REQUIRED
      * E226 TAX YTD NEGATIVE
      * E964 TAX YTD OVER 1000000
      * E446 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3870-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-NAME = SPACES
                   MOVE 'E703' TO WS-REJECT-CODE
               WHEN ESCM-TAX-YTD < 0
                   MOVE 'E226' TO WS-REJECT-CODE
               WHEN ESCM-TAX-YTD > 1000000.00
                   MOVE 'E964' TO WS-REJECT-CODE
               WHEN ESCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E446' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3880-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REGION-CD
                   ADD ESCM-INSURANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE YTD IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3890-LIMIT-INSURANCE.
           IF ESCM-INSURANCE-YTD < 0.00
               MOVE 0.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-INSURANCE-YTD > 50000.00
               MOVE 50000.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CUSHION FEE ON THE DISBURSE AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3900-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO ESCM-CUSHION-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   COMPUTE ESCM-CUSHION-AMT ROUNDED = ESCM-DISBURSE-AMT
                       * ESCM-DISCOUNT / 200
               WHEN ESCM-PAYEE-TYPE-CD = 'AM'
                   MOVE 7.00 TO ESCM-CUSHION-AMT
               WHEN ESCM-PAYEE-TYPE-CD = 'UE'
                   MOVE 7.00 TO ESCM-CUSHION-AMT
               WHEN OTHER
                   COMPUTE ESCM-CUSHION-AMT ROUNDED = ESCM-DISBURSE-AMT
                       * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-CUSHION-AMT > 0 AND ESCM-CUSHION-AMT < 10.00
               MOVE 10.00 TO ESCM-CUSHION-AMT
           END-IF
           IF ESCM-CUSHION-AMT > 150.00
               MOVE 150.00 TO ESCM-CUSHION-AMT
           END-IF
           ADD ESCM-CUSHION-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3910-AGE-EFFECTIVE.
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
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHORTAGE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3920-FLAG-SHORTAGE.
           IF ESCM-SHORTAGE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-SHORTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3930-FLAG-INSURANCE.
           IF ESCT-INSURANCE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCT-INSURANCE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3940-COUNT-RETRY.
           IF ESCM-RETRY-CNT < 9999999
               ADD 1 TO ESCM-RETRY-CNT
           END-IF
           IF ESCM-RETRY-CNT NOT < 3
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3950-ACCRUE-PAYMENT.
           IF ESCM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-PAYMENT-AMT * ESCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-CUSHION-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURPLUS YTD FEE ON THE PAYMENT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3960-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SURPLUS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ESCM-SURPLUS-YTD ROUNDED = ESCM-PAYMENT-YTD *
                       ESCM-DISCOUNT / 200
               WHEN ESCM-REASON-CD = 'C'
                   MOVE 20.00 TO ESCM-SURPLUS-YTD
               WHEN ESCM-REASON-CD = 'F'
                   MOVE 10.00 TO ESCM-SURPLUS-YTD
               WHEN OTHER
                   COMPUTE ESCM-SURPLUS-YTD ROUNDED = ESCM-PAYMENT-YTD *
                       ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-SURPLUS-YTD > 0 AND ESCM-SURPLUS-YTD < 10.00
               MOVE 10.00 TO ESCM-SURPLUS-YTD
           END-IF
           IF ESCM-SURPLUS-YTD > 150.00
               MOVE 150.00 TO ESCM-SURPLUS-YTD
           END-IF
           ADD ESCM-SURPLUS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3970-ACCRUE-SHORTAGE.
           IF ESCM-SHORTAGE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SHORTAGE-YTD * ESCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3980-LIMIT-SHORTAGE.
           IF ESCT-SHORTAGE-AMT < 25.00
               MOVE 25.00 TO ESCT-SHORTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCT-SHORTAGE-AMT > 50000.00
               MOVE 50000.00 TO ESCT-SHORTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SHORTAGE YTD FEE ON THE PAYMENT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3990-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ESCM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SHORTAGE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-STOP-FLAG = 'Y'
                   COMPUTE ESCM-SHORTAGE-YTD ROUNDED = ESCT-PAYMENT-AMT
                       * ESCM-DISCOUNT / 200
               WHEN ESCM-TYPE-CD = 'AKX'
                   MOVE 10.00 TO ESCM-SHORTAGE-YTD
               WHEN ESCM-TYPE-CD = 'REP'
                   MOVE 12.00 TO ESCM-SHORTAGE-YTD
               WHEN OTHER
                   COMPUTE ESCM-SHORTAGE-YTD ROUNDED = ESCT-PAYMENT-AMT
                       * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-SHORTAGE-YTD > 0 AND ESCM-SHORTAGE-YTD < 1.00
               MOVE 1.00 TO ESCM-SHORTAGE-YTD
           END-IF
           IF ESCM-SHORTAGE-YTD > 50.00
               MOVE 50.00 TO ESCM-SHORTAGE-YTD
           END-IF
           ADD ESCM-SHORTAGE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CUSHION AMT.
      *----------------------------------------------------------------*
       4000-PRORATE-CUSHION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-CUSHION-AMT ROUNDED =
                   ESCM-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-CUSHION-AMT
           END-IF
           ADD ESCM-CUSHION-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DISBURSE YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4010-LIMIT-DISBURSE.
           IF ESCM-DISBURSE-YTD < 25.00
               MOVE 25.00 TO ESCM-DISBURSE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-DISBURSE-YTD > 5000.00
               MOVE 5000.00 TO ESCM-DISBURSE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4020-ACCRUE-TAX.
           IF ESCM-TAX-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-TAX-AMT * ESCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4030-DESCRIBE-CHANNEL.
           EVALUATE ESCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4040-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-SCHED-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF WS-WORK-AMT NOT < WS-SHORTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-SHORTAGE-SCHED-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHORTAGE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4050-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCT-TRAN-CD
                   ADD ESCT-SHORTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCT-SHORTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E355 CONTACT REQUIRED
      * E465 INSURANCE YTD NEGATIVE
      * E350 INSURANCE YTD OVER 5000
      * E568 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4060-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CONTACT = SPACES
                   MOVE 'E355' TO WS-REJECT-CODE
               WHEN ESCM-INSURANCE-YTD < 0
                   MOVE 'E465' TO WS-REJECT-CODE
               WHEN ESCM-INSURANCE-YTD > 5000.00
                   MOVE 'E350' TO WS-REJECT-CODE
               WHEN ESCM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E568' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4070-AGE-EFF.
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
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-TYPE-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4090-CHECK-LAST-STMT.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO ESCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT SHORTAGE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4100-SPLIT-SHORTAGE.
           COMPUTE WS-WORK-AMT = ESCM-SHORTAGE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ESCM-SHORTAGE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE ESCM-INSURANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE CUSHION YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4110-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO ESCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ESCM-CUSHION-YTD NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO ESCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4120-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REASON-CD
                   ADD ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4130-AGE-LAST-PAY.
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
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-DISBURSE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE INSURANCE FEE ON THE CUSHION AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4140-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ESCM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ESCM-INSURANCE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   COMPUTE ESCM-INSURANCE-AMT ROUNDED = ESCT-CUSHION-AMT
                       * ESCM-DISCOUNT / 200
               WHEN ESCM-PAYEE-TYPE-CD = 'AM'
                   MOVE 12.00 TO ESCM-INSURANCE-AMT
               WHEN ESCM-PAYEE-TYPE-CD = 'UE'
                   MOVE 7.00 TO ESCM-INSURANCE-AMT
               WHEN OTHER
                   COMPUTE ESCM-INSURANCE-AMT ROUNDED = ESCT-CUSHION-AMT
                       * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-INSURANCE-AMT > 0 AND ESCM-INSURANCE-AMT < 1.00
               MOVE 1.00 TO ESCM-INSURANCE-AMT
           END-IF
           IF ESCM-INSURANCE-AMT > 50.00
               MOVE 50.00 TO ESCM-INSURANCE-AMT
           END-IF
           ADD ESCM-INSURANCE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-BAND-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT-2 NOT < WS-PAYMENT-BAND-LOW(WS-TX)
                   MOVE WS-PAYMENT-BAND-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INSURANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHORTAGE AMT.
      *----------------------------------------------------------------*
       4160-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCT-SHORTAGE-AMT ROUNDED =
                   ESCM-INSURANCE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCT-SHORTAGE-AMT
           END-IF
           ADD ESCT-SHORTAGE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURPLUS YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4170-ACCRUE-SURPLUS.
           IF ESCM-SURPLUS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SURPLUS-YTD * ESCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESCT-CUSHION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CUSHION YTD.
      *----------------------------------------------------------------*
       4180-PRORATE-CUSHION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-CUSHION-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-CUSHION-YTD
           END-IF
           ADD ESCM-CUSHION-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE AMT BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4190-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-PAYEE-TYPE-CD
                   ADD ESCM-INSURANCE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4200-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESCM-INSURANCE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-INSURANCE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE ESCM-TAX-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4210-LIMIT-PAYMENT.
           IF ESCM-PAYMENT-AMT < 50.00
               MOVE 50.00 TO ESCM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-PAYMENT-AMT > 10000.00
               MOVE 10000.00 TO ESCM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4220-AGE-POST.
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
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4230-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESCT-INSURANCE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ESCT-INSURANCE-AMT - (WS-WORK-AMT *
               2)
           COMPUTE ESCT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4240-COUNT-RETRY.
           IF ESCM-RETRY-CNT < 9999999
               ADD 1 TO ESCM-RETRY-CNT
           END-IF
           IF ESCM-RETRY-CNT NOT < 3
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE SHORTAGE SCHED TIER FOR THE INSURANCE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4250-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-SHORTAGE-SCHED-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ESCM-INSURANCE-AMT NOT < WS-SHORTAGE-SCHED-LOW(WS-TX)
                   MOVE WS-SHORTAGE-SCHED-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TAX AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4260-LIMIT-TAX.
           IF ESCM-TAX-AMT < 0.00
               MOVE 0.00 TO ESCM-TAX-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-TAX-AMT > 5000.00
               MOVE 5000.00 TO ESCM-TAX-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4270-FLAG-PAYMENT.
           IF ESCM-PAYMENT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INSURANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       4280-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-PAYMENT-AMT ROUNDED =
                   ESCM-INSURANCE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-PAYMENT-AMT
           END-IF
           ADD ESCM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SHORTAGE AMT IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4290-LIMIT-SHORTAGE.
           IF ESCM-SHORTAGE-AMT < 50.00
               MOVE 50.00 TO ESCM-SHORTAGE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SHORTAGE-AMT > 2500.00
               MOVE 2500.00 TO ESCM-SHORTAGE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4300-DESCRIBE-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'E'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'R'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'U'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Z'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E830 DESC REQUIRED
      * E611 SHORTAGE AMT NEGATIVE
      * E420 SHORTAGE AMT OVER 50000
      * E149 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4310-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-DESC = SPACES
                   MOVE 'E830' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-AMT < 0
                   MOVE 'E611' TO WS-REJECT-CODE
               WHEN ESCM-SHORTAGE-AMT > 50000.00
                   MOVE 'E420' TO WS-REJECT-CODE
               WHEN ESCM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E149' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX AMT BY ANALYSIS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4320-ACCUM-ANALYSIS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-ANALYSIS-CD
                   ADD ESCM-TAX-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-ANALYSIS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-TAX-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
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
                   MOVE ESVAL-CUSHION-YTD
                     TO ESCM-CUSHION-YTD
                   MOVE ESVAL-SURPLUS-YTD
                     TO ESCM-SURPLUS-YTD
                   MOVE ESVAL-SUSPEND-FLAG
                     TO ESCM-SUSPEND-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RVAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-ESVAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE ESC FIELDS TO ESRISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-ESRISK.
           INITIALIZE ESRISK-PARMS
           MOVE ESCM-DISBURSE-YTD
             TO ESRISK-DISBURSE-YTD
           MOVE ESCM-TAX-YTD
             TO ESRISK-TAX-YTD
           MOVE ESCM-SHORTAGE-AMT
             TO ESRISK-SHORTAGE-AMT
           MOVE ESCM-SHORTAGE-YTD
             TO ESRISK-SHORTAGE-YTD
           MOVE ESCM-INSURANCE-AMT
             TO ESRISK-INSURANCE-AMT
           MOVE ESCM-INSURANCE-YTD
             TO ESRISK-INSURANCE-YTD
           MOVE ESCM-LAST-PAY-DATE
             TO ESRISK-LAST-PAY-DATE
           MOVE ESCM-NEXT-REVIEW-DATE
             TO ESRISK-NEXT-REVIEW-DATE
           MOVE ESCM-NOTICE-DATE
             TO ESRISK-NOTICE-DATE
           MOVE ESCM-LAST-ACT-DATE
             TO ESRISK-LAST-ACT-DATE
           MOVE ESCM-REASON-CD
             TO ESRISK-REASON-CD
           MOVE ESCM-REGION-CD
             TO ESRISK-REGION-CD
           MOVE ESCM-TYPE-CD
             TO ESRISK-TYPE-CD
           MOVE ESCM-STATUS-CD
             TO ESRISK-STATUS-CD
           MOVE ESCM-HOLD-FLAG
             TO ESRISK-HOLD-FLAG
           MOVE ESCM-STOP-FLAG
             TO ESRISK-STOP-FLAG
           MOVE ESCM-PREFERRED-FLAG
             TO ESRISK-PREFERRED-FLAG
           MOVE ESCM-CYCLE-CNT
             TO ESRISK-CYCLE-CNT
           MOVE ESCM-EXCEPT-CNT
             TO ESRISK-EXCEPT-CNT
           MOVE ESCM-RATE
             TO ESRISK-RATE
           MOVE ESCM-FEE-PCT
             TO ESRISK-FEE-PCT
           MOVE ESCM-NAME
             TO ESRISK-NAME
           MOVE ESCM-DESC
             TO ESRISK-DESC
           CALL 'ESRISK' USING ESRISK-PARMS
           EVALUATE ESRISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE ESRISK-DISBURSE-YTD
                     TO ESCM-DISBURSE-YTD
                   MOVE ESRISK-TAX-YTD
                     TO ESCM-TAX-YTD
                   MOVE ESRISK-SHORTAGE-AMT
                     TO ESCM-SHORTAGE-AMT
                   MOVE ESRISK-INSURANCE-AMT
                     TO ESCM-INSURANCE-AMT
                   MOVE ESRISK-HOLD-FLAG
                     TO ESCM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-ESRISK' TO ABND-PARAGRAPH
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
           DISPLAY 'ESCSTMT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ESCSTMT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
