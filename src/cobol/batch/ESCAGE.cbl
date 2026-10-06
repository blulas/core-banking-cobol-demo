      *================================================================*
      * PROGRAM  : ESCAGE                                              *
      * TYPE     : BATCH                                               *
      * PURPOSE  : MORTGAGE ESCROW - AGING. READS THE ESC TRANSACTIONS,*
      *            UPDATES THE ESC MASTER AND PRODUCES THE CONTROL     *
      *            REPORT. ESCROW ANALYSIS, TAX AND INSURANCE          *
      *            DISBURSEMENT AND SHORTAGE                           *
      * INPUT    : ESCMST (KSDS), ESCTRN, BANKCTL                      *
      * OUTPUT   : ESCOUT, ESCAGRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, ESRISK                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-04-23 TWH  ORIGINAL                                       *
      * 1992-11-21 TWH  NEW TAX REPORT COLUMNS CHG5859                 *
      * 1994-06-28 EWS  TAX LIMIT REVISED INC4345                      *
      * 1994-02-22 GAF  DISBURSE ROUNDING - HALF UP PRB4272            *
      * 1999-12-16 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-06-07 TWH  DISBURSE CUTOFF MOVED CHG0760                  *
      * 2000-02-23 TWH  DISBURSE TABLE EXPANDED CHG1837                *
      * 2000-03-27 TWH  CUSHION STATUS CODES ADDED REQ2704             *
      * 2003-06-04 TWH  CUSHION ROUNDING - HALF UP PRB7250             *
      * 2003-07-20 DPS  PAYMENT TABLE EXPANDED REQ7980                 *
      * 2007-04-22 GAF  NEW CUSHION REPORT COLUMNS REQ5621             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESCAGE.
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
           SELECT REPORT-FILE ASSIGN TO "ESCAGRPT"
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
                                           VALUE 12.000000.
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
      * WS-DISBURSE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-DISBURSE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000040000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000477000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000879500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000521000'.
       01  WS-DISBURSE-SCHED-TBL REDEFINES WS-DISBURSE-SCHED-VALUES.
           05  WS-DISBURSE-SCHED-ROW OCCURS 4 TIMES.
               10  WS-DISBURSE-SCHED-LOW   PIC 9(09)V99.
               10  WS-DISBURSE-SCHED-RATE  PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-TAX-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TAX-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000483000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000050000000718000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000114000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003550000000344000'.
       01  WS-TAX-SCHED-TBL REDEFINES WS-TAX-SCHED-VALUES.
           05  WS-TAX-SCHED-ROW OCCURS 4 TIMES.
               10  WS-TAX-SCHED-LOW        PIC 9(09)V99.
               10  WS-TAX-SCHED-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-TAX-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-TAX-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000391000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002500000000590000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000591500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000168000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005300000000737000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010300000000741000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012800000000283000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017800000000800500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017900000000560500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018400000000156500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020900000000765000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021400000000876000'.
       01  WS-TAX-BAND-TBL REDEFINES WS-TAX-BAND-VALUES.
           05  WS-TAX-BAND-ROW OCCURS 12 TIMES.
               10  WS-TAX-BAND-LOW         PIC 9(09)V99.
               10  WS-TAX-BAND-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-INSURANC-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-INSURANC-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000811000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000635500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000593000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000491500'.
       01  WS-INSURANC-BAND-TBL REDEFINES WS-INSURANC-BAND-VALUES.
           05  WS-INSURANC-BAND-ROW OCCURS 4 TIMES.
               10  WS-INSURANC-BAND-LOW    PIC 9(09)V99.
               10  WS-INSURANC-BAND-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  ESCAGE  MORTGAGE ESCROW'.
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
               PERFORM 3000-SELECT-RATE
               PERFORM 3010-SELECT-RATE
               PERFORM 3020-AGE-NEXT-REVIEW
               PERFORM 3030-AGE-LAST-ACT
               PERFORM 3040-LIMIT-ITEM
               PERFORM 3050-CHECK-NOTICE
               PERFORM 3060-ASSESS-PREFERRED
               PERFORM 3070-SET-REASON
               PERFORM 3080-SELECT-DISCOUNT
               PERFORM 3090-PRORATE-SURPLUS
               PERFORM 3100-SET-PAYEE-TYPE
               PERFORM 3110-CHECK-LAST-STMT
               PERFORM 3120-CHECK-CLOSE
               PERFORM 3130-EDIT-ADDR
               PERFORM 3140-EDIT-REJECT
               PERFORM 3150-SPLIT-INSURANCE
               PERFORM 3160-ACCUM-REASON
               PERFORM 3170-ACCRUE-DISBURSE
               PERFORM 3180-ASSESS-HOLD
               PERFORM 3190-LIMIT-TAX
               PERFORM 3200-ACCUM-ANALYSIS
               PERFORM 3210-ACCRUE-INSURANCE
               PERFORM 3220-FLAG-ITEM
               PERFORM 3230-ACCRUE-CUSHION
               PERFORM 3240-CHECK-EFF
               PERFORM 3250-LIMIT-ITEM
               PERFORM 3260-ACCUM-STATUS
               PERFORM 3270-SET-CHANNEL
               PERFORM 3280-FLAG-TAX
               PERFORM 3290-DESCRIBE-ANALYSIS
               PERFORM 3300-CHECK-LAST-STMT
               PERFORM 3310-LIMIT-SURPLUS
               PERFORM 3320-FLAG-PAYMENT
               PERFORM 3330-PRORATE-TAX
               PERFORM 3340-LIMIT-SHORTAGE
               PERFORM 3350-SPLIT-CUSHION
               PERFORM 3360-ASSESS-STOP
               PERFORM 3370-CHECK-NEXT-REVIEW
               PERFORM 3380-SPLIT-TAX
               PERFORM 3390-ACCUM-TYPE
               PERFORM 3400-SPLIT-CUSHION
               PERFORM 3410-ASSESS-PREFERRED
               PERFORM 3420-DESCRIBE-REASON
               PERFORM 3430-SET-REASON
               PERFORM 3440-ACCRUE-TAX
               PERFORM 3450-ACCUM-TYPE
               PERFORM 3460-ACCRUE-CUSHION
               PERFORM 3470-EDIT-OPERATOR
               PERFORM 3480-EDIT-NAME
               PERFORM 3490-SET-TRAN
               PERFORM 3500-LIMIT-PAYMENT
               PERFORM 3510-ACCUM-PAYEE-TYPE
               PERFORM 3520-SELECT-FEE-PCT
               PERFORM 3530-CHECK-EFFECTIVE
               PERFORM 3540-ACCUM-TRAN
               PERFORM 3550-PRORATE-PAYMENT
               PERFORM 3560-ACCRUE-PAYMENT
               PERFORM 3570-SET-CHANNEL
               PERFORM 3580-FLAG-DISBURSE
               PERFORM 3590-AGE-EFFECTIVE
               PERFORM 3600-PRORATE-SHORTAGE
               PERFORM 3610-LIMIT-SHORTAGE
               PERFORM 3620-SPLIT-TAX
               PERFORM 3630-EDIT-ADDR
               PERFORM 3640-COUNT-CYCLE
               PERFORM 3650-EDIT-NAME
               PERFORM 3660-SET-TRAN
               PERFORM 3670-COUNT-EXCEPT
               PERFORM 3680-COUNT-NOTICE
               PERFORM 3690-SELECT-FEE-PCT
               PERFORM 3700-SPLIT-PAYMENT
               PERFORM 3710-ASSESS-PREFERRED
               PERFORM 3720-ACCUM-TYPE
               PERFORM 3730-FLAG-TAX
               PERFORM 3740-FLAG-PAYMENT
               PERFORM 3750-FLAG-DISBURSE
               PERFORM 3760-ACCUM-TYPE
               PERFORM 3770-LIMIT-PAYMENT
               PERFORM 3780-AGE-LAST-PAY
               PERFORM 3790-ACCRUE-SURPLUS
               PERFORM 3800-EDIT-OPERATOR
               PERFORM 3810-PRORATE-CUSHION
               PERFORM 3820-PRORATE-PAYMENT
               PERFORM 3830-EDIT-OPERATOR
               PERFORM 3840-LIMIT-INSURANCE
               PERFORM 3850-DESCRIBE-STATUS
               PERFORM 3860-COUNT-CYCLE
               PERFORM 3870-SPLIT-CUSHION
               PERFORM 3880-SET-REGION
               PERFORM 3890-SELECT-FEE-PCT
               PERFORM 3900-FLAG-DISBURSE
               PERFORM 3910-EDIT-OPERATOR
               PERFORM 3920-COUNT-EXCEPT
               PERFORM 3930-CHECK-LAST-PAY
               PERFORM 3940-SELECT-DISCOUNT
               PERFORM 3950-LIMIT-PAYMENT
               PERFORM 3960-ACCUM-CHANNEL
               PERFORM 3970-ACCUM-TYPE
               PERFORM 3980-AGE-EFFECTIVE
               PERFORM 3990-DESCRIBE-TRAN
               PERFORM 4000-ACCRUE-PAYMENT
               PERFORM 4010-DESCRIBE-REASON
               PERFORM 4020-ACCRUE-TAX
               PERFORM 4030-CHECK-LAST-PAY
               PERFORM 4040-DESCRIBE-REASON
               PERFORM 4050-DESCRIBE-STATUS
               PERFORM 4060-EDIT-CUST-ID
               PERFORM 4070-SELECT-FEE-PCT
               PERFORM 4080-ACCUM-REASON
               PERFORM 4090-COUNT-NOTICE
               PERFORM 4100-PRORATE-CUSHION
               PERFORM 4110-ACCUM-ANALYSIS
               PERFORM 4120-DESCRIBE-CHANNEL
               PERFORM 4130-SET-REGION
               PERFORM 4140-SPLIT-DISBURSE
               PERFORM 4150-COUNT-CYCLE
               PERFORM 4160-AGE-NOTICE
               PERFORM 4170-AGE-POST
               PERFORM 4180-PRORATE-TAX
               PERFORM 4190-LIMIT-INSURANCE
               PERFORM 4200-ACCUM-TRAN
               PERFORM 4210-PRORATE-CUSHION
               PERFORM 4220-ASSESS-STOP
               PERFORM 4230-ACCUM-CHANNEL
               PERFORM 4240-PRORATE-CUSHION
               PERFORM 4250-SPLIT-INSURANCE
               PERFORM 4260-EDIT-NAME
               PERFORM 4270-LIMIT-DISBURSE
               PERFORM 4280-SELECT-FEE-PCT
               PERFORM 4290-ACCUM-REGION
               PERFORM 4300-PRORATE-DISBURSE
               PERFORM 4310-SET-PAYEE-TYPE
               PERFORM 4320-AGE-EFFECTIVE
               PERFORM 4330-SET-ANALYSIS
               PERFORM 4340-ACCRUE-PAYMENT
               PERFORM 4350-PRORATE-SURPLUS
               PERFORM 4360-SPLIT-PAYMENT
               PERFORM 4370-COUNT-NOTICE
               PERFORM 4380-DESCRIBE-REGION
               PERFORM 4390-DESCRIBE-TRAN
               PERFORM 4400-SET-PAYEE-TYPE
               PERFORM 4410-ACCRUE-CUSHION
               PERFORM 4420-ACCUM-REGION
               PERFORM 4430-FLAG-SHORTAGE
               PERFORM 4440-AGE-NOTICE
               PERFORM 4450-ASSESS-REVERSAL
               PERFORM 4460-ACCUM-PAYEE-TYPE
               PERFORM 4470-ACCRUE-TAX
               PERFORM 4480-SELECT-DISCOUNT
               PERFORM 4490-EDIT-CONTACT
               PERFORM 4500-SET-REASON
               PERFORM 4510-ACCRUE-SHORTAGE
               PERFORM 4520-SET-ANALYSIS
               PERFORM 4530-ACCRUE-PAYMENT
               PERFORM 4540-FLAG-SURPLUS
               PERFORM 4550-SELECT-DISCOUNT
               PERFORM 4560-SET-PAYEE-TYPE
               PERFORM 4570-PRORATE-DISBURSE
               PERFORM 4580-AGE-CLOSE
               PERFORM 4590-FLAG-SHORTAGE
               PERFORM 4600-ACCUM-PAYEE-TYPE
               PERFORM 4610-PRORATE-PAYMENT
               PERFORM 4620-ASSESS-HOLD
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
      * SELECT THE TAX BAND TIER FOR THE SHORTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3000-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-BAND-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ESCT-SHORTAGE-AMT NOT < WS-TAX-BAND-LOW(WS-TX)
                   MOVE WS-TAX-BAND-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE SCHED TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3010-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-SCHED-RATE(1) TO ESCM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ESCM-PAYMENT-AMT NOT < WS-DISBURSE-SCHED-LOW(WS-TX)
                   MOVE WS-DISBURSE-SCHED-RATE(WS-TX) TO ESCM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
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
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3030-AGE-LAST-ACT.
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
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCT-INSURANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3040-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3050-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ESCM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CUSHION FEE ON THE SHORTAGE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3060-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ESCM-CUSHION-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-STOP-FLAG = 'Y'
                   COMPUTE ESCM-CUSHION-AMT ROUNDED = ESCT-SHORTAGE-AMT
                       * ESCM-RATE / 200
               WHEN ESCM-STATUS-CD = 'A'
                   MOVE 7.00 TO ESCM-CUSHION-AMT
               WHEN ESCM-STATUS-CD = 'E'
                   MOVE 12.00 TO ESCM-CUSHION-AMT
               WHEN OTHER
                   COMPUTE ESCM-CUSHION-AMT ROUNDED = ESCT-SHORTAGE-AMT
                       * ESCM-RATE / 100
           END-EVALUATE
           IF ESCM-CUSHION-AMT > 0 AND ESCM-CUSHION-AMT < 2.00
               MOVE 2.00 TO ESCM-CUSHION-AMT
           END-IF
           IF ESCM-CUSHION-AMT > 100.00
               MOVE 100.00 TO ESCM-CUSHION-AMT
           END-IF
           ADD ESCM-CUSHION-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * C -> F WHEN NEXT DUE DATE IS PAST
      * F -> K WHEN NEXT DUE DATE IS PAST
      * K -> L WHEN NEXT DUE DATE IS PAST
      * L -> S WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'L' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE INSURANC BAND TIER FOR THE DISBURSE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3080-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-INSURANC-BAND-RATE(1) TO ESCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ESCM-DISBURSE-YTD NOT < WS-INSURANC-BAND-LOW(WS-TX)
                   MOVE WS-INSURANC-BAND-RATE(WS-TX) TO ESCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO SURPLUS YTD.
      *----------------------------------------------------------------*
       3090-PRORATE-SURPLUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SURPLUS-YTD ROUNDED =
                   ESCT-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-SURPLUS-YTD
           END-IF
           ADD ESCM-SURPLUS-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * AM -> UE WHEN LAST ACT DATE IS PAST
      * UE -> UP WHEN LAST ACT DATE IS PAST
      * UP -> AM WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3100-SET-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   IF ESCM-LAST-ACT-DATE > 0 AND ESCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UE' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UE'
                   IF ESCM-LAST-ACT-DATE > 0 AND ESCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UP' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UP'
                   IF ESCM-LAST-ACT-DATE > 0 AND ESCM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AM' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3110-CHECK-LAST-STMT.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3120-CHECK-CLOSE.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO ESCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E874 ADDR REQUIRED
      * E194 DISBURSE YTD NEGATIVE
      * E562 DISBURSE YTD OVER 250000
      * E963 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3130-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-ADDR = SPACES
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-YTD < 0
                   MOVE 'E194' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-YTD > 250000.00
                   MOVE 'E562' TO WS-REJECT-CODE
               WHEN ESCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E963' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E294 REJECT CD REQUIRED
      * E725 WORK AMT 2 NEGATIVE
      * E271 WORK AMT 2 OVER 1000000
      * E808 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-REJECT-CD = SPACES
                   MOVE 'E294' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E725' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 1000000.00
                   MOVE 'E271' TO WS-REJECT-CODE
               WHEN ESCM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E808' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3150-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESCM-INSURANCE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-INSURANCE-AMT - (WS-WORK-AMT *
               3)
           COMPUTE ESCT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-REASON.
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
      * DAILY INTEREST ON DISBURSE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3170-ACCRUE-DISBURSE.
           IF ESCM-DISBURSE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-DISBURSE-YTD * ESCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ESCT-INSURANCE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE SHORTAGE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3180-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ESCM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ESCT-SHORTAGE-AMT *
                       ESCM-DISCOUNT / 200
               WHEN ESCM-REASON-CD = 'C'
                   MOVE 20.00 TO WS-WORK-AMT-2
               WHEN ESCM-REASON-CD = 'F'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ESCT-SHORTAGE-AMT *
                       ESCM-DISCOUNT / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 5.00
               MOVE 5.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TAX YTD IS FLOORED AT $25.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3190-LIMIT-TAX.
           IF ESCM-TAX-YTD < 25.00
               MOVE 25.00 TO ESCM-TAX-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-TAX-YTD > 1000.00
               MOVE 1000.00 TO ESCM-TAX-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE YTD BY ANALYSIS CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-ANALYSIS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-ANALYSIS-CD
                   ADD ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-ANALYSIS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON INSURANCE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3210-ACCRUE-INSURANCE.
           IF ESCM-INSURANCE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-INSURANCE-YTD * ESCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ESCM-SURPLUS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3220-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CUSHION YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3230-ACCRUE-CUSHION.
           IF ESCM-CUSHION-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-CUSHION-YTD * ESCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-SURPLUS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-EFF.
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
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3250-LIMIT-ITEM.
           IF WS-WORK-AMT < 25.00
               MOVE 25.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3260-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-STATUS-CD
                   ADD ESCM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3270-SET-CHANNEL.
           EVALUATE ESCT-CHANNEL-CD
               WHEN 'BRN'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TAX AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3280-FLAG-TAX.
           IF ESCM-TAX-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCM-TAX-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE ANALYSIS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3290-DESCRIBE-ANALYSIS.
           EVALUATE ESCM-ANALYSIS-CD
               WHEN 'GD'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LY'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'XJ'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3300-CHECK-LAST-STMT.
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
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS AMT IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3310-LIMIT-SURPLUS.
           IF ESCM-SURPLUS-AMT < 50.00
               MOVE 50.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SURPLUS-AMT > 50000.00
               MOVE 50000.00 TO ESCM-SURPLUS-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3320-FLAG-PAYMENT.
           IF ESCM-PAYMENT-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAX AMT.
      *----------------------------------------------------------------*
       3330-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-TAX-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-TAX-AMT
           END-IF
           ADD ESCM-TAX-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SHORTAGE YTD IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3340-LIMIT-SHORTAGE.
           IF ESCM-SHORTAGE-YTD < 100.00
               MOVE 100.00 TO ESCM-SHORTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SHORTAGE-YTD > 10000.00
               MOVE 10000.00 TO ESCM-SHORTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CUSHION AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3350-SPLIT-CUSHION.
           COMPUTE WS-WORK-AMT = ESCM-CUSHION-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-CUSHION-AMT - (WS-WORK-AMT * 3)
           COMPUTE ESCT-SHORTAGE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURPLUS FEE ON THE CUSHION AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3360-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SURPLUS-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   COMPUTE ESCM-SURPLUS-AMT ROUNDED = ESCT-CUSHION-AMT *
                       ESCM-DISCOUNT / 200
               WHEN ESCT-TRAN-CD = 'DS'
                   MOVE 12.00 TO ESCM-SURPLUS-AMT
               WHEN ESCT-TRAN-CD = 'PY'
                   MOVE 12.00 TO ESCM-SURPLUS-AMT
               WHEN OTHER
                   COMPUTE ESCM-SURPLUS-AMT ROUNDED = ESCT-CUSHION-AMT *
                       ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-SURPLUS-AMT > 0 AND ESCM-SURPLUS-AMT < 5.00
               MOVE 5.00 TO ESCM-SURPLUS-AMT
           END-IF
           IF ESCM-SURPLUS-AMT > 150.00
               MOVE 150.00 TO ESCM-SURPLUS-AMT
           END-IF
           ADD ESCM-SURPLUS-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3370-CHECK-NEXT-REVIEW.
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
               MOVE 'Y' TO ESCT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAX YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3380-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = ESCM-TAX-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ESCM-TAX-YTD - (WS-WORK-AMT * 2)
           COMPUTE ESCM-DISBURSE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE TAX YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3390-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-TYPE-CD
                   ADD ESCM-TAX-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-TAX-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CUSHION YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3400-SPLIT-CUSHION.
           COMPUTE WS-WORK-AMT = ESCM-CUSHION-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-CUSHION-YTD - (WS-WORK-AMT * 3)
           COMPUTE ESCM-TAX-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE INSURANCE YTD FEE ON THE SHORTAGE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3410-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ESCM-INSURANCE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-SUSPEND-FLAG = 'Y'
                   COMPUTE ESCM-INSURANCE-YTD ROUNDED =
                       ESCM-SHORTAGE-AMT * ESCM-FEE-PCT / 200
               WHEN ESCM-TYPE-CD = 'AKX'
                   MOVE 15.00 TO ESCM-INSURANCE-YTD
               WHEN ESCM-TYPE-CD = 'REP'
                   MOVE 10.00 TO ESCM-INSURANCE-YTD
               WHEN OTHER
                   COMPUTE ESCM-INSURANCE-YTD ROUNDED =
                       ESCM-SHORTAGE-AMT * ESCM-FEE-PCT / 100
           END-EVALUATE
           IF ESCM-INSURANCE-YTD > 0 AND ESCM-INSURANCE-YTD < 2.00
               MOVE 2.00 TO ESCM-INSURANCE-YTD
           END-IF
           IF ESCM-INSURANCE-YTD > 100.00
               MOVE 100.00 TO ESCM-INSURANCE-YTD
           END-IF
           ADD ESCM-INSURANCE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3420-DESCRIBE-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'F'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'K'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'L'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'X'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * C -> F WHEN NEXT REVIEW DATE IS PAST
      * F -> K WHEN NEXT REVIEW DATE IS PAST
      * K -> L WHEN NEXT REVIEW DATE IS PAST
      * L -> S WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3430-SET-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'F' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'K' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'L' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'S' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-TAX.
           IF ESCM-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-TAX-YTD * ESCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ESCT-CUSHION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CUSHION AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-TYPE-CD
                   ADD ESCM-CUSHION-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-CUSHION-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CUSHION AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3460-ACCRUE-CUSHION.
           IF ESCT-CUSHION-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCT-CUSHION-AMT * ESCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E564 OPERATOR REQUIRED
      * E143 INSURANCE AMT NEGATIVE
      * E709 INSURANCE AMT OVER 1000000
      * E068 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3470-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-OPERATOR = SPACES
                   MOVE 'E564' TO WS-REJECT-CODE
               WHEN ESCM-INSURANCE-AMT < 0
                   MOVE 'E143' TO WS-REJECT-CODE
               WHEN ESCM-INSURANCE-AMT > 1000000.00
                   MOVE 'E709' TO WS-REJECT-CODE
               WHEN ESCM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E068' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E918 NAME REQUIRED
      * E608 TAX AMT NEGATIVE
      * E574 TAX AMT OVER 99999
      * E533 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3480-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-NAME = SPACES
                   MOVE 'E918' TO WS-REJECT-CODE
               WHEN ESCM-TAX-AMT < 0
                   MOVE 'E608' TO WS-REJECT-CODE
               WHEN ESCM-TAX-AMT > 99999.00
                   MOVE 'E574' TO WS-REJECT-CODE
               WHEN ESCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E533' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DS -> PY WHEN NOTICE DATE IS PAST
      * PY -> AN WHEN NOTICE DATE IS PAST
      * AN -> RF WHEN NOTICE DATE IS PAST
      * RF -> SH WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3490-SET-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AN' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AN'
                   IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RF'
                   IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3500-LIMIT-PAYMENT.
           IF ESCM-PAYMENT-AMT < 50.00
               MOVE 50.00 TO ESCM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-PAYMENT-AMT > 5000.00
               MOVE 5000.00 TO ESCM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHORTAGE AMT BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3510-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-PAYEE-TYPE-CD
                   ADD ESCT-SHORTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCT-SHORTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAX BAND TIER FOR THE SHORTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3520-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-BAND-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ESCM-SHORTAGE-AMT NOT < WS-TAX-BAND-LOW(WS-TX)
                   MOVE WS-TAX-BAND-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3530-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO ESCM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CUSHION AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3540-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCT-TRAN-CD
                   ADD ESCM-CUSHION-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-CUSHION-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURPLUS AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3550-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCT-PAYMENT-AMT ROUNDED =
                   ESCM-SURPLUS-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCT-PAYMENT-AMT
           END-IF
           ADD ESCT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3560-ACCRUE-PAYMENT.
           IF ESCT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCT-PAYMENT-AMT * ESCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-CUSHION-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST STMT DATE IS PAST
      * BAT -> MAL WHEN LAST STMT DATE IS PAST
      * MAL -> TEL WHEN LAST STMT DATE IS PAST
      * TEL -> ONL WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-CHANNEL.
           EVALUATE ESCT-CHANNEL-CD
               WHEN 'BRN'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ESCT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DISBURSE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3580-FLAG-DISBURSE.
           IF ESCM-DISBURSE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCM-DISBURSE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-EFFECTIVE.
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
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE TAX YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SHORTAGE YTD.
      *----------------------------------------------------------------*
       3600-PRORATE-SHORTAGE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SHORTAGE-YTD ROUNDED =
                   ESCM-TAX-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-SHORTAGE-YTD
           END-IF
           ADD ESCM-SHORTAGE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SHORTAGE YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3610-LIMIT-SHORTAGE.
           IF ESCM-SHORTAGE-YTD < 10.00
               MOVE 10.00 TO ESCM-SHORTAGE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-SHORTAGE-YTD > 10000.00
               MOVE 10000.00 TO ESCM-SHORTAGE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT TAX YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3620-SPLIT-TAX.
           COMPUTE WS-WORK-AMT = ESCM-TAX-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-TAX-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E520 ADDR REQUIRED
      * E019 SURPLUS YTD NEGATIVE
      * E896 SURPLUS YTD OVER 250000
      * E897 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3630-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-ADDR = SPACES
                   MOVE 'E520' TO WS-REJECT-CODE
               WHEN ESCM-SURPLUS-YTD < 0
                   MOVE 'E019' TO WS-REJECT-CODE
               WHEN ESCM-SURPLUS-YTD > 250000.00
                   MOVE 'E896' TO WS-REJECT-CODE
               WHEN ESCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E897' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3640-COUNT-CYCLE.
           IF ESCM-CYCLE-CNT < 9999999
               ADD 1 TO ESCM-CYCLE-CNT
           END-IF
           IF ESCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO ESCM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E550 NAME REQUIRED
      * E493 TAX YTD NEGATIVE
      * E793 TAX YTD OVER 250000
      * E424 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3650-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-NAME = SPACES
                   MOVE 'E550' TO WS-REJECT-CODE
               WHEN ESCM-TAX-YTD < 0
                   MOVE 'E493' TO WS-REJECT-CODE
               WHEN ESCM-TAX-YTD > 250000.00
                   MOVE 'E793' TO WS-REJECT-CODE
               WHEN ESCM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E424' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * DS -> PY WHEN CLOSE DATE IS PAST
      * PY -> AN WHEN CLOSE DATE IS PAST
      * AN -> RF WHEN CLOSE DATE IS PAST
      * RF -> SH WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3660-SET-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AN' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'AN'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RF'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'SH' TO ESCT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3670-COUNT-EXCEPT.
           IF ESCM-EXCEPT-CNT < 9999999
               ADD 1 TO ESCM-EXCEPT-CNT
           END-IF
           IF ESCM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO ESCM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3680-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO ESCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE SCHED TIER FOR THE CUSHION YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3690-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-SCHED-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ESCM-CUSHION-YTD NOT < WS-DISBURSE-SCHED-LOW(WS-TX)
                   MOVE WS-DISBURSE-SCHED-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3700-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = ESCM-PAYMENT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ESCM-PAYMENT-YTD - (WS-WORK-AMT * 3)
           COMPUTE ESCM-SHORTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE SURPLUS YTD FEE ON THE SHORTAGE YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3710-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ESCM-SURPLUS-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-HOLD-FLAG = 'Y'
                   COMPUTE ESCM-SURPLUS-YTD ROUNDED = ESCM-SHORTAGE-YTD
                       * ESCM-DISCOUNT / 200
               WHEN ESCT-CHANNEL-CD = 'BRN'
                   MOVE 10.00 TO ESCM-SURPLUS-YTD
               WHEN ESCT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO ESCM-SURPLUS-YTD
               WHEN OTHER
                   COMPUTE ESCM-SURPLUS-YTD ROUNDED = ESCM-SHORTAGE-YTD
                       * ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-SURPLUS-YTD > 0 AND ESCM-SURPLUS-YTD < 2.00
               MOVE 2.00 TO ESCM-SURPLUS-YTD
           END-IF
           IF ESCM-SURPLUS-YTD > 250.00
               MOVE 250.00 TO ESCM-SURPLUS-YTD
           END-IF
           ADD ESCM-SURPLUS-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3720-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-TYPE-CD
                   ADD ESCM-DISBURSE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-DISBURSE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TAX YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3730-FLAG-TAX.
           IF ESCM-TAX-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCM-TAX-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3740-FLAG-PAYMENT.
           IF ESCT-PAYMENT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ESCT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3750-FLAG-DISBURSE.
           IF ESCM-DISBURSE-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCM-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3760-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-TYPE-CD
                   ADD ESCM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $10.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3770-LIMIT-PAYMENT.
           IF ESCM-PAYMENT-AMT < 10.00
               MOVE 10.00 TO ESCM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-PAYMENT-AMT > 2500.00
               MOVE 2500.00 TO ESCM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3780-AGE-LAST-PAY.
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
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-INSURANCE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SURPLUS YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3790-ACCRUE-SURPLUS.
           IF ESCM-SURPLUS-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-SURPLUS-YTD * ESCM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO ESCT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E924 OPERATOR REQUIRED
      * E089 CUSHION AMT NEGATIVE
      * E601 CUSHION AMT OVER 1000000
      * E200 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3800-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-OPERATOR = SPACES
                   MOVE 'E924' TO WS-REJECT-CODE
               WHEN ESCM-CUSHION-AMT < 0
                   MOVE 'E089' TO WS-REJECT-CODE
               WHEN ESCM-CUSHION-AMT > 1000000.00
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN ESCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E200' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CUSHION AMT.
      *----------------------------------------------------------------*
       3810-PRORATE-CUSHION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-CUSHION-AMT ROUNDED =
                   ESCM-DISBURSE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-CUSHION-AMT
           END-IF
           ADD ESCM-CUSHION-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE SHORTAGE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3820-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCT-PAYMENT-AMT ROUNDED =
                   ESCM-SHORTAGE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCT-PAYMENT-AMT
           END-IF
           ADD ESCT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E579 OPERATOR REQUIRED
      * E323 PAYMENT YTD NEGATIVE
      * E114 PAYMENT YTD OVER 1000000
      * E809 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3830-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-OPERATOR = SPACES
                   MOVE 'E579' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-YTD < 0
                   MOVE 'E323' TO WS-REJECT-CODE
               WHEN ESCM-PAYMENT-YTD > 1000000.00
                   MOVE 'E114' TO WS-REJECT-CODE
               WHEN ESCM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E809' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * INSURANCE YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3840-LIMIT-INSURANCE.
           IF ESCM-INSURANCE-YTD < 100.00
               MOVE 100.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-INSURANCE-YTD > 2500.00
               MOVE 2500.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3850-DESCRIBE-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'E'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'R'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'U'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3860-COUNT-CYCLE.
           IF ESCM-CYCLE-CNT < 9999999
               ADD 1 TO ESCM-CYCLE-CNT
           END-IF
           IF ESCM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3870-SPLIT-CUSHION.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 4)
           COMPUTE ESCT-CUSHION-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * B -> G WHEN EFF DATE IS PAST
      * G -> Y WHEN EFF DATE IS PAST
      * Y -> B WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3880-SET-REGION.
           EVALUATE ESCM-REGION-CD
               WHEN 'B'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ESCM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO ESCM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO ESCM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE TAX BAND TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3890-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-BAND-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ESCT-PAYMENT-AMT NOT < WS-TAX-BAND-LOW(WS-TX)
                   MOVE WS-TAX-BAND-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3900-FLAG-DISBURSE.
           IF ESCM-DISBURSE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCM-DISBURSE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E027 OPERATOR REQUIRED
      * E426 DISBURSE AMT NEGATIVE
      * E394 DISBURSE AMT OVER 25000
      * E220 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3910-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCT-OPERATOR = SPACES
                   MOVE 'E027' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-AMT < 0
                   MOVE 'E426' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-AMT > 25000.00
                   MOVE 'E394' TO WS-REJECT-CODE
               WHEN ESCM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E220' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3920-COUNT-EXCEPT.
           IF ESCM-EXCEPT-CNT < 9999999
               ADD 1 TO ESCM-EXCEPT-CNT
           END-IF
           IF ESCM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO ESCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 60 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3930-CHECK-LAST-PAY.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO ESCM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE SCHED TIER FOR THE TAX YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-SCHED-RATE(1) TO ESCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ESCM-TAX-YTD NOT < WS-DISBURSE-SCHED-LOW(WS-TX)
                   MOVE WS-DISBURSE-SCHED-RATE(WS-TX) TO ESCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3950-LIMIT-PAYMENT.
           IF ESCM-PAYMENT-YTD < 0.00
               MOVE 0.00 TO ESCM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-PAYMENT-YTD > 1000.00
               MOVE 1000.00 TO ESCM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHORTAGE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3960-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCT-CHANNEL-CD
                   ADD ESCT-SHORTAGE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCT-SHORTAGE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE SHORTAGE YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3970-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-TYPE-CD
                   ADD ESCM-SHORTAGE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-SHORTAGE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3980-AGE-EFFECTIVE.
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
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-TAX-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'AN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'SH'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4000-ACCRUE-PAYMENT.
           IF ESCM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-PAYMENT-YTD * ESCM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-SURPLUS-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4010-DESCRIBE-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'F'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'K'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'L'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'S'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'X'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4020-ACCRUE-TAX.
           IF ESCM-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-TAX-YTD * ESCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-CUSHION-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4030-CHECK-LAST-PAY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO ESCM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4040-DESCRIBE-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'F'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'K'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'L'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'S'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'X'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4050-DESCRIBE-STATUS.
           EVALUATE ESCM-STATUS-CD
               WHEN 'A'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'E'
                   MOVE 'MATURED' TO WS-DESC
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
      * E276 CUST ID REQUIRED
      * E357 WORK AMT NEGATIVE
      * E713 WORK AMT OVER 10000
      * E874 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4060-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CUST-ID = SPACES
                   MOVE 'E276' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E357' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN ESCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAX SCHED TIER FOR THE SHORTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-SCHED-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ESCT-SHORTAGE-AMT NOT < WS-TAX-SCHED-LOW(WS-TX)
                   MOVE WS-TAX-SCHED-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REASON-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4090-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 12
               MOVE 'Y' TO ESCM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CUSHION AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CUSHION YTD.
      *----------------------------------------------------------------*
       4100-PRORATE-CUSHION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-CUSHION-YTD ROUNDED =
                   ESCT-CUSHION-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-CUSHION-YTD
           END-IF
           ADD ESCM-CUSHION-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY ANALYSIS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4110-ACCUM-ANALYSIS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-ANALYSIS-CD
                   ADD ESCM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-ANALYSIS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4120-DESCRIBE-CHANNEL.
           EVALUATE ESCT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * B -> G WHEN LAST STMT DATE IS PAST
      * G -> Y WHEN LAST STMT DATE IS PAST
      * Y -> B WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4130-SET-REGION.
           EVALUATE ESCM-REGION-CD
               WHEN 'B'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'G' TO ESCM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'G'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Y' TO ESCM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Y'
                   IF ESCM-LAST-STMT-DATE > 0 AND ESCM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO ESCM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT DISBURSE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4140-SPLIT-DISBURSE.
           COMPUTE WS-WORK-AMT = ESCM-DISBURSE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ESCM-DISBURSE-AMT - (WS-WORK-AMT * 2)
           COMPUTE ESCM-SHORTAGE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       4150-COUNT-CYCLE.
           IF ESCM-CYCLE-CNT < 9999999
               ADD 1 TO ESCM-CYCLE-CNT
           END-IF
           IF ESCM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO ESCM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4160-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NOTICE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-POST.
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
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SHORTAGE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO TAX YTD.
      *----------------------------------------------------------------*
       4180-PRORATE-TAX.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-TAX-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-TAX-YTD
           END-IF
           ADD ESCM-TAX-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * INSURANCE YTD IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4190-LIMIT-INSURANCE.
           IF ESCM-INSURANCE-YTD < 10.00
               MOVE 10.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-INSURANCE-YTD > 5000.00
               MOVE 5000.00 TO ESCM-INSURANCE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4200-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCT-TRAN-CD
                   ADD ESCM-DISBURSE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-DISBURSE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURPLUS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CUSHION AMT.
      *----------------------------------------------------------------*
       4210-PRORATE-CUSHION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCT-CUSHION-AMT ROUNDED =
                   ESCM-SURPLUS-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCT-CUSHION-AMT
           END-IF
           ADD ESCT-CUSHION-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4220-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ESCM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ESCM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   COMPUTE ESCM-PAYMENT-YTD ROUNDED = WS-WORK-AMT-2 *
                       ESCM-DISCOUNT / 200
               WHEN ESCM-ANALYSIS-CD = 'GD'
                   MOVE 20.00 TO ESCM-PAYMENT-YTD
               WHEN ESCM-ANALYSIS-CD = 'LY'
                   MOVE 10.00 TO ESCM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE ESCM-PAYMENT-YTD ROUNDED = WS-WORK-AMT-2 *
                       ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-PAYMENT-YTD > 0 AND ESCM-PAYMENT-YTD < 2.00
               MOVE 2.00 TO ESCM-PAYMENT-YTD
           END-IF
           IF ESCM-PAYMENT-YTD > 25.00
               MOVE 25.00 TO ESCM-PAYMENT-YTD
           END-IF
           ADD ESCM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4230-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCT-CHANNEL-CD
                   ADD ESCM-DISBURSE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-DISBURSE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE SURPLUS YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CUSHION YTD.
      *----------------------------------------------------------------*
       4240-PRORATE-CUSHION.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-CUSHION-YTD ROUNDED =
                   ESCM-SURPLUS-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-CUSHION-YTD
           END-IF
           ADD ESCM-CUSHION-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT INSURANCE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4250-SPLIT-INSURANCE.
           COMPUTE WS-WORK-AMT = ESCT-INSURANCE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ESCT-INSURANCE-AMT - (WS-WORK-AMT *
               12)
           COMPUTE ESCT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E726 NAME REQUIRED
      * E369 CUSHION YTD NEGATIVE
      * E356 CUSHION YTD OVER 50000
      * E212 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4260-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-NAME = SPACES
                   MOVE 'E726' TO WS-REJECT-CODE
               WHEN ESCM-CUSHION-YTD < 0
                   MOVE 'E369' TO WS-REJECT-CODE
               WHEN ESCM-CUSHION-YTD > 50000.00
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN ESCM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E212' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DISBURSE AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4270-LIMIT-DISBURSE.
           IF ESCM-DISBURSE-AMT < 25.00
               MOVE 25.00 TO ESCM-DISBURSE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ESCM-DISBURSE-AMT > 50000.00
               MOVE 50000.00 TO ESCM-DISBURSE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE DISBURSE SCHED TIER FOR THE SHORTAGE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4280-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-DISBURSE-SCHED-RATE(1) TO ESCM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ESCT-SHORTAGE-AMT NOT < WS-DISBURSE-SCHED-LOW(WS-TX)
                   MOVE WS-DISBURSE-SCHED-RATE(WS-TX) TO ESCM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4290-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REGION-CD
                   ADD ESCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE INSURANCE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DISBURSE YTD.
      *----------------------------------------------------------------*
       4300-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-DISBURSE-YTD ROUNDED =
                   ESCT-INSURANCE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-DISBURSE-YTD
           END-IF
           ADD ESCM-DISBURSE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * AM -> UE WHEN NOTICE DATE IS PAST
      * UE -> UP WHEN NOTICE DATE IS PAST
      * UP -> AM WHEN NOTICE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4310-SET-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UE' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UE'
                   IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UP' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UP'
                   IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AM' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4320-AGE-EFFECTIVE.
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
               WHEN WS-DAYS NOT > 45
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ANALYSIS CD:
      * GD -> LY WHEN NEXT DUE DATE IS PAST
      * LY -> XJ WHEN NEXT DUE DATE IS PAST
      * XJ -> GD WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4330-SET-ANALYSIS.
           EVALUATE ESCM-ANALYSIS-CD
               WHEN 'GD'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LY' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LY'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XJ'
                   IF ESCM-NEXT-DUE-DATE > 0 AND ESCM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GD' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4340-ACCRUE-PAYMENT.
           IF ESCT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCT-PAYMENT-AMT * ESCM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ESCT-SHORTAGE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CUSHION YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO SURPLUS AMT.
      *----------------------------------------------------------------*
       4350-PRORATE-SURPLUS.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-SURPLUS-AMT ROUNDED =
                   ESCM-CUSHION-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-SURPLUS-AMT
           END-IF
           ADD ESCM-SURPLUS-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4360-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = ESCM-PAYMENT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ESCM-PAYMENT-YTD - (WS-WORK-AMT * 12)
           COMPUTE ESCM-INSURANCE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * NOTICE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4370-COUNT-NOTICE.
           IF ESCM-NOTICE-CNT < 9999999
               ADD 1 TO ESCM-NOTICE-CNT
           END-IF
           IF ESCM-NOTICE-CNT NOT < 5
               MOVE 'Y' TO ESCM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4380-DESCRIBE-REGION.
           EVALUATE ESCM-REGION-CD
               WHEN 'B'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'G'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'Y'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4390-DESCRIBE-TRAN.
           EVALUATE ESCT-TRAN-CD
               WHEN 'DS'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'AN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'SH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * AM -> UE WHEN EFF DATE IS PAST
      * UE -> UP WHEN EFF DATE IS PAST
      * UP -> AM WHEN EFF DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4400-SET-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UE' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UE'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UP' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UP'
                   IF ESCT-EFF-DATE > 0 AND ESCT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AM' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CUSHION YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4410-ACCRUE-CUSHION.
           IF ESCM-CUSHION-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-CUSHION-YTD * ESCM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ESCM-TAX-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DISBURSE AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4420-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-REGION-CD
                   ADD ESCM-DISBURSE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-DISBURSE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SHORTAGE AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4430-FLAG-SHORTAGE.
           IF ESCT-SHORTAGE-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCT-SHORTAGE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4440-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ESCM-NOTICE-DATE > 0 AND ESCM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ESCM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ESCM-SURPLUS-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ESCM-SURPLUS-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ESCM-SURPLUS-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ESCM-SURPLUS-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-SURPLUS-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE TAX AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4450-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ESCT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ESCM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCM-PREFERRED-FLAG = 'Y'
                   COMPUTE ESCM-PAYMENT-AMT ROUNDED = ESCM-TAX-AMT *
                       ESCM-DISCOUNT / 200
               WHEN ESCT-TRAN-CD = 'DS'
                   MOVE 20.00 TO ESCM-PAYMENT-AMT
               WHEN ESCT-TRAN-CD = 'PY'
                   MOVE 15.00 TO ESCM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE ESCM-PAYMENT-AMT ROUNDED = ESCM-TAX-AMT *
                       ESCM-DISCOUNT / 100
           END-EVALUATE
           IF ESCM-PAYMENT-AMT > 0 AND ESCM-PAYMENT-AMT < 1.00
               MOVE 1.00 TO ESCM-PAYMENT-AMT
           END-IF
           IF ESCM-PAYMENT-AMT > 35.00
               MOVE 35.00 TO ESCM-PAYMENT-AMT
           END-IF
           ADD ESCM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE YTD BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4460-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-PAYEE-TYPE-CD
                   ADD ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON TAX YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4470-ACCRUE-TAX.
           IF ESCM-TAX-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-TAX-YTD * ESCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAX BAND TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4480-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-BAND-RATE(1) TO ESCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ESCM-PAYMENT-AMT NOT < WS-TAX-BAND-LOW(WS-TX)
                   MOVE WS-TAX-BAND-RATE(WS-TX) TO ESCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E123 CONTACT REQUIRED
      * E110 DISBURSE YTD NEGATIVE
      * E051 DISBURSE YTD OVER 10000
      * E868 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4490-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ESCM-CONTACT = SPACES
                   MOVE 'E123' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-YTD < 0
                   MOVE 'E110' TO WS-REJECT-CODE
               WHEN ESCM-DISBURSE-YTD > 10000.00
                   MOVE 'E051' TO WS-REJECT-CODE
               WHEN ESCT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E868' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * C -> F WHEN NEXT REVIEW DATE IS PAST
      * F -> K WHEN NEXT REVIEW DATE IS PAST
      * K -> L WHEN NEXT REVIEW DATE IS PAST
      * L -> S WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4500-SET-REASON.
           EVALUATE ESCM-REASON-CD
               WHEN 'C'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'F' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'K' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'L' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'L'
                   IF ESCM-NEXT-REVIEW-DATE > 0 AND
                       ESCM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'S' TO ESCM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON SHORTAGE AMT AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4510-ACCRUE-SHORTAGE.
           IF ESCT-SHORTAGE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCT-SHORTAGE-AMT * ESCM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO ESCM-TAX-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR ANALYSIS CD:
      * GD -> LY WHEN CLOSE DATE IS PAST
      * LY -> XJ WHEN CLOSE DATE IS PAST
      * XJ -> GD WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4520-SET-ANALYSIS.
           EVALUATE ESCM-ANALYSIS-CD
               WHEN 'GD'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LY' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LY'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XJ' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XJ'
                   IF ESCM-CLOSE-DATE > 0 AND ESCM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GD' TO ESCM-ANALYSIS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4530-ACCRUE-PAYMENT.
           IF ESCM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ESCM-PAYMENT-YTD * ESCM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ESCM-SHORTAGE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SURPLUS YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4540-FLAG-SURPLUS.
           IF ESCM-SURPLUS-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ESCM-SURPLUS-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE TAX BAND TIER FOR THE CUSHION YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4550-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-TAX-BAND-RATE(1) TO ESCM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ESCM-CUSHION-YTD NOT < WS-TAX-BAND-LOW(WS-TX)
                   MOVE WS-TAX-BAND-RATE(WS-TX) TO ESCM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ESCM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ESCM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * AM -> UE WHEN LAST PAY DATE IS PAST
      * UE -> UP WHEN LAST PAY DATE IS PAST
      * UP -> AM WHEN LAST PAY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4560-SET-PAYEE-TYPE.
           EVALUATE ESCM-PAYEE-TYPE-CD
               WHEN 'AM'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UE' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UE'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UP' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UP'
                   IF ESCM-LAST-PAY-DATE > 0 AND ESCM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'AM' TO ESCM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE DISBURSE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO DISBURSE YTD.
      *----------------------------------------------------------------*
       4570-PRORATE-DISBURSE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-DISBURSE-YTD ROUNDED =
                   ESCM-DISBURSE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ESCM-DISBURSE-YTD
           END-IF
           ADD ESCM-DISBURSE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4580-AGE-CLOSE.
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
               WHEN WS-DAYS NOT > 15
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ESCM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SHORTAGE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4590-FLAG-SHORTAGE.
           IF ESCM-SHORTAGE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ESCM-SHORTAGE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE INSURANCE YTD BY PAYEE TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       4600-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ESCM-PAYEE-TYPE-CD
                   ADD ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ESCM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ESCM-INSURANCE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CUSHION AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       4610-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ESCM-PAYMENT-YTD ROUNDED =
                   ESCM-CUSHION-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ESCM-PAYMENT-YTD
           END-IF
           ADD ESCM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE CUSHION FEE ON THE INSURANCE AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4620-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN ESCM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO ESCT-CUSHION-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ESCT-REVERSAL-FLAG = 'Y'
                   COMPUTE ESCT-CUSHION-AMT ROUNDED = ESCM-INSURANCE-AMT
                       * ESCM-RATE / 200
               WHEN ESCM-TYPE-CD = 'AKX'
                   MOVE 7.00 TO ESCT-CUSHION-AMT
               WHEN ESCM-TYPE-CD = 'REP'
                   MOVE 12.00 TO ESCT-CUSHION-AMT
               WHEN OTHER
                   COMPUTE ESCT-CUSHION-AMT ROUNDED = ESCM-INSURANCE-AMT
                       * ESCM-RATE / 100
           END-EVALUATE
           IF ESCT-CUSHION-AMT > 0 AND ESCT-CUSHION-AMT < 5.00
               MOVE 5.00 TO ESCT-CUSHION-AMT
           END-IF
           IF ESCT-CUSHION-AMT > 25.00
               MOVE 25.00 TO ESCT-CUSHION-AMT
           END-IF
           ADD ESCT-CUSHION-AMT TO WS-TOTAL-FEES.

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
                   MOVE ESRISK-SHORTAGE-YTD
                     TO ESCM-SHORTAGE-YTD
                   MOVE ESRISK-INSURANCE-AMT
                     TO ESCM-INSURANCE-AMT
                   MOVE ESRISK-INSURANCE-YTD
                     TO ESCM-INSURANCE-YTD
                   MOVE ESRISK-STOP-FLAG
                     TO ESCM-STOP-FLAG
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
           DISPLAY 'ESCAGE READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'ESCAGE' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
