      *================================================================*
      * PROGRAM  : AULNOTC                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DEALER AUTO FINANCE - CUSTOMER NOTICES. READS THE   *
      *            ADL TRANSACTIONS, UPDATES THE ADL MASTER AND        *
      *            PRODUCES THE CONTROL REPORT. INDIRECT AUTO DEALER   *
      *            CONTRACTS, RESERVE AND CHARGEBACK                   *
      * INPUT    : ADLMST (KSDS), ADLTRN, BANKCTL                      *
      * OUTPUT   : ADLOUT, AULNORPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, AURISK, AUWAIV          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-09-07 DPS  ORIGINAL                                       *
      * 2000-01-04 LMB  PERFORMANCE - PAYMENT READ AHEAD PRB3048       *
      * 2001-01-16 GAF  PERFORMANCE - RESERVE READ AHEAD REQ6858       *
      * 2004-08-22 RJT  CHARGEBK NOTICE TEXT PER LEGAL REQ4539         *
      * 2005-08-06 CRP  CHARGEBK NOTICE TEXT PER LEGAL INC0262         *
      * 2009-02-24 JLM  REBATE EXCEPTIONS TO OPS QUEUE INC0798         *
      * 2010-11-11 SKN  NEW REBATE REPORT COLUMNS PRB5748              *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AULNOTC.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "ADLMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS ADLM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "ADLTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "ADLOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "AULNORPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY ADLMST.
       FD  TRAN-FILE.
       COPY ADLTRN.
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
           05  WS-PERIOD-DAYS              PIC S9(05) VALUE 91.
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
      * WS-PAYMENT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000837500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000847500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000825500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007000000000342000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007250000000853500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017250000000622500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017350000000709000'.
       01  WS-PAYMENT-TIER-TBL REDEFINES WS-PAYMENT-TIER-VALUES.
           05  WS-PAYMENT-TIER-ROW OCCURS 7 TIMES.
               10  WS-PAYMENT-TIER-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-GAP-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-GAP-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000678000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000448000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000750000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010350000000399000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012850000000805500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013350000000693500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00018350000000519000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019350000000666000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00019850000000332000'.
       01  WS-GAP-TIER-TBL REDEFINES WS-GAP-TIER-VALUES.
           05  WS-GAP-TIER-ROW OCCURS 9 TIMES.
               10  WS-GAP-TIER-LOW         PIC 9(09)V99.
               10  WS-GAP-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RESERVE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RESERVE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000313000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000224000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000089000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002250000000145000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007250000000204000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012250000000448500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013250000000557000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023250000000440000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023350000000579000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023600000000542500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00033600000000380000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00038600000000492500'.
       01  WS-RESERVE-SCHED-TBL REDEFINES WS-RESERVE-SCHED-VALUES.
           05  WS-RESERVE-SCHED-ROW OCCURS 12 TIMES.
               10  WS-RESERVE-SCHED-LOW    PIC 9(09)V99.
               10  WS-RESERVE-SCHED-RATE   PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-PAYMENT-TIERX - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-TIERX-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000595500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000359500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010500000000175000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020500000000376500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023000000000310000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023250000000472000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00025750000000340000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026250000000119000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026300000000724500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026550000000350000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026800000000610500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00036800000000547000'.
       01  WS-PAYMENT-TIERX-TBL REDEFINES WS-PAYMENT-TIERX-VALUES.
           05  WS-PAYMENT-TIERX-ROW OCCURS 12 TIMES.
               10  WS-PAYMENT-TIERX-LOW    PIC 9(09)V99.
               10  WS-PAYMENT-TIERX-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  AULNOTC  DEALER AUTO FINANCE'.
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
       COPY AURISKP.
       COPY AUWAIVP.

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
               MOVE 'ADLMST' TO ABND-FILE-NAME
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
           MOVE ADLT-KEY TO ADLM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-LIMIT-PAYMENT
               PERFORM 3010-FLAG-RESERVE
               PERFORM 3020-ASSESS-REVERSAL
               PERFORM 3030-ACCRUE-RESERVE
               PERFORM 3040-SELECT-RATE
               PERFORM 3050-SPLIT-CHARGEBK
               PERFORM 3060-CHECK-EXPIRY
               PERFORM 3070-SET-DEALER
               PERFORM 3080-CHECK-LAST-PAY
               PERFORM 3090-ACCRUE-REBATE
               PERFORM 3100-SELECT-RATE
               PERFORM 3110-COUNT-RETRY
               PERFORM 3120-FLAG-AMOUNT-FIN
               PERFORM 3130-ASSESS-PREFERRED
               PERFORM 3140-CHECK-LAST-ACT
               PERFORM 3150-PRORATE-GAP
               PERFORM 3160-DESCRIBE-CYCLE
               PERFORM 3170-ACCUM-REASON
               PERFORM 3180-CHECK-LAST-STMT
               PERFORM 3190-ACCRUE-RATE
               PERFORM 3200-CHECK-EFFECTIVE
               PERFORM 3210-DESCRIBE-TRAN
               PERFORM 3220-PRORATE-REBATE
               PERFORM 3230-SET-REASON
               PERFORM 3240-ACCRUE-RESERVE
               PERFORM 3250-DESCRIBE-TIER
               PERFORM 3260-EDIT-ADDR
               PERFORM 3270-EDIT-REF-NO
               PERFORM 3280-PRORATE-RESERVE
               PERFORM 3290-ASSESS-EXEMPT
               PERFORM 3300-SELECT-RATE
               PERFORM 3310-FLAG-GAP
               PERFORM 3320-CHECK-LAST-PAY
               PERFORM 3330-COUNT-CYCLE
               PERFORM 3340-PRORATE-GAP
               PERFORM 3350-AGE-NEXT-REVIEW
               PERFORM 3360-SET-STATUS
               PERFORM 3370-CHECK-LAST-PAY
               PERFORM 3380-COUNT-SEQ
               PERFORM 3390-EDIT-OPERATOR
               PERFORM 3400-ACCUM-TIER
               PERFORM 3410-EDIT-OPERATOR
               PERFORM 3420-EDIT-ADDR
               PERFORM 3430-COUNT-RETRY
               PERFORM 3440-AGE-LAST-PAY
               PERFORM 3450-FLAG-REBATE
               PERFORM 3460-AGE-EFFECTIVE
               PERFORM 3470-SET-TYPE
               PERFORM 3480-DESCRIBE-TRAN
               PERFORM 3490-SET-CHANNEL
               PERFORM 3500-ACCUM-TIER
               PERFORM 3510-PRORATE-AMOUNT-FIN
               PERFORM 3520-SPLIT-RESERVE
               PERFORM 3530-AGE-LAST-STMT
               PERFORM 3540-SET-TIER
               PERFORM 3550-ACCRUE-CHARGEBK
               PERFORM 3560-FLAG-CHARGEBK
               PERFORM 3570-LIMIT-CHARGEBK
               PERFORM 3580-CHECK-NOTICE
               PERFORM 3590-FLAG-PAYMENT
               PERFORM 3600-ASSESS-REVIEW
               PERFORM 3610-ACCRUE-RESERVE
               PERFORM 3620-SPLIT-PAYMENT
               PERFORM 3630-ACCRUE-PAYMENT
               PERFORM 3640-ACCUM-TYPE
               PERFORM 3650-ACCUM-CYCLE
               PERFORM 3660-COUNT-CYCLE
               PERFORM 3670-PRORATE-REBATE
               PERFORM 3680-COUNT-SEQ
               PERFORM 3690-SET-STATUS
               PERFORM 3700-EDIT-DESC
               PERFORM 3710-ASSESS-OVERRIDE
               PERFORM 3720-SPLIT-PAYMENT
               PERFORM 3730-COUNT-EXCEPT
               PERFORM 3740-SET-REASON
               PERFORM 3750-ACCRUE-RESERVE
               PERFORM 3760-CHECK-EXPIRY
               PERFORM 3770-ASSESS-REVERSAL
               PERFORM 3780-LIMIT-REBATE
               PERFORM 3790-ACCRUE-CHARGEBK
               PERFORM 3800-ASSESS-OVERRIDE
               PERFORM 3810-EDIT-MEMO
               PERFORM 3820-AGE-LAST-ACT
               PERFORM 3830-ASSESS-EXEMPT
               PERFORM 3840-SPLIT-RESERVE
               PERFORM 3850-ACCUM-CHANNEL
               PERFORM 3860-AGE-LAST-MAINT
               PERFORM 3870-CHECK-NOTICE
               PERFORM 3880-SPLIT-RESERVE
               PERFORM 3890-ACCUM-TIER
               PERFORM 3900-ACCRUE-CHARGEBK
               PERFORM 3910-SELECT-FEE-PCT
               PERFORM 3920-SELECT-MARGIN
               PERFORM 3930-SET-DEALER
               PERFORM 3940-SET-CHANNEL
               PERFORM 3950-AGE-LAST-MAINT
               PERFORM 3960-ACCUM-TRAN
               PERFORM 3970-AGE-LAST-PAY
               PERFORM 3980-ACCUM-TIER
               PERFORM 3990-COUNT-RETRY
               PERFORM 4000-SELECT-FEE-PCT
               PERFORM 4010-EDIT-REJECT
               PERFORM 4020-SELECT-MARGIN
               PERFORM 4030-COUNT-PAYMENT
               PERFORM 4040-COUNT-RETRY
               PERFORM 4050-DESCRIBE-CHANNEL
               PERFORM 4060-ACCUM-TYPE
               PERFORM 4070-SELECT-RATE
               PERFORM 4080-PRORATE-RESERVE
               PERFORM 4090-SPLIT-AMOUNT-FIN
               PERFORM 4100-EDIT-NAME
               PERFORM 4110-SET-TYPE
               PERFORM 4120-AGE-LAST-MAINT
               PERFORM 4130-EDIT-ADDR
               PERFORM 4140-SELECT-MARGIN
               PERFORM 4150-DESCRIBE-STATUS
               PERFORM 4160-SELECT-MARGIN
               PERFORM 4170-PRORATE-PAYMENT
               PERFORM 4180-SELECT-FEE-PCT
               PERFORM 4190-SPLIT-PAYMENT
               PERFORM 4200-ACCUM-TYPE
               PERFORM 4210-ACCUM-TYPE
               PERFORM 4220-FLAG-PAYMENT
               PERFORM 4230-ASSESS-REVIEW
               PERFORM 4240-SPLIT-RESERVE
               PERFORM 4250-ACCRUE-RESERVE
               PERFORM 4260-AGE-NEXT-DUE
               PERFORM 4270-CHECK-LAST-ACT
               PERFORM 4280-CHECK-NEXT-REVIEW
               PERFORM 4290-CHECK-EXPIRY
               PERFORM 4300-LIMIT-RESERVE
               PERFORM 4310-SELECT-FEE-PCT
               PERFORM 4320-AGE-NOTICE
               PERFORM 7030-CALL-AURISK
               PERFORM 7010-CALL-AUWAIV
               REWRITE ADL-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'ADLMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM ADL-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3000-LIMIT-PAYMENT.
           IF ADLM-PAYMENT-YTD < 0.00
               MOVE 0.00 TO ADLM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-PAYMENT-YTD > 10000.00
               MOVE 10000.00 TO ADLM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESERVE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3010-FLAG-RESERVE.
           IF ADLT-RESERVE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ADLT-RESERVE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGEBK YTD FEE ON THE AMOUNT FIN AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3020-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ADLM-CHARGEBK-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ADLM-CHARGEBK-YTD ROUNDED =
                       ADLT-AMOUNT-FIN-AMT * ADLM-MARGIN / 200
               WHEN ADLM-REASON-CD = 'FZV'
                   MOVE 5.00 TO ADLM-CHARGEBK-YTD
               WHEN ADLM-REASON-CD = 'KPP'
                   MOVE 12.00 TO ADLM-CHARGEBK-YTD
               WHEN OTHER
                   COMPUTE ADLM-CHARGEBK-YTD ROUNDED =
                       ADLT-AMOUNT-FIN-AMT * ADLM-MARGIN / 100
           END-EVALUATE
           IF ADLM-CHARGEBK-YTD > 0 AND ADLM-CHARGEBK-YTD < 10.00
               MOVE 10.00 TO ADLM-CHARGEBK-YTD
           END-IF
           IF ADLM-CHARGEBK-YTD > 25.00
               MOVE 25.00 TO ADLM-CHARGEBK-YTD
           END-IF
           ADD ADLM-CHARGEBK-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-RESERVE.
           IF ADLT-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-RESERVE-AMT * ADLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO ADLM-CHARGEBK-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT TIERX TIER FOR THE GAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3040-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-TIERX-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ADLM-GAP-AMT NOT < WS-PAYMENT-TIERX-LOW(WS-TX)
                   MOVE WS-PAYMENT-TIERX-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-CHARGEBK.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 4)
           COMPUTE ADLM-CHARGEBK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3060-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEALER CD:
      * DRU -> FNF WHEN EFFECTIVE DATE IS PAST
      * FNF -> GBX WHEN EFFECTIVE DATE IS PAST
      * GBX -> GTU WHEN EFFECTIVE DATE IS PAST
      * GTU -> JNE WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3070-SET-DEALER.
           EVALUATE ADLM-DEALER-CD
               WHEN 'DRU'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNF' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNF'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GBX' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GBX'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GTU' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GTU'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JNE' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 45 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3080-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REBATE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3090-ACCRUE-REBATE.
           IF ADLM-REBATE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-REBATE-YTD * ADLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT TIERX TIER FOR THE GAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3100-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-TIERX-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ADLM-GAP-AMT NOT < WS-PAYMENT-TIERX-LOW(WS-TX)
                   MOVE WS-PAYMENT-TIERX-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3110-COUNT-RETRY.
           IF ADLM-RETRY-CNT < 9999999
               ADD 1 TO ADLM-RETRY-CNT
           END-IF
           IF ADLM-RETRY-CNT NOT < 3
               MOVE 'Y' TO ADLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT FIN YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3120-FLAG-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ADLM-AMOUNT-FIN-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE GAP YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3130-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ADLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ADLM-GAP-YTD *
                       ADLM-MARGIN / 200
               WHEN ADLM-STATUS-CD = 'EUA'
                   MOVE 10.00 TO WS-WORK-AMT
               WHEN ADLM-STATUS-CD = 'HMW'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ADLM-GAP-YTD *
                       ADLM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3140-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GAP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3150-PRORATE-GAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-GAP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3160-DESCRIBE-CYCLE.
           EVALUATE ADLM-CYCLE-CD
               WHEN 'A'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'J'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'T'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'W'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3170-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-REASON-CD
                   ADD ADLT-RESERVE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLT-RESERVE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3180-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ADLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3190-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ADLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ADLM-GAP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO ADLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3210-DESCRIBE-TRAN.
           EVALUATE ADLT-TRAN-CD
               WHEN 'BK'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CB'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RB'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REBATE YTD.
      *----------------------------------------------------------------*
       3220-PRORATE-REBATE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-REBATE-YTD ROUNDED =
                   ADLT-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-REBATE-YTD
           END-IF
           ADD ADLM-REBATE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * FZV -> KPP WHEN NEXT DUE DATE IS PAST
      * KPP -> MVN WHEN NEXT DUE DATE IS PAST
      * MVN -> UVY WHEN NEXT DUE DATE IS PAST
      * UVY -> ZWZ WHEN NEXT DUE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-REASON.
           EVALUATE ADLM-REASON-CD
               WHEN 'FZV'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KPP' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KPP'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MVN' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MVN'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UVY' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UVY'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWZ' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-RESERVE.
           IF ADLM-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-RESERVE-AMT * ADLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO ADLM-CHARGEBK-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3250-DESCRIBE-TIER.
           EVALUATE ADLM-TIER-CD
               WHEN 'EW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UG'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'WY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E751 ADDR REQUIRED
      * E163 GAP AMT NEGATIVE
      * E386 GAP AMT OVER 5000
      * E714 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3260-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-ADDR = SPACES
                   MOVE 'E751' TO WS-REJECT-CODE
               WHEN ADLM-GAP-AMT < 0
                   MOVE 'E163' TO WS-REJECT-CODE
               WHEN ADLM-GAP-AMT > 5000.00
                   MOVE 'E386' TO WS-REJECT-CODE
               WHEN ADLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E714' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E955 REF NO REQUIRED
      * E111 CHARGEBK YTD NEGATIVE
      * E596 CHARGEBK YTD OVER 5000
      * E309 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3270-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-REF-NO = SPACES
                   MOVE 'E955' TO WS-REJECT-CODE
               WHEN ADLM-CHARGEBK-YTD < 0
                   MOVE 'E111' TO WS-REJECT-CODE
               WHEN ADLM-CHARGEBK-YTD > 5000.00
                   MOVE 'E596' TO WS-REJECT-CODE
               WHEN ADLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E309' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESERVE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3280-PRORATE-RESERVE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-RESERVE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE RESERVE FEE ON THE PAYMENT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3290-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ADLT-RESERVE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-PREFERRED-FLAG = 'Y'
                   COMPUTE ADLT-RESERVE-AMT ROUNDED = ADLM-PAYMENT-YTD *
                       ADLM-FEE-PCT / 200
               WHEN ADLM-TIER-CD = 'EW'
                   MOVE 12.00 TO ADLT-RESERVE-AMT
               WHEN ADLM-TIER-CD = 'JF'
                   MOVE 12.00 TO ADLT-RESERVE-AMT
               WHEN OTHER
                   COMPUTE ADLT-RESERVE-AMT ROUNDED = ADLM-PAYMENT-YTD *
                       ADLM-FEE-PCT / 100
           END-EVALUATE
           IF ADLT-RESERVE-AMT > 0 AND ADLT-RESERVE-AMT < 10.00
               MOVE 10.00 TO ADLT-RESERVE-AMT
           END-IF
           IF ADLT-RESERVE-AMT > 35.00
               MOVE 35.00 TO ADLT-RESERVE-AMT
           END-IF
           ADD ADLT-RESERVE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE SCHED TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3300-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-SCHED-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ADLM-REBATE-AMT NOT < WS-RESERVE-SCHED-LOW(WS-TX)
                   MOVE WS-RESERVE-SCHED-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GAP AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3310-FLAG-GAP.
           IF ADLM-GAP-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ADLM-GAP-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3320-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ADLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3330-COUNT-CYCLE.
           IF ADLM-CYCLE-CNT < 9999999
               ADD 1 TO ADLM-CYCLE-CNT
           END-IF
           IF ADLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO ADLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT FIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO GAP AMT.
      *----------------------------------------------------------------*
       3340-PRORATE-GAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-GAP-AMT ROUNDED =
                   ADLT-AMOUNT-FIN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-GAP-AMT
           END-IF
           ADD ADLM-GAP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3350-AGE-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NEXT-REVIEW-DATE > 0 AND ADLM-NEXT-REVIEW-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EUA -> HMW WHEN NOTICE DATE IS PAST
      * HMW -> RCB WHEN NOTICE DATE IS PAST
      * RCB -> EUA WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3360-SET-STATUS.
           EVALUATE ADLM-STATUS-CD
               WHEN 'EUA'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HMW' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HMW'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RCB' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RCB'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EUA' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3370-CHECK-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-PAY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ADLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3380-COUNT-SEQ.
           IF ADLT-SEQ < 9999999
               ADD 1 TO ADLT-SEQ
           END-IF
           IF ADLT-SEQ NOT < 5
               MOVE 'Y' TO ADLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E209 OPERATOR REQUIRED
      * E702 PAYMENT AMT NEGATIVE
      * E835 PAYMENT AMT OVER 25000
      * E805 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3390-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-OPERATOR = SPACES
                   MOVE 'E209' TO WS-REJECT-CODE
               WHEN ADLM-PAYMENT-AMT < 0
                   MOVE 'E702' TO WS-REJECT-CODE
               WHEN ADLM-PAYMENT-AMT > 25000.00
                   MOVE 'E835' TO WS-REJECT-CODE
               WHEN ADLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E805' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3400-ACCUM-TIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TIER-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E214 OPERATOR REQUIRED
      * E433 RESERVE AMT NEGATIVE
      * E937 RESERVE AMT OVER 10000
      * E266 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3410-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-OPERATOR = SPACES
                   MOVE 'E214' TO WS-REJECT-CODE
               WHEN ADLT-RESERVE-AMT < 0
                   MOVE 'E433' TO WS-REJECT-CODE
               WHEN ADLT-RESERVE-AMT > 10000.00
                   MOVE 'E937' TO WS-REJECT-CODE
               WHEN ADLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E266' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E948 ADDR REQUIRED
      * E084 RESERVE AMT NEGATIVE
      * E538 RESERVE AMT OVER 50000
      * E300 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3420-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-ADDR = SPACES
                   MOVE 'E948' TO WS-REJECT-CODE
               WHEN ADLT-RESERVE-AMT < 0
                   MOVE 'E084' TO WS-REJECT-CODE
               WHEN ADLT-RESERVE-AMT > 50000.00
                   MOVE 'E538' TO WS-REJECT-CODE
               WHEN ADLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E300' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3430-COUNT-RETRY.
           IF ADLM-RETRY-CNT < 9999999
               ADD 1 TO ADLM-RETRY-CNT
           END-IF
           IF ADLM-RETRY-CNT NOT < 12
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3440-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REBATE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3450-FLAG-REBATE.
           IF ADLM-REBATE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ADLM-REBATE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-RESERVE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-RESERVE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-RESERVE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLM-RESERVE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-RESERVE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN POST DATE IS PAST
      * H -> S WHEN POST DATE IS PAST
      * S -> V WHEN POST DATE IS PAST
      * V -> W WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3470-SET-TYPE.
           EVALUATE ADLM-TYPE-CD
               WHEN 'B'
                   IF ADLT-POST-DATE > 0 AND ADLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ADLT-POST-DATE > 0 AND ADLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF ADLT-POST-DATE > 0 AND ADLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF ADLT-POST-DATE > 0 AND ADLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3480-DESCRIBE-TRAN.
           EVALUATE ADLT-TRAN-CD
               WHEN 'BK'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'PY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RB'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN POST DATE IS PAST
      * BAT -> MAL WHEN POST DATE IS PAST
      * MAL -> TEL WHEN POST DATE IS PAST
      * TEL -> ONL WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3490-SET-CHANNEL.
           EVALUATE ADLT-CHANNEL-CD
               WHEN 'BRN'
                   IF ADLT-POST-DATE > 0 AND ADLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ADLT-POST-DATE > 0 AND ADLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ADLT-POST-DATE > 0 AND ADLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ADLT-POST-DATE > 0 AND ADLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT FIN AMT BY TIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3500-ACCUM-TIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TIER-CD
                   ADD ADLM-AMOUNT-FIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-AMOUNT-FIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REBATE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT FIN AMT.
      *----------------------------------------------------------------*
       3510-PRORATE-AMOUNT-FIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLT-AMOUNT-FIN-AMT ROUNDED =
                   ADLM-REBATE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLT-AMOUNT-FIN-AMT
           END-IF
           ADD ADLT-AMOUNT-FIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3520-SPLIT-RESERVE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE ADLM-RESERVE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3530-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-GAP-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ADLM-GAP-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-GAP-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-GAP-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-GAP-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TIER CD:
      * EW -> JF WHEN LAST STMT DATE IS PAST
      * JF -> KD WHEN LAST STMT DATE IS PAST
      * KD -> UG WHEN LAST STMT DATE IS PAST
      * UG -> WY WHEN LAST STMT DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3540-SET-TIER.
           EVALUATE ADLM-TIER-CD
               WHEN 'EW'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JF' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGEBK YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3550-ACCRUE-CHARGEBK.
           IF ADLM-CHARGEBK-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-CHARGEBK-YTD * ADLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK YTD OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3560-FLAG-CHARGEBK.
           IF ADLM-CHARGEBK-YTD > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ADLM-CHARGEBK-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3570-LIMIT-CHARGEBK.
           IF ADLM-CHARGEBK-AMT < 0.00
               MOVE 0.00 TO ADLM-CHARGEBK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-CHARGEBK-AMT > 2500.00
               MOVE 2500.00 TO ADLM-CHARGEBK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3580-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ADLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3590-FLAG-PAYMENT.
           IF ADLM-PAYMENT-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ADLM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESERVE FEE ON THE AMOUNT FIN AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3600-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ADLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ADLT-RESERVE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   COMPUTE ADLT-RESERVE-AMT ROUNDED =
                       ADLT-AMOUNT-FIN-AMT * ADLM-FEE-PCT / 200
               WHEN ADLT-TRAN-CD = 'BK'
                   MOVE 7.00 TO ADLT-RESERVE-AMT
               WHEN ADLT-TRAN-CD = 'PY'
                   MOVE 12.00 TO ADLT-RESERVE-AMT
               WHEN OTHER
                   COMPUTE ADLT-RESERVE-AMT ROUNDED =
                       ADLT-AMOUNT-FIN-AMT * ADLM-FEE-PCT / 100
           END-EVALUATE
           IF ADLT-RESERVE-AMT > 0 AND ADLT-RESERVE-AMT < 5.00
               MOVE 5.00 TO ADLT-RESERVE-AMT
           END-IF
           IF ADLT-RESERVE-AMT > 250.00
               MOVE 250.00 TO ADLT-RESERVE-AMT
           END-IF
           ADD ADLT-RESERVE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3610-ACCRUE-RESERVE.
           IF ADLM-RESERVE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-RESERVE-YTD * ADLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ADLT-REBATE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3620-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = ADLM-PAYMENT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ADLM-PAYMENT-YTD - (WS-WORK-AMT * 2)
           COMPUTE ADLT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3630-ACCRUE-PAYMENT.
           IF ADLM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-PAYMENT-AMT * ADLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ADLT-AMOUNT-FIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT FIN YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3640-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TYPE-CD
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-AMOUNT-FIN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3650-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-CYCLE-CD
                   ADD ADLM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-CYCLE.
           IF ADLM-CYCLE-CNT < 9999999
               ADD 1 TO ADLM-CYCLE-CNT
           END-IF
           IF ADLM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO ADLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHARGEBK YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REBATE AMT.
      *----------------------------------------------------------------*
       3670-PRORATE-REBATE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLT-REBATE-AMT ROUNDED =
                   ADLM-CHARGEBK-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLT-REBATE-AMT
           END-IF
           ADD ADLT-REBATE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3680-COUNT-SEQ.
           IF ADLT-SEQ < 9999999
               ADD 1 TO ADLT-SEQ
           END-IF
           IF ADLT-SEQ NOT < 10
               MOVE 'Y' TO ADLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EUA -> HMW WHEN EFF DATE IS PAST
      * HMW -> RCB WHEN EFF DATE IS PAST
      * RCB -> EUA WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-STATUS.
           EVALUATE ADLM-STATUS-CD
               WHEN 'EUA'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HMW' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HMW'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RCB' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RCB'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EUA' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E292 DESC REQUIRED
      * E604 PAYMENT AMT NEGATIVE
      * E175 PAYMENT AMT OVER 5000
      * E185 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3700-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-DESC = SPACES
                   MOVE 'E292' TO WS-REJECT-CODE
               WHEN ADLT-PAYMENT-AMT < 0
                   MOVE 'E604' TO WS-REJECT-CODE
               WHEN ADLT-PAYMENT-AMT > 5000.00
                   MOVE 'E175' TO WS-REJECT-CODE
               WHEN ADLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E185' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT FIN FEE ON THE GAP YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3710-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ADLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ADLM-AMOUNT-FIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   COMPUTE ADLM-AMOUNT-FIN-AMT ROUNDED = ADLM-GAP-YTD *
                       ADLM-FEE-PCT / 200
               WHEN ADLM-TYPE-CD = 'B'
                   MOVE 20.00 TO ADLM-AMOUNT-FIN-AMT
               WHEN ADLM-TYPE-CD = 'H'
                   MOVE 12.00 TO ADLM-AMOUNT-FIN-AMT
               WHEN OTHER
                   COMPUTE ADLM-AMOUNT-FIN-AMT ROUNDED = ADLM-GAP-YTD *
                       ADLM-FEE-PCT / 100
           END-EVALUATE
           IF ADLM-AMOUNT-FIN-AMT > 0 AND ADLM-AMOUNT-FIN-AMT < 2.00
               MOVE 2.00 TO ADLM-AMOUNT-FIN-AMT
           END-IF
           IF ADLM-AMOUNT-FIN-AMT > 25.00
               MOVE 25.00 TO ADLM-AMOUNT-FIN-AMT
           END-IF
           ADD ADLM-AMOUNT-FIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3720-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = ADLM-PAYMENT-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ADLM-PAYMENT-YTD - (WS-WORK-AMT * 2)
           COMPUTE ADLM-AMOUNT-FIN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3730-COUNT-EXCEPT.
           IF ADLM-EXCEPT-CNT < 9999999
               ADD 1 TO ADLM-EXCEPT-CNT
           END-IF
           IF ADLM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO ADLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * FZV -> KPP WHEN NOTICE DATE IS PAST
      * KPP -> MVN WHEN NOTICE DATE IS PAST
      * MVN -> UVY WHEN NOTICE DATE IS PAST
      * UVY -> ZWZ WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3740-SET-REASON.
           EVALUATE ADLM-REASON-CD
               WHEN 'FZV'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KPP' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KPP'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MVN' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MVN'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UVY' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UVY'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWZ' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3750-ACCRUE-RESERVE.
           IF ADLM-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-RESERVE-AMT * ADLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ADLT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ADLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PAYMENT AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3770-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ADLT-PAYMENT-AMT *
                       ADLM-RATE / 200
               WHEN ADLM-DEALER-CD = 'DRU'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN ADLM-DEALER-CD = 'FNF'
                   MOVE 7.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ADLT-PAYMENT-AMT *
                       ADLM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 1.00
               MOVE 1.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 75.00
               MOVE 75.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * REBATE YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3780-LIMIT-REBATE.
           IF ADLM-REBATE-YTD < 50.00
               MOVE 50.00 TO ADLM-REBATE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-REBATE-YTD > 2500.00
               MOVE 2500.00 TO ADLM-REBATE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGEBK YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3790-ACCRUE-CHARGEBK.
           IF ADLM-CHARGEBK-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-CHARGEBK-YTD * ADLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO ADLM-REBATE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT FIN FEE ON THE RESERVE YTD.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3800-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ADLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ADLT-AMOUNT-FIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ADLT-AMOUNT-FIN-AMT ROUNDED =
                       ADLM-RESERVE-YTD * ADLM-FEE-PCT / 200
               WHEN ADLM-TYPE-CD = 'B'
                   MOVE 15.00 TO ADLT-AMOUNT-FIN-AMT
               WHEN ADLM-TYPE-CD = 'H'
                   MOVE 5.00 TO ADLT-AMOUNT-FIN-AMT
               WHEN OTHER
                   COMPUTE ADLT-AMOUNT-FIN-AMT ROUNDED =
                       ADLM-RESERVE-YTD * ADLM-FEE-PCT / 100
           END-EVALUATE
           IF ADLT-AMOUNT-FIN-AMT > 0 AND ADLT-AMOUNT-FIN-AMT < 10.00
               MOVE 10.00 TO ADLT-AMOUNT-FIN-AMT
           END-IF
           IF ADLT-AMOUNT-FIN-AMT > 50.00
               MOVE 50.00 TO ADLT-AMOUNT-FIN-AMT
           END-IF
           ADD ADLT-AMOUNT-FIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E436 MEMO REQUIRED
      * E273 GAP YTD NEGATIVE
      * E068 GAP YTD OVER 1000000
      * E987 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       3810-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-MEMO = SPACES
                   MOVE 'E436' TO WS-REJECT-CODE
               WHEN ADLM-GAP-YTD < 0
                   MOVE 'E273' TO WS-REJECT-CODE
               WHEN ADLM-GAP-YTD > 1000000.00
                   MOVE 'E068' TO WS-REJECT-CODE
               WHEN ADLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E987' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3820-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-ACT-DATE > 0 AND ADLM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-PAYMENT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RESERVE FEE ON THE CHARGEBK YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3830-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ADLM-RESERVE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   COMPUTE ADLM-RESERVE-AMT ROUNDED = ADLM-CHARGEBK-YTD
                       * ADLM-MARGIN / 200
               WHEN ADLM-CYCLE-CD = 'A'
                   MOVE 20.00 TO ADLM-RESERVE-AMT
               WHEN ADLM-CYCLE-CD = 'H'
                   MOVE 3.00 TO ADLM-RESERVE-AMT
               WHEN OTHER
                   COMPUTE ADLM-RESERVE-AMT ROUNDED = ADLM-CHARGEBK-YTD
                       * ADLM-MARGIN / 100
           END-EVALUATE
           IF ADLM-RESERVE-AMT > 0 AND ADLM-RESERVE-AMT < 5.00
               MOVE 5.00 TO ADLM-RESERVE-AMT
           END-IF
           IF ADLM-RESERVE-AMT > 50.00
               MOVE 50.00 TO ADLM-RESERVE-AMT
           END-IF
           ADD ADLM-RESERVE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT RESERVE AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3840-SPLIT-RESERVE.
           COMPUTE WS-WORK-AMT = ADLT-RESERVE-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ADLT-RESERVE-AMT - (WS-WORK-AMT * 2)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3850-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLT-CHANNEL-CD
                   ADD ADLM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3870-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESERVE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3880-SPLIT-RESERVE.
           COMPUTE WS-WORK-AMT = ADLM-RESERVE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ADLM-RESERVE-YTD - (WS-WORK-AMT * 4)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY TIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3890-ACCUM-TIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TIER-CD
                   ADD ADLM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGEBK AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3900-ACCRUE-CHARGEBK.
           IF ADLM-CHARGEBK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-CHARGEBK-AMT * ADLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ADLM-REBATE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP TIER TIER FOR THE CHARGEBK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3910-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-TIER-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ADLM-CHARGEBK-YTD NOT < WS-GAP-TIER-LOW(WS-TX)
                   MOVE WS-GAP-TIER-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP TIER TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3920-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-TIER-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ADLM-REBATE-AMT NOT < WS-GAP-TIER-LOW(WS-TX)
                   MOVE WS-GAP-TIER-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEALER CD:
      * DRU -> FNF WHEN EXPIRY DATE IS PAST
      * FNF -> GBX WHEN EXPIRY DATE IS PAST
      * GBX -> GTU WHEN EXPIRY DATE IS PAST
      * GTU -> JNE WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3930-SET-DEALER.
           EVALUATE ADLM-DEALER-CD
               WHEN 'DRU'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNF' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNF'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GBX' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GBX'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GTU' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GTU'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JNE' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST STMT DATE IS PAST
      * BAT -> MAL WHEN LAST STMT DATE IS PAST
      * MAL -> TEL WHEN LAST STMT DATE IS PAST
      * TEL -> ONL WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3940-SET-CHANNEL.
           EVALUATE ADLT-CHANNEL-CD
               WHEN 'BRN'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3950-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-GAP-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ADLM-GAP-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ADLM-GAP-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLM-GAP-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-GAP-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE GAP YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3960-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLT-TRAN-CD
                   ADD ADLM-GAP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-GAP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       3970-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-RESERVE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ADLM-RESERVE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-RESERVE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-RESERVE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-RESERVE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3980-ACCUM-TIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TIER-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       3990-COUNT-RETRY.
           IF ADLM-RETRY-CNT < 9999999
               ADD 1 TO ADLM-RETRY-CNT
           END-IF
           IF ADLM-RETRY-CNT NOT < 12
               MOVE 'Y' TO ADLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT TIERX TIER FOR THE AMOUNT FIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4000-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-TIERX-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ADLM-AMOUNT-FIN-YTD NOT < WS-PAYMENT-TIERX-LOW(WS-TX)
                   MOVE WS-PAYMENT-TIERX-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E070 REJECT CD REQUIRED
      * E151 PAYMENT YTD NEGATIVE
      * E894 PAYMENT YTD OVER 25000
      * E443 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-REJECT-CD = SPACES
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN ADLM-PAYMENT-YTD < 0
                   MOVE 'E151' TO WS-REJECT-CODE
               WHEN ADLM-PAYMENT-YTD > 25000.00
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN ADLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E443' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE SCHED TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4020-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-SCHED-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-RESERVE-SCHED-LOW(WS-TX)
                   MOVE WS-RESERVE-SCHED-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4030-COUNT-PAYMENT.
           IF ADLM-PAYMENT-CNT < 9999999
               ADD 1 TO ADLM-PAYMENT-CNT
           END-IF
           IF ADLM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4040-COUNT-RETRY.
           IF ADLM-RETRY-CNT < 9999999
               ADD 1 TO ADLM-RETRY-CNT
           END-IF
           IF ADLM-RETRY-CNT NOT < 5
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4050-DESCRIBE-CHANNEL.
           EVALUATE ADLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REBATE YTD BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4060-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TYPE-CD
                   ADD ADLM-REBATE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-REBATE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE SCHED TIER FOR THE RESERVE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-SCHED-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ADLT-RESERVE-AMT NOT < WS-RESERVE-SCHED-LOW(WS-TX)
                   MOVE WS-RESERVE-SCHED-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESERVE AMT.
      *----------------------------------------------------------------*
       4080-PRORATE-RESERVE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-RESERVE-AMT ROUNDED =
                   ADLM-PAYMENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-RESERVE-AMT
           END-IF
           ADD ADLM-RESERVE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT FIN AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4090-SPLIT-AMOUNT-FIN.
           COMPUTE WS-WORK-AMT = ADLT-AMOUNT-FIN-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ADLT-AMOUNT-FIN-AMT - (WS-WORK-AMT *
               12)
           COMPUTE ADLM-GAP-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E094 NAME REQUIRED
      * E665 GAP YTD NEGATIVE
      * E687 GAP YTD OVER 50000
      * E364 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4100-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-NAME = SPACES
                   MOVE 'E094' TO WS-REJECT-CODE
               WHEN ADLM-GAP-YTD < 0
                   MOVE 'E665' TO WS-REJECT-CODE
               WHEN ADLM-GAP-YTD > 50000.00
                   MOVE 'E687' TO WS-REJECT-CODE
               WHEN ADLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E364' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN LAST ACT DATE IS PAST
      * H -> S WHEN LAST ACT DATE IS PAST
      * S -> V WHEN LAST ACT DATE IS PAST
      * V -> W WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4110-SET-TYPE.
           EVALUATE ADLM-TYPE-CD
               WHEN 'B'
                   IF ADLM-LAST-ACT-DATE > 0 AND ADLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ADLM-LAST-ACT-DATE > 0 AND ADLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF ADLM-LAST-ACT-DATE > 0 AND ADLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF ADLM-LAST-ACT-DATE > 0 AND ADLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4120-AGE-LAST-MAINT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-MAINT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E756 ADDR REQUIRED
      * E618 REBATE YTD NEGATIVE
      * E127 REBATE YTD OVER 1000000
      * E593 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4130-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-ADDR = SPACES
                   MOVE 'E756' TO WS-REJECT-CODE
               WHEN ADLM-REBATE-YTD < 0
                   MOVE 'E618' TO WS-REJECT-CODE
               WHEN ADLM-REBATE-YTD > 1000000.00
                   MOVE 'E127' TO WS-REJECT-CODE
               WHEN ADLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E593' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP TIER TIER FOR THE RESERVE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4140-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-TIER-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ADLT-RESERVE-AMT NOT < WS-GAP-TIER-LOW(WS-TX)
                   MOVE WS-GAP-TIER-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4150-DESCRIBE-STATUS.
           EVALUATE ADLM-STATUS-CD
               WHEN 'EUA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HMW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RCB'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT TIERX TIER FOR THE CHARGEBK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4160-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-TIERX-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ADLM-CHARGEBK-YTD NOT < WS-PAYMENT-TIERX-LOW(WS-TX)
                   MOVE WS-PAYMENT-TIERX-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHARGEBK YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       4170-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLT-PAYMENT-AMT ROUNDED =
                   ADLM-CHARGEBK-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLT-PAYMENT-AMT
           END-IF
           ADD ADLT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE GAP TIER TIER FOR THE AMOUNT FIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4180-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-TIER-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ADLM-AMOUNT-FIN-AMT NOT < WS-GAP-TIER-LOW(WS-TX)
                   MOVE WS-GAP-TIER-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4190-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = ADLM-PAYMENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-PAYMENT-AMT - (WS-WORK-AMT * 3)
           COMPUTE ADLM-CHARGEBK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT FIN AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4200-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TYPE-CD
                   ADD ADLM-AMOUNT-FIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-AMOUNT-FIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGEBK AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4210-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TYPE-CD
                   ADD ADLM-CHARGEBK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-CHARGEBK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4220-FLAG-PAYMENT.
           IF ADLT-PAYMENT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ADLT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE REBATE YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4230-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ADLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ADLM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-PREFERRED-FLAG = 'Y'
                   COMPUTE ADLM-PAYMENT-YTD ROUNDED = ADLM-REBATE-YTD *
                       ADLM-RATE / 200
               WHEN ADLM-TIER-CD = 'EW'
                   MOVE 20.00 TO ADLM-PAYMENT-YTD
               WHEN ADLM-TIER-CD = 'JF'
                   MOVE 3.00 TO ADLM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE ADLM-PAYMENT-YTD ROUNDED = ADLM-REBATE-YTD *
                       ADLM-RATE / 100
           END-EVALUATE
           IF ADLM-PAYMENT-YTD > 0 AND ADLM-PAYMENT-YTD < 2.00
               MOVE 2.00 TO ADLM-PAYMENT-YTD
           END-IF
           IF ADLM-PAYMENT-YTD > 35.00
               MOVE 35.00 TO ADLM-PAYMENT-YTD
           END-IF
           ADD ADLM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT RESERVE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4240-SPLIT-RESERVE.
           COMPUTE WS-WORK-AMT = ADLM-RESERVE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-RESERVE-AMT - (WS-WORK-AMT * 3)
           COMPUTE ADLM-CHARGEBK-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4250-ACCRUE-RESERVE.
           IF ADLT-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-RESERVE-AMT * ADLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ADLM-RESERVE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4260-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 180 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4270-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF ADLM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4280-CHECK-NEXT-REVIEW.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NEXT-REVIEW-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NEXT-REVIEW-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ADLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4290-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ADLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESERVE AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4300-LIMIT-RESERVE.
           IF ADLM-RESERVE-AMT < 50.00
               MOVE 50.00 TO ADLM-RESERVE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-RESERVE-AMT > 5000.00
               MOVE 5000.00 TO ADLM-RESERVE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE SCHED TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4310-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-SCHED-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF ADLM-PAYMENT-YTD NOT < WS-RESERVE-SCHED-LOW(WS-TX)
                   MOVE WS-RESERVE-SCHED-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4320-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NOTICE-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE ADL FIELDS TO AURISK AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7030-CALL-AURISK.
           INITIALIZE AURISK-PARMS
           MOVE ADLM-GAP-AMT
             TO AURISK-GAP-AMT
           MOVE ADLM-AMOUNT-FIN-AMT
             TO AURISK-AMOUNT-FIN-AMT
           MOVE ADLM-CHARGEBK-AMT
             TO AURISK-CHARGEBK-AMT
           MOVE ADLM-REBATE-AMT
             TO AURISK-REBATE-AMT
           MOVE ADLM-RESERVE-AMT
             TO AURISK-RESERVE-AMT
           MOVE ADLM-PAYMENT-AMT
             TO AURISK-PAYMENT-AMT
           MOVE ADLM-NEXT-REVIEW-DATE
             TO AURISK-NEXT-REVIEW-DATE
           MOVE ADLM-EFFECTIVE-DATE
             TO AURISK-EFFECTIVE-DATE
           MOVE ADLM-LAST-STMT-DATE
             TO AURISK-LAST-STMT-DATE
           MOVE ADLM-LAST-ACT-DATE
             TO AURISK-LAST-ACT-DATE
           MOVE ADLM-STATUS-CD
             TO AURISK-STATUS-CD
           MOVE ADLM-TIER-CD
             TO AURISK-TIER-CD
           MOVE ADLM-CYCLE-CD
             TO AURISK-CYCLE-CD
           MOVE ADLM-DEALER-CD
             TO AURISK-DEALER-CD
           MOVE ADLM-REVIEW-FLAG
             TO AURISK-REVIEW-FLAG
           MOVE ADLM-EXEMPT-FLAG
             TO AURISK-EXEMPT-FLAG
           MOVE ADLM-PREFERRED-FLAG
             TO AURISK-PREFERRED-FLAG
           MOVE ADLM-PAYMENT-CNT
             TO AURISK-PAYMENT-CNT
           MOVE ADLM-RETRY-CNT
             TO AURISK-RETRY-CNT
           MOVE ADLM-RATE
             TO AURISK-RATE
           MOVE ADLM-MARGIN
             TO AURISK-MARGIN
           MOVE ADLM-REF-NO
             TO AURISK-REF-NO
           MOVE ADLM-CONTACT
             TO AURISK-CONTACT
           CALL 'AURISK' USING AURISK-PARMS
           EVALUATE AURISK-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE AURISK-CHARGEBK-AMT
                     TO ADLM-CHARGEBK-AMT
                   MOVE AURISK-RESERVE-AMT
                     TO ADLM-RESERVE-AMT
                   MOVE AURISK-REVIEW-FLAG
                     TO ADLM-REVIEW-FLAG
                   MOVE AURISK-PREFERRED-FLAG
                     TO ADLM-PREFERRED-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-AURISK' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE ADL FIELDS TO AUWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-AUWAIV.
           INITIALIZE AUWAIV-PARMS
           MOVE ADLM-PAYMENT-AMT
             TO AUWAIV-PAYMENT-AMT
           MOVE ADLM-GAP-AMT
             TO AUWAIV-GAP-AMT
           MOVE ADLM-AMOUNT-FIN-YTD
             TO AUWAIV-AMOUNT-FIN-YTD
           MOVE ADLM-PAYMENT-YTD
             TO AUWAIV-PAYMENT-YTD
           MOVE ADLM-CHARGEBK-AMT
             TO AUWAIV-CHARGEBK-AMT
           MOVE ADLM-AMOUNT-FIN-AMT
             TO AUWAIV-AMOUNT-FIN-AMT
           MOVE ADLM-NEXT-REVIEW-DATE
             TO AUWAIV-NEXT-REVIEW-DATE
           MOVE ADLM-LAST-PAY-DATE
             TO AUWAIV-LAST-PAY-DATE
           MOVE ADLM-LAST-MAINT-DATE
             TO AUWAIV-LAST-MAINT-DATE
           MOVE ADLM-NEXT-DUE-DATE
             TO AUWAIV-NEXT-DUE-DATE
           MOVE ADLM-TIER-CD
             TO AUWAIV-TIER-CD
           MOVE ADLM-CYCLE-CD
             TO AUWAIV-CYCLE-CD
           MOVE ADLM-STATUS-CD
             TO AUWAIV-STATUS-CD
           MOVE ADLM-TYPE-CD
             TO AUWAIV-TYPE-CD
           MOVE ADLM-PREFERRED-FLAG
             TO AUWAIV-PREFERRED-FLAG
           MOVE ADLM-NOTICE-SENT-FLAG
             TO AUWAIV-NOTICE-SENT-FLAG
           MOVE ADLM-EXEMPT-FLAG
             TO AUWAIV-EXEMPT-FLAG
           MOVE ADLM-PAYMENT-CNT
             TO AUWAIV-PAYMENT-CNT
           MOVE ADLM-RETRY-CNT
             TO AUWAIV-RETRY-CNT
           MOVE ADLM-FEE-PCT
             TO AUWAIV-FEE-PCT
           MOVE ADLM-RATE
             TO AUWAIV-RATE
           MOVE ADLM-CONTACT
             TO AUWAIV-CONTACT
           MOVE ADLM-NAME
             TO AUWAIV-NAME
           CALL 'AUWAIV' USING AUWAIV-PARMS
           EVALUATE AUWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE AUWAIV-PAYMENT-AMT
                     TO ADLM-PAYMENT-AMT
                   MOVE AUWAIV-CHARGEBK-AMT
                     TO ADLM-CHARGEBK-AMT
                   MOVE AUWAIV-NOTICE-SENT-FLAG
                     TO ADLM-NOTICE-SENT-FLAG
                   MOVE AUWAIV-EXEMPT-FLAG
                     TO ADLM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-AUWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE ADLT-KEY TO RD-KEY
           MOVE ADLT-TRAN-CD TO RD-CODE
           MOVE ADLT-PAYMENT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE ADLT-KEY TO RD-KEY
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
           DISPLAY 'AULNOTC READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'AULNOTC' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
