      *================================================================*
      * PROGRAM  : BPYSTMT                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : BILL PAYMENT SERVICE - STATEMENT PREPARATION. READS *
      *            THE BPY TRANSACTIONS, UPDATES THE BPY MASTER AND    *
      *            PRODUCES THE CONTROL REPORT. CONSUMER BILL PAYMENT  *
      *            PAYEE MAINTENANCE AND CHECK OR EFT DISBURSEMENT     *
      * INPUT    : BPYMST (KSDS), BPYTRN, BANKCTL                      *
      * OUTPUT   : BPYOUT, BPYSTRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, BPPRC, BPSTAT, BPELG    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-11-27 DPS  ORIGINAL                                       *
      * 1994-02-18 EWS  FEE CALCULATION CORRECTED CHG8799              *
      * 1995-03-01 RJT  PAYMENT CUTOFF MOVED REQ7722                   *
      * 1997-01-21 GAF  RETURNED CUTOFF MOVED PRB3618                  *
      * 2001-01-22 BHS  PAYMENT TABLE EXPANDED PRB0507                 *
      * 2004-08-13 SKN  RETURNED ROUNDING - HALF UP CHG9580            *
      * 2006-09-18 LMB  REFUND FEE SCHEDULE UPDATE INC2932             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BPYSTMT.
       AUTHOR. R J TRAVERS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "BPYMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS BPYM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "BPYTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "BPYOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "BPYSTRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY BPYMST.
       FD  TRAN-FILE.
       COPY BPYTRN.
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
                                           VALUE 21.000000.
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
      * WS-PAYMENT-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-PAYMENT-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000194000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000507000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000350000000664000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000450000000387500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000700000000859500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001700000000176000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011700000000637500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012700000000518500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012750000000862000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000878000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016250000000818000'.
       01  WS-PAYMENT-TIER-TBL REDEFINES WS-PAYMENT-TIER-VALUES.
           05  WS-PAYMENT-TIER-ROW OCCURS 11 TIMES.
               10  WS-PAYMENT-TIER-LOW     PIC 9(09)V99.
               10  WS-PAYMENT-TIER-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000660500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000407500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00003000000000474000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000376000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008500000000434000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011000000000039500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00013500000000348500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00014000000000218500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016500000000658500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021500000000395000'.
       01  WS-FEE-BAND-TBL REDEFINES WS-FEE-BAND-VALUES.
           05  WS-FEE-BAND-ROW OCCURS 10 TIMES.
               10  WS-FEE-BAND-LOW         PIC 9(09)V99.
               10  WS-FEE-BAND-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RETURNED-GRID - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RETURNED-GRID-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000493000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000151000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000095000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000820000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020050000000173500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022550000000764000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00027550000000215000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00037550000000733500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00038550000000133500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00039550000000528500'.
       01  WS-RETURNED-GRID-TBL REDEFINES WS-RETURNED-GRID-VALUES.
           05  WS-RETURNED-GRID-ROW OCCURS 10 TIMES.
               10  WS-RETURNED-GRID-LOW    PIC 9(09)V99.
               10  WS-RETURNED-GRID-RATE   PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  BPYSTMT  BILL PAYMENT SERVICE'.
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
       COPY BPPRCP.
       COPY BPSTATP.
       COPY BPELGP.

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
               MOVE 'BPYMST' TO ABND-FILE-NAME
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
           MOVE BPYT-KEY TO BPYM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-LIMIT-ITEM
               PERFORM 3010-PRORATE-RETURNED
               PERFORM 3020-COUNT-CYCLE
               PERFORM 3030-ACCRUE-RATE
               PERFORM 3040-DESCRIBE-CHANNEL
               PERFORM 3050-ACCRUE-FEE
               PERFORM 3060-AGE-EFF
               PERFORM 3070-ASSESS-SUSPEND
               PERFORM 3080-SPLIT-FEE
               PERFORM 3090-ACCUM-REMIT-METHOD
               PERFORM 3100-SET-SOURCE
               PERFORM 3110-EDIT-DESC
               PERFORM 3120-ACCUM-TRAN
               PERFORM 3130-SPLIT-REFUND
               PERFORM 3140-PRORATE-RETURNED
               PERFORM 3150-CHECK-LAST-ACT
               PERFORM 3160-ACCRUE-PAYMENT
               PERFORM 3170-DESCRIBE-CHANNEL
               PERFORM 3180-CHECK-NOTICE
               PERFORM 3190-SELECT-MARGIN
               PERFORM 3200-LIMIT-REFUND
               PERFORM 3210-ASSESS-EXEMPT
               PERFORM 3220-ACCRUE-REFUND
               PERFORM 3230-LIMIT-REFUND
               PERFORM 3240-ACCRUE-PENALTY-PCT
               PERFORM 3250-ACCRUE-PAYMENT
               PERFORM 3260-ACCRUE-PAYMENT
               PERFORM 3270-SET-TRAN
               PERFORM 3280-LIMIT-PAYMENT
               PERFORM 3290-PRORATE-PAYMENT
               PERFORM 3300-DESCRIBE-CYCLE
               PERFORM 3310-ASSESS-EXEMPT
               PERFORM 3320-ACCRUE-REFUND
               PERFORM 3330-PRORATE-FEE
               PERFORM 3340-COUNT-SEQ
               PERFORM 3350-EDIT-CUST-ID
               PERFORM 3360-AGE-MATURITY
               PERFORM 3370-PRORATE-FEE
               PERFORM 3380-SPLIT-FEE
               PERFORM 3390-DESCRIBE-REASON
               PERFORM 3400-ACCUM-CHANNEL
               PERFORM 3410-CHECK-EFF
               PERFORM 3420-SET-SOURCE
               PERFORM 3430-FLAG-PAYMENT
               PERFORM 3440-SET-PAYEE-TYPE
               PERFORM 3450-SELECT-RATE
               PERFORM 3460-COUNT-SEQ
               PERFORM 3470-CHECK-EFF
               PERFORM 3480-AGE-LAST-STMT
               PERFORM 3490-EDIT-REF-NO
               PERFORM 3500-FLAG-REFUND
               PERFORM 3510-FLAG-REFUND
               PERFORM 3520-PRORATE-RETURNED
               PERFORM 3530-SELECT-MARGIN
               PERFORM 3540-ACCRUE-REFUND
               PERFORM 3550-PRORATE-RETURNED
               PERFORM 3560-DESCRIBE-CHANNEL
               PERFORM 3570-CHECK-LAST-STMT
               PERFORM 3580-PRORATE-REFUND
               PERFORM 3590-AGE-POST
               PERFORM 3600-SELECT-MARGIN
               PERFORM 3610-ACCUM-CHANNEL
               PERFORM 3620-ASSESS-HOLD
               PERFORM 3630-ASSESS-REVERSAL
               PERFORM 3640-COUNT-ITEM
               PERFORM 3650-SET-STATUS
               PERFORM 3660-ACCUM-CHANNEL
               PERFORM 3670-CHECK-EXPIRY
               PERFORM 3680-ACCRUE-PENALTY-PCT
               PERFORM 3690-LIMIT-ITEM
               PERFORM 3700-DESCRIBE-STATUS
               PERFORM 3710-ASSESS-EXEMPT
               PERFORM 3720-SPLIT-RETURNED
               PERFORM 3730-AGE-EFF
               PERFORM 3740-ACCUM-TRAN
               PERFORM 3750-AGE-NOTICE
               PERFORM 3760-SELECT-RATE
               PERFORM 3770-PRORATE-PAYMENT
               PERFORM 3780-SET-STATUS
               PERFORM 3790-DESCRIBE-SOURCE
               PERFORM 3800-FLAG-ITEM
               PERFORM 3810-ACCRUE-RATE
               PERFORM 3820-PRORATE-PAYMENT
               PERFORM 3830-ACCRUE-FEE
               PERFORM 3840-LIMIT-ITEM
               PERFORM 3850-EDIT-DESC
               PERFORM 3860-AGE-OPEN
               PERFORM 3870-ACCRUE-RETURNED
               PERFORM 3880-SELECT-RATE
               PERFORM 3890-CHECK-NOTICE
               PERFORM 3900-AGE-CLOSE
               PERFORM 3910-ACCRUE-FEE
               PERFORM 3920-ACCRUE-PAYMENT
               PERFORM 3930-DESCRIBE-PAYEE-TYPE
               PERFORM 3940-ASSESS-OVERRIDE
               PERFORM 3950-PRORATE-FEE
               PERFORM 3960-ASSESS-REVERSAL
               PERFORM 3970-FLAG-PAYMENT
               PERFORM 3980-LIMIT-FEE
               PERFORM 3990-FLAG-PAYMENT
               PERFORM 4000-ASSESS-REVERSAL
               PERFORM 4010-PRORATE-FEE
               PERFORM 4020-DESCRIBE-REMIT-METHOD
               PERFORM 4030-FLAG-RETURNED
               PERFORM 4040-ACCUM-SOURCE
               PERFORM 4050-SPLIT-FEE
               PERFORM 4060-COUNT-EXCEPT
               PERFORM 4070-CHECK-OPEN
               PERFORM 4080-FLAG-FEE
               PERFORM 4090-AGE-NOTICE
               PERFORM 4100-EDIT-REJECT
               PERFORM 4110-AGE-LAST-STMT
               PERFORM 4120-COUNT-EXCEPT
               PERFORM 4130-ACCUM-SOURCE
               PERFORM 4140-FLAG-FEE
               PERFORM 4150-COUNT-SEQ
               PERFORM 4160-CHECK-EXPIRY
               PERFORM 4170-COUNT-EXCEPT
               PERFORM 4180-CHECK-EXPIRY
               PERFORM 4190-LIMIT-FEE
               PERFORM 4200-COUNT-SEQ
               PERFORM 4210-DESCRIBE-CYCLE
               PERFORM 4220-SPLIT-RETURNED
               PERFORM 4230-SET-CHANNEL
               PERFORM 4240-PRORATE-REFUND
               PERFORM 4250-ACCUM-PAYEE-TYPE
               PERFORM 4260-COUNT-SEQ
               PERFORM 4270-LIMIT-ITEM
               PERFORM 4280-SELECT-RATE
               PERFORM 4290-SELECT-PENALTY-PCT
               PERFORM 4300-SELECT-MARGIN
               PERFORM 4310-ACCRUE-PAYMENT
               PERFORM 4320-ACCUM-SOURCE
               PERFORM 4330-PRORATE-FEE
               PERFORM 4340-LIMIT-FEE
               PERFORM 4350-EDIT-REJECT
               PERFORM 4360-SPLIT-FEE
               PERFORM 4370-LIMIT-REFUND
               PERFORM 4380-ASSESS-REVERSAL
               PERFORM 4390-FLAG-PAYMENT
               PERFORM 4400-ACCUM-SOURCE
               PERFORM 4410-EDIT-OPERATOR
               PERFORM 4420-COUNT-SEQ
               PERFORM 4430-SET-CHANNEL
               PERFORM 4440-PRORATE-RETURNED
               PERFORM 4450-PRORATE-RETURNED
               PERFORM 4460-ASSESS-REVERSAL
               PERFORM 4470-SPLIT-FEE
               PERFORM 4480-EDIT-DESC
               PERFORM 4490-EDIT-REF-NO
               PERFORM 4500-AGE-POST
               PERFORM 4510-CHECK-EFF
               PERFORM 4520-AGE-EFFECTIVE
               PERFORM 4530-PRORATE-ITEM
               PERFORM 4540-PRORATE-FEE
               PERFORM 4550-SET-STATUS
               PERFORM 4560-FLAG-REFUND
               PERFORM 4570-FLAG-REFUND
               PERFORM 4580-SPLIT-REFUND
               PERFORM 4590-ACCRUE-REFUND
               PERFORM 7010-CALL-BPPRC
               PERFORM 7020-CALL-BPSTAT
               PERFORM 7000-CALL-BPELG
               REWRITE BPY-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'BPYMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM BPY-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3000-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 50000.00
               MOVE 50000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REFUND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETURNED YTD.
      *----------------------------------------------------------------*
       3010-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-YTD ROUNDED =
                   BPYM-REFUND-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-RETURNED-YTD
           END-IF
           ADD BPYM-RETURNED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-CYCLE.
           IF BPYM-CYCLE-CNT < 9999999
               ADD 1 TO BPYM-CYCLE-CNT
           END-IF
           IF BPYM-CYCLE-CNT NOT < 10
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYT-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3040-DESCRIBE-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3050-ACCRUE-FEE.
           IF BPYT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-FEE-AMT * BPYM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BPYM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3060-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED YTD FEE ON THE REFUND YTD.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3070-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BPYM-RETURNED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = BPYM-REFUND-YTD *
                       BPYM-MARGIN / 200
               WHEN BPYM-SOURCE-CD = 'CT'
                   MOVE 3.00 TO BPYM-RETURNED-YTD
               WHEN BPYM-SOURCE-CD = 'FV'
                   MOVE 10.00 TO BPYM-RETURNED-YTD
               WHEN OTHER
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = BPYM-REFUND-YTD *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYM-RETURNED-YTD > 0 AND BPYM-RETURNED-YTD < 2.00
               MOVE 2.00 TO BPYM-RETURNED-YTD
           END-IF
           IF BPYM-RETURNED-YTD > 100.00
               MOVE 100.00 TO BPYM-RETURNED-YTD
           END-IF
           ADD BPYM-RETURNED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3080-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYM-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPYM-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE BPYM-RETURNED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND YTD BY REMIT METHOD CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3090-ACCUM-REMIT-METHOD.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-REMIT-METHOD-CD
                   ADD BPYM-REFUND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-REMIT-METHOD-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-REFUND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CT -> FV WHEN OPEN DATE IS PAST
      * FV -> HD WHEN OPEN DATE IS PAST
      * HD -> KD WHEN OPEN DATE IS PAST
      * KD -> NE WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3100-SET-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FV' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FV'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HD'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E744 DESC REQUIRED
      * E058 PAYMENT AMT NEGATIVE
      * E509 PAYMENT AMT OVER 99999
      * E724 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3110-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-DESC = SPACES
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT < 0
                   MOVE 'E058' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT > 99999.00
                   MOVE 'E509' TO WS-REJECT-CODE
               WHEN BPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E724' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3120-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-TRAN-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3130-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPYM-REFUND-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BPYM-REFUND-AMT - (WS-WORK-AMT * 4)
           COMPUTE BPYT-RETURNED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETURNED YTD.
      *----------------------------------------------------------------*
       3140-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-YTD ROUNDED =
                   BPYM-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-RETURNED-YTD
           END-IF
           ADD BPYM-RETURNED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 45 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3150-CHECK-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF BPYM-LAST-ACT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3160-ACCRUE-PAYMENT.
           IF BPYT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-PAYMENT-AMT * BPYM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPYM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3170-DESCRIBE-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3180-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE PAYMENT TIER TIER FOR THE RETURNED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3190-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-PAYMENT-TIER-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 11
               IF BPYT-RETURNED-AMT NOT < WS-PAYMENT-TIER-LOW(WS-TX)
                   MOVE WS-PAYMENT-TIER-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $10.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3200-LIMIT-REFUND.
           IF BPYT-REFUND-AMT < 10.00
               MOVE 10.00 TO BPYT-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-REFUND-AMT > 500.00
               MOVE 500.00 TO BPYT-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE REFUND YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3210-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BPYM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE BPYM-PAYMENT-AMT ROUNDED = BPYM-REFUND-YTD *
                       BPYM-MARGIN / 200
               WHEN BPYM-PAYEE-TYPE-CD = 'BT'
                   MOVE 10.00 TO BPYM-PAYMENT-AMT
               WHEN BPYM-PAYEE-TYPE-CD = 'FR'
                   MOVE 12.00 TO BPYM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE BPYM-PAYMENT-AMT ROUNDED = BPYM-REFUND-YTD *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYM-PAYMENT-AMT > 0 AND BPYM-PAYMENT-AMT < 1.00
               MOVE 1.00 TO BPYM-PAYMENT-AMT
           END-IF
           IF BPYM-PAYMENT-AMT > 25.00
               MOVE 25.00 TO BPYM-PAYMENT-AMT
           END-IF
           ADD BPYM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3220-ACCRUE-REFUND.
           IF BPYT-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-REFUND-AMT * BPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BPYT-RETURNED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3230-LIMIT-REFUND.
           IF BPYT-REFUND-AMT < 10.00
               MOVE 10.00 TO BPYT-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-REFUND-AMT > 10000.00
               MOVE 10000.00 TO BPYT-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3240-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BPYM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPYT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-PAYMENT.
           IF BPYM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-PAYMENT-YTD * BPYM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3260-ACCRUE-PAYMENT.
           IF BPYT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-PAYMENT-AMT * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYM-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SC -> PD WHEN MATURITY DATE IS PAST
      * PD -> RT WHEN MATURITY DATE IS PAST
      * RT -> CN WHEN MATURITY DATE IS PAST
      * CN -> RF WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3270-SET-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RT' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RT'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3280-LIMIT-PAYMENT.
           IF BPYM-PAYMENT-YTD < 50.00
               MOVE 50.00 TO BPYM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-PAYMENT-YTD > 1000.00
               MOVE 1000.00 TO BPYM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETURNED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3290-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-PAYMENT-AMT ROUNDED =
                   BPYM-RETURNED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYT-PAYMENT-AMT
           END-IF
           ADD BPYT-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3300-DESCRIBE-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JH'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'JV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'RM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3310-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BPYM-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BPYM-REFUND-AMT ROUNDED = WS-WORK-AMT *
                       BPYM-RATE / 200
               WHEN BPYM-STATUS-CD = 'JX'
                   MOVE 5.00 TO BPYM-REFUND-AMT
               WHEN BPYM-STATUS-CD = 'LR'
                   MOVE 12.00 TO BPYM-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYM-REFUND-AMT ROUNDED = WS-WORK-AMT *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-REFUND-AMT > 0 AND BPYM-REFUND-AMT < 10.00
               MOVE 10.00 TO BPYM-REFUND-AMT
           END-IF
           IF BPYM-REFUND-AMT > 150.00
               MOVE 150.00 TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3320-ACCRUE-REFUND.
           IF BPYM-REFUND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-REFUND-YTD * BPYM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPYM-RETURNED-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REFUND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3330-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-FEE-YTD ROUNDED =
                   BPYM-REFUND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-FEE-YTD
           END-IF
           ADD BPYM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3340-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 10
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E413 CUST ID REQUIRED
      * E730 WORK AMT NEGATIVE
      * E579 WORK AMT OVER 50000
      * E277 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3350-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-CUST-ID = SPACES
                   MOVE 'E413' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E730' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E579' TO WS-REJECT-CODE
               WHEN BPYM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E277' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM MATURITY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3360-AGE-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RETURNED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       3370-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-FEE-YTD ROUNDED =
                   BPYT-RETURNED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-FEE-YTD
           END-IF
           ADD BPYM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3380-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYT-FEE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPYT-FEE-AMT - (WS-WORK-AMT * 3)
           COMPUTE BPYT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3390-DESCRIBE-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BX'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'GM'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'LT'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RR'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WJ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3400-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-CHANNEL-CD
                   ADD BPYM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 90 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3410-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF BPYT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CT -> FV WHEN CLOSE DATE IS PAST
      * FV -> HD WHEN CLOSE DATE IS PAST
      * HD -> KD WHEN CLOSE DATE IS PAST
      * KD -> NE WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3420-SET-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FV' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FV'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HD'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3430-FLAG-PAYMENT.
           IF BPYM-PAYMENT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * BT -> FR WHEN NOTICE DATE IS PAST
      * FR -> LL WHEN NOTICE DATE IS PAST
      * LL -> US WHEN NOTICE DATE IS PAST
      * US -> XM WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3440-SET-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FR' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FR'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LL' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LL'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED GRID TIER FOR THE RETURNED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3450-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-GRID-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF BPYT-RETURNED-AMT NOT < WS-RETURNED-GRID-LOW(WS-TX)
                   MOVE WS-RETURNED-GRID-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3460-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 12
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 180 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3470-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF BPYT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3480-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
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
      * E584 REF NO REQUIRED
      * E787 REFUND YTD NEGATIVE
      * E971 REFUND YTD OVER 5000
      * E095 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3490-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-REF-NO = SPACES
                   MOVE 'E584' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-YTD < 0
                   MOVE 'E787' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-YTD > 5000.00
                   MOVE 'E971' TO WS-REJECT-CODE
               WHEN BPYM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E095' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3500-FLAG-REFUND.
           IF BPYT-REFUND-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYT-REFUND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3510-FLAG-REFUND.
           IF BPYT-REFUND-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYT-REFUND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REFUND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       3520-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-RETURNED-AMT ROUNDED =
                   BPYT-REFUND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYT-RETURNED-AMT
           END-IF
           ADD BPYT-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED GRID TIER FOR THE REFUND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3530-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-GRID-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF BPYM-REFUND-YTD NOT < WS-RETURNED-GRID-LOW(WS-TX)
                   MOVE WS-RETURNED-GRID-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3540-ACCRUE-REFUND.
           IF BPYT-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-REFUND-AMT * BPYM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPYM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETURNED YTD.
      *----------------------------------------------------------------*
       3550-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-YTD ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-RETURNED-YTD
           END-IF
           ADD BPYM-RETURNED-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3560-DESCRIBE-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 120 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3570-CHECK-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPYM-LAST-STMT-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       3580-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-REFUND-AMT ROUNDED =
                   BPYM-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYT-REFUND-AMT
           END-IF
           ADD BPYT-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-REFUND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED GRID TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3600-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-GRID-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF BPYM-PAYMENT-YTD NOT < WS-RETURNED-GRID-LOW(WS-TX)
                   MOVE WS-RETURNED-GRID-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3610-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-CHANNEL-CD
                   ADD BPYT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE YTD FEE ON THE REFUND AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3620-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPYM-FEE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   COMPUTE BPYM-FEE-YTD ROUNDED = BPYT-REFUND-AMT *
                       BPYM-MARGIN / 200
               WHEN BPYM-CYCLE-CD = 'DA'
                   MOVE 12.00 TO BPYM-FEE-YTD
               WHEN BPYM-CYCLE-CD = 'FP'
                   MOVE 3.00 TO BPYM-FEE-YTD
               WHEN OTHER
                   COMPUTE BPYM-FEE-YTD ROUNDED = BPYT-REFUND-AMT *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYM-FEE-YTD > 0 AND BPYM-FEE-YTD < 5.00
               MOVE 5.00 TO BPYM-FEE-YTD
           END-IF
           IF BPYM-FEE-YTD > 250.00
               MOVE 250.00 TO BPYM-FEE-YTD
           END-IF
           ADD BPYM-FEE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND FEE ON THE WORK AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3630-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BPYM-REFUND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   COMPUTE BPYM-REFUND-AMT ROUNDED = WS-WORK-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-PAYEE-TYPE-CD = 'BT'
                   MOVE 20.00 TO BPYM-REFUND-AMT
               WHEN BPYM-PAYEE-TYPE-CD = 'FR'
                   MOVE 15.00 TO BPYM-REFUND-AMT
               WHEN OTHER
                   COMPUTE BPYM-REFUND-AMT ROUNDED = WS-WORK-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYM-REFUND-AMT > 0 AND BPYM-REFUND-AMT < 1.00
               MOVE 1.00 TO BPYM-REFUND-AMT
           END-IF
           IF BPYM-REFUND-AMT > 100.00
               MOVE 100.00 TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3640-COUNT-ITEM.
           IF BPYM-ITEM-CNT < 9999999
               ADD 1 TO BPYM-ITEM-CNT
           END-IF
           IF BPYM-ITEM-CNT NOT < 6
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN MATURITY DATE IS PAST
      * LR -> NZ WHEN MATURITY DATE IS PAST
      * NZ -> RV WHEN MATURITY DATE IS PAST
      * RV -> TM WHEN MATURITY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3650-SET-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LR' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LR'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3660-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-CHANNEL-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3670-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3680-ACCRUE-PENALTY-PCT.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * BPYM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPYM-RETURNED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3690-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3700-DESCRIBE-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'TM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3710-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BPYM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   COMPUTE BPYM-PAYMENT-YTD ROUNDED = BPYT-FEE-AMT *
                       BPYM-MARGIN / 200
               WHEN BPYM-CYCLE-CD = 'DA'
                   MOVE 3.00 TO BPYM-PAYMENT-YTD
               WHEN BPYM-CYCLE-CD = 'FP'
                   MOVE 5.00 TO BPYM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE BPYM-PAYMENT-YTD ROUNDED = BPYT-FEE-AMT *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYM-PAYMENT-YTD > 0 AND BPYM-PAYMENT-YTD < 5.00
               MOVE 5.00 TO BPYM-PAYMENT-YTD
           END-IF
           IF BPYM-PAYMENT-YTD > 150.00
               MOVE 150.00 TO BPYM-PAYMENT-YTD
           END-IF
           ADD BPYM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT RETURNED YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3720-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPYM-RETURNED-YTD / 2
           COMPUTE WS-WORK-AMT-2 = BPYM-RETURNED-YTD - (WS-WORK-AMT * 2)
           COMPUTE BPYT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3730-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-EFF-DATE TO DTU-DATE-1
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
      * ACCUMULATE PAYMENT AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3740-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-TRAN-CD
                   ADD BPYT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-RETURNED-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED GRID TIER FOR THE REFUND YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3760-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-GRID-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF BPYM-REFUND-YTD NOT < WS-RETURNED-GRID-LOW(WS-TX)
                   MOVE WS-RETURNED-GRID-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       3770-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-PAYMENT-YTD ROUNDED =
                   BPYM-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-PAYMENT-YTD
           END-IF
           ADD BPYM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN MATURITY DATE IS PAST
      * LR -> NZ WHEN MATURITY DATE IS PAST
      * NZ -> RV WHEN MATURITY DATE IS PAST
      * RV -> TM WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3780-SET-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LR' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LR'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3790-DESCRIBE-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FV'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'HD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KD'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NE'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'NJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'PS'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3800-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3810-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3820-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3830-ACCRUE-FEE.
           IF BPYM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-FEE-YTD * BPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BPYT-RETURNED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3840-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 10000.00
               MOVE 10000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E918 DESC REQUIRED
      * E961 FEE YTD NEGATIVE
      * E625 FEE YTD OVER 250000
      * E341 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-DESC = SPACES
                   MOVE 'E918' TO WS-REJECT-CODE
               WHEN BPYM-FEE-YTD < 0
                   MOVE 'E961' TO WS-REJECT-CODE
               WHEN BPYM-FEE-YTD > 250000.00
                   MOVE 'E625' TO WS-REJECT-CODE
               WHEN BPYM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E341' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3860-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETURNED AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3870-ACCRUE-RETURNED.
           IF BPYT-RETURNED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-RETURNED-AMT * BPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3880-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF BPYM-PAYMENT-YTD NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3890-CHECK-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NOTICE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3900-AGE-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3910-ACCRUE-FEE.
           IF BPYT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-FEE-AMT * BPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BPYT-RETURNED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3920-ACCRUE-PAYMENT.
           IF BPYM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-PAYMENT-YTD * BPYM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BPYT-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3930-DESCRIBE-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'LL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'US'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'XM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED YTD FEE ON THE PAYMENT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3940-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BPYM-RETURNED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-HOLD-FLAG = 'Y'
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = BPYM-PAYMENT-AMT
                       * BPYM-RATE / 200
               WHEN BPYT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO BPYM-RETURNED-YTD
               WHEN BPYT-CHANNEL-CD = 'BAT'
                   MOVE 15.00 TO BPYM-RETURNED-YTD
               WHEN OTHER
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = BPYM-PAYMENT-AMT
                       * BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-RETURNED-YTD > 0 AND BPYM-RETURNED-YTD < 1.00
               MOVE 1.00 TO BPYM-RETURNED-YTD
           END-IF
           IF BPYM-RETURNED-YTD > 25.00
               MOVE 25.00 TO BPYM-RETURNED-YTD
           END-IF
           ADD BPYM-RETURNED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3950-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-FEE-AMT ROUNDED =
                   BPYM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYT-FEE-AMT
           END-IF
           ADD BPYT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE REFUND AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3960-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BPYM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE BPYM-PAYMENT-YTD ROUNDED = BPYM-REFUND-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-REASON-CD = 'AX'
                   MOVE 5.00 TO BPYM-PAYMENT-YTD
               WHEN BPYM-REASON-CD = 'BX'
                   MOVE 5.00 TO BPYM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE BPYM-PAYMENT-YTD ROUNDED = BPYM-REFUND-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYM-PAYMENT-YTD > 0 AND BPYM-PAYMENT-YTD < 1.00
               MOVE 1.00 TO BPYM-PAYMENT-YTD
           END-IF
           IF BPYM-PAYMENT-YTD > 250.00
               MOVE 250.00 TO BPYM-PAYMENT-YTD
           END-IF
           ADD BPYM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3970-FLAG-PAYMENT.
           IF BPYM-PAYMENT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3980-LIMIT-FEE.
           IF BPYM-FEE-AMT < 0.00
               MOVE 0.00 TO BPYM-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-FEE-AMT > 2500.00
               MOVE 2500.00 TO BPYM-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3990-FLAG-PAYMENT.
           IF BPYM-PAYMENT-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED FEE ON THE WORK AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       4000-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BPYT-RETURNED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = WS-WORK-AMT *
                       BPYM-MARGIN / 200
               WHEN BPYM-SOURCE-CD = 'CT'
                   MOVE 12.00 TO BPYT-RETURNED-AMT
               WHEN BPYM-SOURCE-CD = 'FV'
                   MOVE 5.00 TO BPYT-RETURNED-AMT
               WHEN OTHER
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = WS-WORK-AMT *
                       BPYM-MARGIN / 100
           END-EVALUATE
           IF BPYT-RETURNED-AMT > 0 AND BPYT-RETURNED-AMT < 2.00
               MOVE 2.00 TO BPYT-RETURNED-AMT
           END-IF
           IF BPYT-RETURNED-AMT > 150.00
               MOVE 150.00 TO BPYT-RETURNED-AMT
           END-IF
           ADD BPYT-RETURNED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE RETURNED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       4010-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-FEE-AMT ROUNDED =
                   BPYT-RETURNED-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYT-FEE-AMT
           END-IF
           ADD BPYT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REMIT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4020-DESCRIBE-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FNS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NMS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'TAF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'VNK'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'WUA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'XCT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETURNED AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4030-FLAG-RETURNED.
           IF BPYM-RETURNED-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-RETURNED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4040-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-SOURCE-CD
                   ADD BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4050-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 12
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 12)
           COMPUTE BPYM-FEE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4060-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 365 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4070-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF BPYM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4080-FLAG-FEE.
           IF BPYM-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4090-AGE-NOTICE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NOTICE-DATE > 0 AND BPYM-NOTICE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NOTICE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E102 REJECT CD REQUIRED
      * E686 FEE AMT NEGATIVE
      * E565 FEE AMT OVER 50000
      * E544 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4100-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-REJECT-CD = SPACES
                   MOVE 'E102' TO WS-REJECT-CODE
               WHEN BPYM-FEE-AMT < 0
                   MOVE 'E686' TO WS-REJECT-CODE
               WHEN BPYM-FEE-AMT > 50000.00
                   MOVE 'E565' TO WS-REJECT-CODE
               WHEN BPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E544' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-45 AND OVER 45 DAYS.
      *----------------------------------------------------------------*
       4110-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-FEE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4120-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4130-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-SOURCE-CD
                   ADD BPYM-FEE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-FEE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4140-FLAG-FEE.
           IF BPYM-FEE-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4150-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 6
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4160-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO BPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4170-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 90 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4180-CHECK-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EXPIRY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4190-LIMIT-FEE.
           IF BPYT-FEE-AMT < 10.00
               MOVE 10.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-FEE-AMT > 5000.00
               MOVE 5000.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4200-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 3
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4210-DESCRIBE-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'JH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JV'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'YR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT RETURNED AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4220-SPLIT-RETURNED.
           COMPUTE WS-WORK-AMT = BPYM-RETURNED-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPYM-RETURNED-AMT - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4230-SET-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RETURNED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REFUND YTD.
      *----------------------------------------------------------------*
       4240-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-REFUND-YTD ROUNDED =
                   BPYM-RETURNED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-REFUND-YTD
           END-IF
           ADD BPYM-REFUND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY PAYEE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4250-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-PAYEE-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4260-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 3
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4270-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RETURNED GRID TIER FOR THE RETURNED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4280-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RETURNED-GRID-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF BPYM-RETURNED-AMT NOT < WS-RETURNED-GRID-LOW(WS-TX)
                   MOVE WS-RETURNED-GRID-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE RETURNED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4290-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO BPYM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF BPYM-RETURNED-AMT NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO BPYM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4300-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF BPYT-FEE-AMT NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4310-ACCRUE-PAYMENT.
           IF BPYM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-PAYMENT-YTD * BPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4320-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-SOURCE-CD
                   ADD BPYM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETURNED YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       4330-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-FEE-AMT ROUNDED =
                   BPYM-RETURNED-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-FEE-AMT
           END-IF
           ADD BPYM-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FEE YTD IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4340-LIMIT-FEE.
           IF BPYM-FEE-YTD < 100.00
               MOVE 100.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-FEE-YTD > 1000.00
               MOVE 1000.00 TO BPYM-FEE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E996 REJECT CD REQUIRED
      * E891 REFUND YTD NEGATIVE
      * E435 REFUND YTD OVER 5000
      * E594 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4350-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-REJECT-CD = SPACES
                   MOVE 'E996' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-YTD < 0
                   MOVE 'E891' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-YTD > 5000.00
                   MOVE 'E435' TO WS-REJECT-CODE
               WHEN BPYM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E594' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4360-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYM-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = BPYM-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE BPYM-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REFUND YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4370-LIMIT-REFUND.
           IF BPYM-REFUND-YTD < 25.00
               MOVE 25.00 TO BPYM-REFUND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-REFUND-YTD > 500.00
               MOVE 500.00 TO BPYM-REFUND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE REFUND AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4380-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BPYM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   COMPUTE BPYM-PAYMENT-AMT ROUNDED = BPYT-REFUND-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-REMIT-METHOD-CD = 'CGV'
                   MOVE 5.00 TO BPYM-PAYMENT-AMT
               WHEN BPYM-REMIT-METHOD-CD = 'FNS'
                   MOVE 10.00 TO BPYM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE BPYM-PAYMENT-AMT ROUNDED = BPYT-REFUND-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYM-PAYMENT-AMT > 0 AND BPYM-PAYMENT-AMT < 10.00
               MOVE 10.00 TO BPYM-PAYMENT-AMT
           END-IF
           IF BPYM-PAYMENT-AMT > 250.00
               MOVE 250.00 TO BPYM-PAYMENT-AMT
           END-IF
           ADD BPYM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4390-FLAG-PAYMENT.
           IF BPYM-PAYMENT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4400-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-SOURCE-CD
                   ADD BPYM-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E338 OPERATOR REQUIRED
      * E473 RETURNED YTD NEGATIVE
      * E430 RETURNED YTD OVER 99999
      * E866 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4410-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-OPERATOR = SPACES
                   MOVE 'E338' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-YTD < 0
                   MOVE 'E473' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-YTD > 99999.00
                   MOVE 'E430' TO WS-REJECT-CODE
               WHEN BPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E866' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4420-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 3
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EFFECTIVE DATE IS PAST
      * BAT -> MAL WHEN EFFECTIVE DATE IS PAST
      * MAL -> TEL WHEN EFFECTIVE DATE IS PAST
      * TEL -> ONL WHEN EFFECTIVE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4430-SET-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       4440-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-RETURNED-AMT
           END-IF
           ADD BPYM-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE REFUND YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       4450-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-AMT ROUNDED =
                   BPYM-REFUND-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-RETURNED-AMT
           END-IF
           ADD BPYM-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE WORK AMT 2.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4460-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO BPYM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE BPYM-FEE-AMT ROUNDED = WS-WORK-AMT-2 *
                       BPYM-RATE / 200
               WHEN BPYM-CYCLE-CD = 'DA'
                   MOVE 3.00 TO BPYM-FEE-AMT
               WHEN BPYM-CYCLE-CD = 'FP'
                   MOVE 7.00 TO BPYM-FEE-AMT
               WHEN OTHER
                   COMPUTE BPYM-FEE-AMT ROUNDED = WS-WORK-AMT-2 *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-FEE-AMT > 0 AND BPYM-FEE-AMT < 1.00
               MOVE 1.00 TO BPYM-FEE-AMT
           END-IF
           IF BPYM-FEE-AMT > 35.00
               MOVE 35.00 TO BPYM-FEE-AMT
           END-IF
           ADD BPYM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FEE AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4470-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYT-FEE-AMT / 4
           COMPUTE WS-WORK-AMT-2 = BPYT-FEE-AMT - (WS-WORK-AMT * 4)
           COMPUTE BPYM-RETURNED-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E738 DESC REQUIRED
      * E367 RETURNED AMT NEGATIVE
      * E874 RETURNED AMT OVER 50000
      * E387 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4480-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-DESC = SPACES
                   MOVE 'E738' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-AMT < 0
                   MOVE 'E367' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-AMT > 50000.00
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN BPYM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E387' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E684 REF NO REQUIRED
      * E885 FEE AMT NEGATIVE
      * E546 FEE AMT OVER 5000
      * E603 CLOSE DATE IN FUTURE
      *----------------------------------------------------------------*
       4490-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-REF-NO = SPACES
                   MOVE 'E684' TO WS-REJECT-CODE
               WHEN BPYT-FEE-AMT < 0
                   MOVE 'E885' TO WS-REJECT-CODE
               WHEN BPYT-FEE-AMT > 5000.00
                   MOVE 'E546' TO WS-REJECT-CODE
               WHEN BPYM-CLOSE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E603' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4500-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF BPYT-POST-DATE > 0 AND BPYT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-RETURNED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4510-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF BPYT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4520-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4530-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE YTD.
      *----------------------------------------------------------------*
       4540-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-FEE-YTD ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-FEE-YTD
           END-IF
           ADD BPYM-FEE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN LAST STMT DATE IS PAST
      * LR -> NZ WHEN LAST STMT DATE IS PAST
      * NZ -> RV WHEN LAST STMT DATE IS PAST
      * RV -> TM WHEN LAST STMT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4550-SET-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LR' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LR'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF BPYM-LAST-STMT-DATE > 0 AND BPYM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REFUND AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4560-FLAG-REFUND.
           IF BPYT-REFUND-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYT-REFUND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * REFUND YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4570-FLAG-REFUND.
           IF BPYM-REFUND-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-REFUND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REFUND YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4580-SPLIT-REFUND.
           COMPUTE WS-WORK-AMT = BPYM-REFUND-YTD / 2
           COMPUTE WS-WORK-AMT-2 = BPYM-REFUND-YTD - (WS-WORK-AMT * 2)
           COMPUTE BPYT-REFUND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND YTD AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4590-ACCRUE-REFUND.
           IF BPYM-REFUND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-REFUND-YTD * BPYM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPYM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE BPY FIELDS TO BPPRC AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-BPPRC.
           INITIALIZE BPPRC-PARMS
           MOVE BPYM-FEE-AMT
             TO BPPRC-FEE-AMT
           MOVE BPYM-PAYMENT-AMT
             TO BPPRC-PAYMENT-AMT
           MOVE BPYM-PAYMENT-YTD
             TO BPPRC-PAYMENT-YTD
           MOVE BPYM-REFUND-YTD
             TO BPPRC-REFUND-YTD
           MOVE BPYM-RETURNED-YTD
             TO BPPRC-RETURNED-YTD
           MOVE BPYM-FEE-YTD
             TO BPPRC-FEE-YTD
           MOVE BPYM-EXPIRY-DATE
             TO BPPRC-EXPIRY-DATE
           MOVE BPYM-LAST-ACT-DATE
             TO BPPRC-LAST-ACT-DATE
           MOVE BPYM-EFFECTIVE-DATE
             TO BPPRC-EFFECTIVE-DATE
           MOVE BPYM-NEXT-DUE-DATE
             TO BPPRC-NEXT-DUE-DATE
           MOVE BPYM-SOURCE-CD
             TO BPPRC-SOURCE-CD
           MOVE BPYM-REMIT-METHOD-CD
             TO BPPRC-REMIT-METHOD-CD
           MOVE BPYM-REASON-CD
             TO BPPRC-REASON-CD
           MOVE BPYM-STATUS-CD
             TO BPPRC-STATUS-CD
           MOVE BPYM-SUSPEND-FLAG
             TO BPPRC-SUSPEND-FLAG
           MOVE BPYM-OVERRIDE-FLAG
             TO BPPRC-OVERRIDE-FLAG
           MOVE BPYM-HOLD-FLAG
             TO BPPRC-HOLD-FLAG
           MOVE BPYM-EXCEPT-CNT
             TO BPPRC-EXCEPT-CNT
           MOVE BPYM-CYCLE-CNT
             TO BPPRC-CYCLE-CNT
           MOVE BPYM-MARGIN
             TO BPPRC-MARGIN
           MOVE BPYM-PENALTY-PCT
             TO BPPRC-PENALTY-PCT
           MOVE BPYM-ADDR
             TO BPPRC-ADDR
           MOVE BPYM-NAME
             TO BPPRC-NAME
           CALL 'BPPRC' USING BPPRC-PARMS
           EVALUATE BPPRC-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BPPRC-REFUND-YTD
                     TO BPYM-REFUND-YTD
                   MOVE BPPRC-RETURNED-YTD
                     TO BPYM-RETURNED-YTD
                   MOVE BPPRC-FEE-YTD
                     TO BPYM-FEE-YTD
                   MOVE BPPRC-HOLD-FLAG
                     TO BPYM-HOLD-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RPRC' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-BPPRC' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE BPY FIELDS TO BPSTAT AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-BPSTAT.
           INITIALIZE BPSTAT-PARMS
           MOVE BPYM-PAYMENT-AMT
             TO BPSTAT-PAYMENT-AMT
           MOVE BPYM-RETURNED-AMT
             TO BPSTAT-RETURNED-AMT
           MOVE BPYM-REFUND-AMT
             TO BPSTAT-REFUND-AMT
           MOVE BPYM-PAYMENT-YTD
             TO BPSTAT-PAYMENT-YTD
           MOVE BPYM-RETURNED-YTD
             TO BPSTAT-RETURNED-YTD
           MOVE BPYM-FEE-AMT
             TO BPSTAT-FEE-AMT
           MOVE BPYM-OPEN-DATE
             TO BPSTAT-OPEN-DATE
           MOVE BPYM-MATURITY-DATE
             TO BPSTAT-MATURITY-DATE
           MOVE BPYM-LAST-STMT-DATE
             TO BPSTAT-LAST-STMT-DATE
           MOVE BPYM-CLOSE-DATE
             TO BPSTAT-CLOSE-DATE
           MOVE BPYM-CYCLE-CD
             TO BPSTAT-CYCLE-CD
           MOVE BPYM-REMIT-METHOD-CD
             TO BPSTAT-REMIT-METHOD-CD
           MOVE BPYM-STATUS-CD
             TO BPSTAT-STATUS-CD
           MOVE BPYM-REASON-CD
             TO BPSTAT-REASON-CD
           MOVE BPYM-AUTO-RENEW-FLAG
             TO BPSTAT-AUTO-RENEW-FLAG
           MOVE BPYM-EXEMPT-FLAG
             TO BPSTAT-EXEMPT-FLAG
           MOVE BPYM-SUSPEND-FLAG
             TO BPSTAT-SUSPEND-FLAG
           MOVE BPYM-PAYMENT-CNT
             TO BPSTAT-PAYMENT-CNT
           MOVE BPYM-EXCEPT-CNT
             TO BPSTAT-EXCEPT-CNT
           MOVE BPYM-PENALTY-PCT
             TO BPSTAT-PENALTY-PCT
           MOVE BPYM-RATE
             TO BPSTAT-RATE
           MOVE BPYM-REF-NO
             TO BPSTAT-REF-NO
           MOVE BPYM-CUST-ID
             TO BPSTAT-CUST-ID
           CALL 'BPSTAT' USING BPSTAT-PARMS
           EVALUATE BPSTAT-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BPSTAT-RETURNED-AMT
                     TO BPYM-RETURNED-AMT
                   MOVE BPSTAT-REFUND-AMT
                     TO BPYM-REFUND-AMT
                   MOVE BPSTAT-RETURNED-YTD
                     TO BPYM-RETURNED-YTD
                   MOVE BPSTAT-EXEMPT-FLAG
                     TO BPYM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RTAT' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-BPSTAT' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE BPY FIELDS TO BPELG AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-BPELG.
           INITIALIZE BPELG-PARMS
           MOVE BPYM-PAYMENT-YTD
             TO BPELG-PAYMENT-YTD
           MOVE BPYM-FEE-YTD
             TO BPELG-FEE-YTD
           MOVE BPYM-REFUND-YTD
             TO BPELG-REFUND-YTD
           MOVE BPYM-FEE-AMT
             TO BPELG-FEE-AMT
           MOVE BPYM-PAYMENT-AMT
             TO BPELG-PAYMENT-AMT
           MOVE BPYM-REFUND-AMT
             TO BPELG-REFUND-AMT
           MOVE BPYM-EXPIRY-DATE
             TO BPELG-EXPIRY-DATE
           MOVE BPYM-LAST-STMT-DATE
             TO BPELG-LAST-STMT-DATE
           MOVE BPYM-NEXT-DUE-DATE
             TO BPELG-NEXT-DUE-DATE
           MOVE BPYM-CLOSE-DATE
             TO BPELG-CLOSE-DATE
           MOVE BPYM-CYCLE-CD
             TO BPELG-CYCLE-CD
           MOVE BPYM-STATUS-CD
             TO BPELG-STATUS-CD
           MOVE BPYM-REMIT-METHOD-CD
             TO BPELG-REMIT-METHOD-CD
           MOVE BPYM-PAYEE-TYPE-CD
             TO BPELG-PAYEE-TYPE-CD
           MOVE BPYM-OVERRIDE-FLAG
             TO BPELG-OVERRIDE-FLAG
           MOVE BPYM-HOLD-FLAG
             TO BPELG-HOLD-FLAG
           MOVE BPYM-SUSPEND-FLAG
             TO BPELG-SUSPEND-FLAG
           MOVE BPYM-EXCEPT-CNT
             TO BPELG-EXCEPT-CNT
           MOVE BPYM-CYCLE-CNT
             TO BPELG-CYCLE-CNT
           MOVE BPYM-RATE
             TO BPELG-RATE
           MOVE BPYM-MARGIN
             TO BPELG-MARGIN
           MOVE BPYM-ADDR
             TO BPELG-ADDR
           MOVE BPYM-REF-NO
             TO BPELG-REF-NO
           CALL 'BPELG' USING BPELG-PARMS
           EVALUATE BPELG-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE BPELG-PAYMENT-YTD
                     TO BPYM-PAYMENT-YTD
                   MOVE BPELG-FEE-AMT
                     TO BPYM-FEE-AMT
                   MOVE BPELG-PAYMENT-AMT
                     TO BPYM-PAYMENT-AMT
                   MOVE BPELG-REFUND-AMT
                     TO BPYM-REFUND-AMT
                   CONTINUE
               WHEN 08
                   MOVE 'RELG' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-BPELG' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE BPYT-KEY TO RD-KEY
           MOVE BPYT-TRAN-CD TO RD-CODE
           MOVE BPYT-PAYMENT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE BPYT-KEY TO RD-KEY
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
           DISPLAY 'BPYSTMT READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'BPYSTMT' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
