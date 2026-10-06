      *================================================================*
      * PROGRAM  : AULRNEW                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DEALER AUTO FINANCE - RENEWAL. READS THE ADL        *
      *            TRANSACTIONS, UPDATES THE ADL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. INDIRECT AUTO DEALER CONTRACTS, *
      *            RESERVE AND CHARGEBACK                              *
      * INPUT    : ADLMST (KSDS), ADLTRN, BANKCTL                      *
      * OUTPUT   : ADLOUT, AULRNRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, AURISK                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-03-12 MKL  ORIGINAL                                       *
      * 2000-10-22 GAF  CHARGEBK CUTOFF MOVED CHG4156                  *
      * 2001-03-11 EWS  RESERVE CUTOFF MOVED REQ5829                   *
      * 2002-11-14 GAF  CHARGEBK CALCULATION CORRECTED REQ2655         *
      * 2003-02-08 JLM  GAP ROUNDING - HALF UP REQ3397                 *
      * 2005-01-14 SKN  RESERVE RERUN PROTECTION CHG5006               *
      * 2006-11-09 BHS  REBATE FEE SCHEDULE UPDATE CHG0259             *
      * 2006-01-16 MKL  REBATE STATUS CODES ADDED INC1786              *
      * 2009-02-17 EWS  GAP FEE SCHEDULE UPDATE INC8277                *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AULRNEW.
       AUTHOR. D P SHAW.

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
           SELECT REPORT-FILE ASSIGN TO "AULRNRPT"
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
      * WS-GAP-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-GAP-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000291500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000709500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020000000000224000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020100000000855000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021100000000769000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00021350000000192500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00023850000000100000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00024350000000161000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026850000000718000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00026950000000009000'.
       01  WS-GAP-TIER-TBL REDEFINES WS-GAP-TIER-VALUES.
           05  WS-GAP-TIER-ROW OCCURS 10 TIMES.
               10  WS-GAP-TIER-LOW         PIC 9(09)V99.
               10  WS-GAP-TIER-RATE        PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-RESERVE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RESERVE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000499000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000250000000294500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001250000000594500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001350000000415000'.
       01  WS-RESERVE-BAND-TBL REDEFINES WS-RESERVE-BAND-VALUES.
           05  WS-RESERVE-BAND-ROW OCCURS 4 TIMES.
               10  WS-RESERVE-BAND-LOW     PIC 9(09)V99.
               10  WS-RESERVE-BAND-RATE    PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  AULRNEW  DEALER AUTO FINANCE'.
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
               PERFORM 3000-LIMIT-AMOUNT-FIN
               PERFORM 3010-ASSESS-STOP
               PERFORM 3020-PRORATE-CHARGEBK
               PERFORM 3030-FLAG-RESERVE
               PERFORM 3040-SPLIT-CHARGEBK
               PERFORM 3050-SPLIT-CHARGEBK
               PERFORM 3060-ACCRUE-REBATE
               PERFORM 3070-SELECT-RATE
               PERFORM 3080-SET-TRAN
               PERFORM 3090-ACCRUE-RESERVE
               PERFORM 3100-DESCRIBE-CHANNEL
               PERFORM 3110-EDIT-DESC
               PERFORM 3120-ACCRUE-PAYMENT
               PERFORM 3130-SELECT-RATE
               PERFORM 3140-ACCRUE-PAYMENT
               PERFORM 3150-COUNT-EXCEPT
               PERFORM 3160-ACCUM-REASON
               PERFORM 3170-AGE-NEXT-REVIEW
               PERFORM 3180-ASSESS-STOP
               PERFORM 3190-ACCRUE-CHARGEBK
               PERFORM 3200-SPLIT-CHARGEBK
               PERFORM 3210-PRORATE-REBATE
               PERFORM 3220-AGE-LAST-ACT
               PERFORM 3230-SPLIT-REBATE
               PERFORM 3240-SELECT-FEE-PCT
               PERFORM 3250-ACCRUE-PAYMENT
               PERFORM 3260-AGE-EFFECTIVE
               PERFORM 3270-SELECT-RATE
               PERFORM 3280-ACCUM-DEALER
               PERFORM 3290-ACCRUE-PAYMENT
               PERFORM 3300-AGE-EXPIRY
               PERFORM 3310-CHECK-NOTICE
               PERFORM 3320-AGE-LAST-MAINT
               PERFORM 3330-SELECT-MARGIN
               PERFORM 3340-CHECK-LAST-ACT
               PERFORM 3350-PRORATE-AMOUNT-FIN
               PERFORM 3360-ACCRUE-RATE
               PERFORM 3370-ACCRUE-AMOUNT-FIN
               PERFORM 3380-SELECT-RATE
               PERFORM 3390-FLAG-PAYMENT
               PERFORM 3400-SELECT-MARGIN
               PERFORM 3410-PRORATE-GAP
               PERFORM 3420-FLAG-REBATE
               PERFORM 3430-LIMIT-CHARGEBK
               PERFORM 3440-ACCRUE-RESERVE
               PERFORM 3450-CHECK-NOTICE
               PERFORM 3460-COUNT-CYCLE
               PERFORM 3470-FLAG-AMOUNT-FIN
               PERFORM 3480-ACCUM-TRAN
               PERFORM 3490-SPLIT-CHARGEBK
               PERFORM 3500-DESCRIBE-DEALER
               PERFORM 3510-LIMIT-AMOUNT-FIN
               PERFORM 3520-SELECT-MARGIN
               PERFORM 3530-SELECT-FEE-PCT
               PERFORM 3540-ACCRUE-AMOUNT-FIN
               PERFORM 3550-CHECK-LAST-PAY
               PERFORM 3560-ACCUM-REASON
               PERFORM 3570-ACCUM-TIER
               PERFORM 3580-SELECT-MARGIN
               PERFORM 3590-AGE-LAST-STMT
               PERFORM 3600-ACCRUE-REBATE
               PERFORM 3610-ASSESS-REVERSAL
               PERFORM 3620-DESCRIBE-CHANNEL
               PERFORM 3630-SELECT-RATE
               PERFORM 3640-SET-DEALER
               PERFORM 3650-CHECK-LAST-ACT
               PERFORM 3660-PRORATE-PAYMENT
               PERFORM 3670-DESCRIBE-DEALER
               PERFORM 3680-AGE-NEXT-DUE
               PERFORM 3690-SET-TRAN
               PERFORM 3700-SET-TIER
               PERFORM 3710-AGE-LAST-STMT
               PERFORM 3720-SPLIT-GAP
               PERFORM 3730-DESCRIBE-TYPE
               PERFORM 3740-AGE-LAST-STMT
               PERFORM 3750-ACCRUE-AMOUNT-FIN
               PERFORM 3760-ACCRUE-GAP
               PERFORM 3770-SELECT-MARGIN
               PERFORM 3780-ACCUM-STATUS
               PERFORM 3790-SELECT-RATE
               PERFORM 3800-SET-STATUS
               PERFORM 3810-LIMIT-ITEM
               PERFORM 3820-ACCRUE-AMOUNT-FIN
               PERFORM 3830-SELECT-FEE-PCT
               PERFORM 3840-ACCRUE-GAP
               PERFORM 3850-FLAG-AMOUNT-FIN
               PERFORM 3860-SPLIT-GAP
               PERFORM 3870-DESCRIBE-CYCLE
               PERFORM 3880-AGE-POST
               PERFORM 3890-PRORATE-RESERVE
               PERFORM 3900-ASSESS-STOP
               PERFORM 3910-SET-CYCLE
               PERFORM 3920-ACCRUE-CHARGEBK
               PERFORM 3930-PRORATE-PAYMENT
               PERFORM 3940-ASSESS-EXEMPT
               PERFORM 3950-SELECT-MARGIN
               PERFORM 3960-EDIT-OPERATOR
               PERFORM 3970-SET-CYCLE
               PERFORM 3980-COUNT-RETRY
               PERFORM 3990-FLAG-AMOUNT-FIN
               PERFORM 4000-AGE-LAST-MAINT
               PERFORM 4010-COUNT-SEQ
               PERFORM 4020-LIMIT-PAYMENT
               PERFORM 4030-SELECT-MARGIN
               PERFORM 4040-SPLIT-RESERVE
               PERFORM 4050-LIMIT-CHARGEBK
               PERFORM 4060-AGE-EFFECTIVE
               PERFORM 4070-ACCRUE-RESERVE
               PERFORM 4080-AGE-EXPIRY
               PERFORM 4090-LIMIT-GAP
               PERFORM 4100-DESCRIBE-TRAN
               PERFORM 4110-EDIT-OPERATOR
               PERFORM 4120-ASSESS-PREFERRED
               PERFORM 4130-SELECT-RATE
               PERFORM 4140-CHECK-LAST-PAY
               PERFORM 4150-ASSESS-OVERRIDE
               PERFORM 4160-FLAG-AMOUNT-FIN
               PERFORM 4170-SET-STATUS
               PERFORM 4180-PRORATE-PAYMENT
               PERFORM 4190-SET-STATUS
               PERFORM 4200-FLAG-REBATE
               PERFORM 4210-LIMIT-CHARGEBK
               PERFORM 4220-FLAG-ITEM
               PERFORM 4230-PRORATE-PAYMENT
               PERFORM 4240-ACCRUE-RESERVE
               PERFORM 4250-SET-CHANNEL
               PERFORM 4260-SPLIT-PAYMENT
               PERFORM 4270-AGE-NEXT-REVIEW
               PERFORM 4280-COUNT-SEQ
               PERFORM 4290-LIMIT-RESERVE
               PERFORM 4300-FLAG-ITEM
               PERFORM 4310-EDIT-CUST-ID
               PERFORM 4320-ACCUM-TIER
               PERFORM 4330-ACCRUE-MARGIN
               PERFORM 4340-SET-CHANNEL
               PERFORM 4350-COUNT-EXCEPT
               PERFORM 4360-ACCUM-STATUS
               PERFORM 4370-COUNT-PAYMENT
               PERFORM 4380-CHECK-EFF
               PERFORM 4390-LIMIT-CHARGEBK
               PERFORM 4400-SPLIT-REBATE
               PERFORM 4410-SET-TYPE
               PERFORM 4420-LIMIT-REBATE
               PERFORM 4430-COUNT-RETRY
               PERFORM 4440-ACCUM-CYCLE
               PERFORM 4450-EDIT-OPERATOR
               PERFORM 4460-DESCRIBE-CHANNEL
               PERFORM 4470-AGE-EFFECTIVE
               PERFORM 4480-EDIT-MEMO
               PERFORM 4490-SET-TRAN
               PERFORM 4500-ASSESS-REVERSAL
               PERFORM 4510-FLAG-RESERVE
               PERFORM 4520-COUNT-EXCEPT
               PERFORM 4530-CHECK-EXPIRY
               PERFORM 4540-AGE-NEXT-REVIEW
               PERFORM 4550-LIMIT-GAP
               PERFORM 4560-DESCRIBE-DEALER
               PERFORM 4570-ASSESS-REVIEW
               PERFORM 4580-SET-STATUS
               PERFORM 4590-SELECT-RATE
               PERFORM 4600-SPLIT-GAP
               PERFORM 4610-ACCRUE-RESERVE
               PERFORM 4620-FLAG-REBATE
               PERFORM 4630-AGE-EFFECTIVE
               PERFORM 4640-SET-TIER
               PERFORM 4650-PRORATE-CHARGEBK
               PERFORM 4660-AGE-EFFECTIVE
               PERFORM 4670-SET-CYCLE
               PERFORM 4680-ACCUM-TIER
               PERFORM 4690-SPLIT-REBATE
               PERFORM 4700-SELECT-RATE
               PERFORM 4710-COUNT-EXCEPT
               PERFORM 4720-PRORATE-RESERVE
               PERFORM 4730-EDIT-ADDR
               PERFORM 4740-SPLIT-PAYMENT
               PERFORM 4750-ACCRUE-PAYMENT
               PERFORM 4760-COUNT-RETRY
               PERFORM 4770-SET-TIER
               PERFORM 4780-EDIT-CONTACT
               PERFORM 4790-ASSESS-REVERSAL
               PERFORM 7030-CALL-AURISK
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
      * AMOUNT FIN YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3000-LIMIT-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-YTD < 25.00
               MOVE 25.00 TO ADLM-AMOUNT-FIN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-AMOUNT-FIN-YTD > 5000.00
               MOVE 5000.00 TO ADLM-AMOUNT-FIN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE GAP AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3010-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ADLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ADLM-GAP-AMT *
                       ADLM-MARGIN / 200
               WHEN ADLM-STATUS-CD = 'EUA'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN ADLM-STATUS-CD = 'HMW'
                   MOVE 3.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ADLM-GAP-AMT *
                       ADLM-MARGIN / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 10.00
               MOVE 10.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE GAP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CHARGEBK YTD.
      *----------------------------------------------------------------*
       3020-PRORATE-CHARGEBK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-CHARGEBK-YTD ROUNDED =
                   ADLM-GAP-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-CHARGEBK-YTD
           END-IF
           ADD ADLM-CHARGEBK-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RESERVE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3030-FLAG-RESERVE.
           IF ADLM-RESERVE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ADLM-RESERVE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHARGEBK AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-CHARGEBK.
           COMPUTE WS-WORK-AMT = ADLM-CHARGEBK-AMT / 4
           COMPUTE WS-WORK-AMT-2 = ADLM-CHARGEBK-AMT - (WS-WORK-AMT * 4)
           COMPUTE ADLM-AMOUNT-FIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT 2 INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3050-SPLIT-CHARGEBK.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT-2 / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT-2 - (WS-WORK-AMT * 2)
           COMPUTE ADLM-CHARGEBK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REBATE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-REBATE.
           IF ADLM-REBATE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-REBATE-AMT * ADLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ADLM-AMOUNT-FIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE BAND TIER FOR THE GAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3070-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLM-GAP-AMT NOT < WS-RESERVE-BAND-LOW(WS-TX)
                   MOVE WS-RESERVE-BAND-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BK -> PY WHEN EFFECTIVE DATE IS PAST
      * PY -> CB WHEN EFFECTIVE DATE IS PAST
      * CB -> RB WHEN EFFECTIVE DATE IS PAST
      * RB -> GP WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3080-SET-TRAN.
           EVALUATE ADLT-TRAN-CD
               WHEN 'BK'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CB' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CB'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GP' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3090-ACCRUE-RESERVE.
           IF ADLT-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-RESERVE-AMT * ADLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ADLM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3100-DESCRIBE-CHANNEL.
           EVALUATE ADLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'REVIEW' TO WS-DESC
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
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E680 DESC REQUIRED
      * E215 RESERVE YTD NEGATIVE
      * E634 RESERVE YTD OVER 99999
      * E364 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3110-EDIT-DESC.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-DESC = SPACES
                   MOVE 'E680' TO WS-REJECT-CODE
               WHEN ADLM-RESERVE-YTD < 0
                   MOVE 'E215' TO WS-REJECT-CODE
               WHEN ADLM-RESERVE-YTD > 99999.00
                   MOVE 'E634' TO WS-REJECT-CODE
               WHEN ADLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E364' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3120-ACCRUE-PAYMENT.
           IF ADLM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-PAYMENT-AMT * ADLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO ADLT-RESERVE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP TIER TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3130-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-TIER-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ADLM-REBATE-AMT NOT < WS-GAP-TIER-LOW(WS-TX)
                   MOVE WS-GAP-TIER-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT YTD AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3140-ACCRUE-PAYMENT.
           IF ADLM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-PAYMENT-YTD * ADLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO ADLM-AMOUNT-FIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3150-COUNT-EXCEPT.
           IF ADLM-EXCEPT-CNT < 9999999
               ADD 1 TO ADLM-EXCEPT-CNT
           END-IF
           IF ADLM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO ADLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE CHARGEBK YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-REASON-CD
                   ADD ADLM-CHARGEBK-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-CHARGEBK-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3170-AGE-NEXT-REVIEW.
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
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CHARGEBK YTD FEE ON THE PAYMENT AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3180-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ADLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO ADLM-CHARGEBK-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ADLM-CHARGEBK-YTD ROUNDED = ADLT-PAYMENT-AMT
                       * ADLM-MARGIN / 200
               WHEN ADLM-TYPE-CD = 'B'
                   MOVE 10.00 TO ADLM-CHARGEBK-YTD
               WHEN ADLM-TYPE-CD = 'H'
                   MOVE 20.00 TO ADLM-CHARGEBK-YTD
               WHEN OTHER
                   COMPUTE ADLM-CHARGEBK-YTD ROUNDED = ADLT-PAYMENT-AMT
                       * ADLM-MARGIN / 100
           END-EVALUATE
           IF ADLM-CHARGEBK-YTD > 0 AND ADLM-CHARGEBK-YTD < 10.00
               MOVE 10.00 TO ADLM-CHARGEBK-YTD
           END-IF
           IF ADLM-CHARGEBK-YTD > 100.00
               MOVE 100.00 TO ADLM-CHARGEBK-YTD
           END-IF
           ADD ADLM-CHARGEBK-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGEBK AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3190-ACCRUE-CHARGEBK.
           IF ADLM-CHARGEBK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-CHARGEBK-AMT * ADLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO ADLM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHARGEBK YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3200-SPLIT-CHARGEBK.
           COMPUTE WS-WORK-AMT = ADLM-CHARGEBK-YTD / 12
           COMPUTE WS-WORK-AMT-2 = ADLM-CHARGEBK-YTD - (WS-WORK-AMT *
               12)
           COMPUTE ADLT-AMOUNT-FIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GAP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REBATE AMT.
      *----------------------------------------------------------------*
       3210-PRORATE-REBATE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-REBATE-AMT ROUNDED =
                   ADLM-GAP-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-REBATE-AMT
           END-IF
           ADD ADLM-REBATE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3220-AGE-LAST-ACT.
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
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT REBATE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3230-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = ADLM-REBATE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ADLM-REBATE-AMT - (WS-WORK-AMT * 12)
           COMPUTE ADLM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP TIER TIER FOR THE REBATE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3240-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-TIER-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ADLM-REBATE-YTD NOT < WS-GAP-TIER-LOW(WS-TX)
                   MOVE WS-GAP-TIER-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3250-ACCRUE-PAYMENT.
           IF ADLT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-PAYMENT-AMT * ADLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ADLM-GAP-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3260-AGE-EFFECTIVE.
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
                   ADD ADLM-AMOUNT-FIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-AMOUNT-FIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-AMOUNT-FIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-AMOUNT-FIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-AMOUNT-FIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE GAP TIER TIER FOR THE AMOUNT FIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3270-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-TIER-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ADLM-AMOUNT-FIN-YTD NOT < WS-GAP-TIER-LOW(WS-TX)
                   MOVE WS-GAP-TIER-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE AMT BY DEALER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3280-ACCUM-DEALER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-DEALER-CD
                   ADD ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-DEALER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3290-ACCRUE-PAYMENT.
           IF ADLM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-PAYMENT-AMT * ADLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       3300-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-RESERVE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ADLM-RESERVE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ADLM-RESERVE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-RESERVE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-RESERVE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3310-CHECK-NOTICE.
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
           IF WS-DAYS > 90
               MOVE 'Y' TO ADLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3320-AGE-LAST-MAINT.
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
               WHEN WS-DAYS NOT > 30
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3330-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT NOT < WS-RESERVE-BAND-LOW(WS-TX)
                   MOVE WS-RESERVE-BAND-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3340-CHECK-LAST-ACT.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT FIN AMT.
      *----------------------------------------------------------------*
       3350-PRORATE-AMOUNT-FIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLT-AMOUNT-FIN-AMT ROUNDED =
                   ADLM-PAYMENT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLT-AMOUNT-FIN-AMT
           END-IF
           ADD ADLT-AMOUNT-FIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3360-ACCRUE-RATE.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ADLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ADLM-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT FIN AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3370-ACCRUE-AMOUNT-FIN.
           IF ADLT-AMOUNT-FIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-AMOUNT-FIN-AMT * ADLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ADLT-RESERVE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP TIER TIER FOR THE REBATE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3380-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-TIER-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ADLM-REBATE-YTD NOT < WS-GAP-TIER-LOW(WS-TX)
                   MOVE WS-GAP-TIER-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3390-FLAG-PAYMENT.
           IF ADLM-PAYMENT-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ADLM-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE BAND TIER FOR THE AMOUNT FIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3400-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLT-AMOUNT-FIN-AMT NOT < WS-RESERVE-BAND-LOW(WS-TX)
                   MOVE WS-RESERVE-BAND-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GAP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO GAP AMT.
      *----------------------------------------------------------------*
       3410-PRORATE-GAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-GAP-AMT ROUNDED =
                   ADLM-GAP-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-GAP-AMT
           END-IF
           ADD ADLM-GAP-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * REBATE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3420-FLAG-REBATE.
           IF ADLM-REBATE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ADLM-REBATE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK YTD IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3430-LIMIT-CHARGEBK.
           IF ADLM-CHARGEBK-YTD < 0.00
               MOVE 0.00 TO ADLM-CHARGEBK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-CHARGEBK-YTD > 2500.00
               MOVE 2500.00 TO ADLM-CHARGEBK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3440-ACCRUE-RESERVE.
           IF ADLT-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-RESERVE-AMT * ADLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ADLM-RESERVE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NOTICE DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-NOTICE.
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
               MOVE 'Y' TO ADLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3460-COUNT-CYCLE.
           IF ADLM-CYCLE-CNT < 9999999
               ADD 1 TO ADLM-CYCLE-CNT
           END-IF
           IF ADLM-CYCLE-CNT NOT < 5
               MOVE 'Y' TO ADLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3470-FLAG-AMOUNT-FIN.
           IF ADLT-AMOUNT-FIN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ADLT-AMOUNT-FIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT FIN YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3480-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLT-TRAN-CD
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-AMOUNT-FIN-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHARGEBK YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3490-SPLIT-CHARGEBK.
           COMPUTE WS-WORK-AMT = ADLM-CHARGEBK-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ADLM-CHARGEBK-YTD - (WS-WORK-AMT * 4)
           COMPUTE ADLM-AMOUNT-FIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DEALER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3500-DESCRIBE-DEALER.
           EVALUATE ADLM-DEALER-CD
               WHEN 'DRU'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'FNF'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GBX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GTU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JNE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KTP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3510-LIMIT-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-AMT < 25.00
               MOVE 25.00 TO ADLM-AMOUNT-FIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-AMOUNT-FIN-AMT > 10000.00
               MOVE 10000.00 TO ADLM-AMOUNT-FIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE BAND TIER FOR THE RESERVE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3520-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLM-RESERVE-YTD NOT < WS-RESERVE-BAND-LOW(WS-TX)
                   MOVE WS-RESERVE-BAND-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE BAND TIER FOR THE AMOUNT FIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3530-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLM-AMOUNT-FIN-AMT NOT < WS-RESERVE-BAND-LOW(WS-TX)
                   MOVE WS-RESERVE-BAND-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT FIN AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3540-ACCRUE-AMOUNT-FIN.
           IF ADLT-AMOUNT-FIN-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-AMOUNT-FIN-AMT * ADLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ADLM-REBATE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3550-CHECK-LAST-PAY.
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
           IF WS-DAYS > 15
               MOVE 'Y' TO ADLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3560-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-REASON-CD
                   ADD ADLM-RESERVE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-RESERVE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY TIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3570-ACCUM-TIER.
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
      * SELECT THE RESERVE BAND TIER FOR THE GAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLM-GAP-AMT NOT < WS-RESERVE-BAND-LOW(WS-TX)
                   MOVE WS-RESERVE-BAND-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3590-AGE-LAST-STMT.
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
                   ADD ADLT-RESERVE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ADLT-RESERVE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ADLT-RESERVE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLT-RESERVE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLT-RESERVE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REBATE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3600-ACCRUE-REBATE.
           IF ADLT-REBATE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-REBATE-AMT * ADLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO ADLM-REBATE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REBATE FEE ON THE REBATE YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $150.00.
      *----------------------------------------------------------------*
       3610-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ADLM-REBATE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-PREFERRED-FLAG = 'Y'
                   COMPUTE ADLM-REBATE-AMT ROUNDED = ADLM-REBATE-YTD *
                       ADLM-FEE-PCT / 200
               WHEN ADLM-REASON-CD = 'FZV'
                   MOVE 15.00 TO ADLM-REBATE-AMT
               WHEN ADLM-REASON-CD = 'KPP'
                   MOVE 10.00 TO ADLM-REBATE-AMT
               WHEN OTHER
                   COMPUTE ADLM-REBATE-AMT ROUNDED = ADLM-REBATE-YTD *
                       ADLM-FEE-PCT / 100
           END-EVALUATE
           IF ADLM-REBATE-AMT > 0 AND ADLM-REBATE-AMT < 5.00
               MOVE 5.00 TO ADLM-REBATE-AMT
           END-IF
           IF ADLM-REBATE-AMT > 150.00
               MOVE 150.00 TO ADLM-REBATE-AMT
           END-IF
           ADD ADLM-REBATE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3620-DESCRIBE-CHANNEL.
           EVALUATE ADLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE BAND TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3630-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLT-REBATE-AMT NOT < WS-RESERVE-BAND-LOW(WS-TX)
                   MOVE WS-RESERVE-BAND-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEALER CD:
      * DRU -> FNF WHEN POST DATE IS PAST
      * FNF -> GBX WHEN POST DATE IS PAST
      * GBX -> GTU WHEN POST DATE IS PAST
      * GTU -> JNE WHEN POST DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3640-SET-DEALER.
           EVALUATE ADLM-DEALER-CD
               WHEN 'DRU'
                   IF ADLT-POST-DATE > 0 AND ADLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNF' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNF'
                   IF ADLT-POST-DATE > 0 AND ADLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GBX' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GBX'
                   IF ADLT-POST-DATE > 0 AND ADLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GTU' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GTU'
                   IF ADLT-POST-DATE > 0 AND ADLT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JNE' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST ACT DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-LAST-ACT.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO ADLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3660-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-PAYMENT-AMT ROUNDED =
                   ADLM-PAYMENT-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-PAYMENT-AMT
           END-IF
           ADD ADLM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE DEALER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3670-DESCRIBE-DEALER.
           EVALUATE ADLM-DEALER-CD
               WHEN 'DRU'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'FNF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GBX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GTU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'JNE'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KTP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3680-AGE-NEXT-DUE.
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
                   ADD ADLM-CHARGEBK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ADLM-CHARGEBK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ADLM-CHARGEBK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-CHARGEBK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-CHARGEBK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BK -> PY WHEN EFF DATE IS PAST
      * PY -> CB WHEN EFF DATE IS PAST
      * CB -> RB WHEN EFF DATE IS PAST
      * RB -> GP WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3690-SET-TRAN.
           EVALUATE ADLT-TRAN-CD
               WHEN 'BK'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CB' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CB'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GP' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TIER CD:
      * EW -> JF WHEN LAST MAINT DATE IS PAST
      * JF -> KD WHEN LAST MAINT DATE IS PAST
      * KD -> UG WHEN LAST MAINT DATE IS PAST
      * UG -> WY WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3700-SET-TIER.
           EVALUATE ADLM-TIER-CD
               WHEN 'EW'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JF' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KD' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UG' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WY' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3710-AGE-LAST-STMT.
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
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT GAP YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3720-SPLIT-GAP.
           COMPUTE WS-WORK-AMT = ADLM-GAP-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-GAP-YTD - (WS-WORK-AMT * 3)
           COMPUTE ADLM-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3730-DESCRIBE-TYPE.
           EVALUATE ADLM-TYPE-CD
               WHEN 'B'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'H'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'S'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'V'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'W'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'Z'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-LAST-STMT.
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
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT FIN YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3750-ACCRUE-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-AMOUNT-FIN-YTD * ADLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ADLM-AMOUNT-FIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GAP YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3760-ACCRUE-GAP.
           IF ADLM-GAP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-GAP-YTD * ADLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ADLM-GAP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP TIER TIER FOR THE AMOUNT FIN AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3770-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-TIER-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ADLM-AMOUNT-FIN-AMT NOT < WS-GAP-TIER-LOW(WS-TX)
                   MOVE WS-GAP-TIER-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE AMT BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3780-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-STATUS-CD
                   ADD ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP TIER TIER FOR THE AMOUNT FIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3790-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-TIER-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ADLM-AMOUNT-FIN-YTD NOT < WS-GAP-TIER-LOW(WS-TX)
                   MOVE WS-GAP-TIER-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EUA -> HMW WHEN EFFECTIVE DATE IS PAST
      * HMW -> RCB WHEN EFFECTIVE DATE IS PAST
      * RCB -> EUA WHEN EFFECTIVE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3800-SET-STATUS.
           EVALUATE ADLM-STATUS-CD
               WHEN 'EUA'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HMW' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HMW'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RCB' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RCB'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EUA' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3810-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 1000.00
               MOVE 1000.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON AMOUNT FIN YTD AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3820-ACCRUE-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-AMOUNT-FIN-YTD * ADLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ADLM-RESERVE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP TIER TIER FOR THE RESERVE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3830-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-TIER-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ADLM-RESERVE-YTD NOT < WS-GAP-TIER-LOW(WS-TX)
                   MOVE WS-GAP-TIER-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GAP AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3840-ACCRUE-GAP.
           IF ADLM-GAP-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-GAP-AMT * ADLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ADLM-AMOUNT-FIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3850-FLAG-AMOUNT-FIN.
           IF ADLT-AMOUNT-FIN-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ADLT-AMOUNT-FIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GAP YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3860-SPLIT-GAP.
           COMPUTE WS-WORK-AMT = ADLM-GAP-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-GAP-YTD - (WS-WORK-AMT * 3)
           COMPUTE ADLM-REBATE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-CYCLE.
           EVALUATE ADLM-CYCLE-CD
               WHEN 'A'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'H'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'J'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'T'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'W'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3880-AGE-POST.
           MOVE ZERO TO WS-DAYS
           IF ADLT-POST-DATE > 0 AND ADLT-POST-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLT-POST-DATE TO DTU-DATE-1
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
               WHEN WS-DAYS NOT > 150
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RESERVE AMT.
      *----------------------------------------------------------------*
       3890-PRORATE-RESERVE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-RESERVE-AMT ROUNDED =
                   ADLT-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-RESERVE-AMT
           END-IF
           ADD ADLM-RESERVE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE RESERVE AMT.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3900-ASSESS-STOP.
           EVALUATE TRUE
               WHEN ADLM-STOP-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-REVIEW-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = ADLM-RESERVE-AMT *
                       ADLM-FEE-PCT / 200
               WHEN ADLT-TRAN-CD = 'BK'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN ADLT-TRAN-CD = 'PY'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = ADLM-RESERVE-AMT *
                       ADLM-FEE-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 50.00
               MOVE 50.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> H WHEN LAST STMT DATE IS PAST
      * H -> J WHEN LAST STMT DATE IS PAST
      * J -> T WHEN LAST STMT DATE IS PAST
      * T -> W WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3910-SET-CYCLE.
           EVALUATE ADLM-CYCLE-CD
               WHEN 'A'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHARGEBK AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3920-ACCRUE-CHARGEBK.
           IF ADLM-CHARGEBK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-CHARGEBK-AMT * ADLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ADLT-RESERVE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3930-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE GAP YTD FEE ON THE PAYMENT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3940-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ADLM-GAP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ADLM-GAP-YTD ROUNDED = ADLT-PAYMENT-AMT *
                       ADLM-FEE-PCT / 200
               WHEN ADLM-DEALER-CD = 'DRU'
                   MOVE 15.00 TO ADLM-GAP-YTD
               WHEN ADLM-DEALER-CD = 'FNF'
                   MOVE 15.00 TO ADLM-GAP-YTD
               WHEN OTHER
                   COMPUTE ADLM-GAP-YTD ROUNDED = ADLT-PAYMENT-AMT *
                       ADLM-FEE-PCT / 100
           END-EVALUATE
           IF ADLM-GAP-YTD > 0 AND ADLM-GAP-YTD < 1.00
               MOVE 1.00 TO ADLM-GAP-YTD
           END-IF
           IF ADLM-GAP-YTD > 250.00
               MOVE 250.00 TO ADLM-GAP-YTD
           END-IF
           ADD ADLM-GAP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE BAND TIER FOR THE RESERVE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3950-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLT-RESERVE-AMT NOT < WS-RESERVE-BAND-LOW(WS-TX)
                   MOVE WS-RESERVE-BAND-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E030 OPERATOR REQUIRED
      * E761 GAP YTD NEGATIVE
      * E704 GAP YTD OVER 25000
      * E368 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3960-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-OPERATOR = SPACES
                   MOVE 'E030' TO WS-REJECT-CODE
               WHEN ADLM-GAP-YTD < 0
                   MOVE 'E761' TO WS-REJECT-CODE
               WHEN ADLM-GAP-YTD > 25000.00
                   MOVE 'E704' TO WS-REJECT-CODE
               WHEN ADLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E368' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> H WHEN NEXT DUE DATE IS PAST
      * H -> J WHEN NEXT DUE DATE IS PAST
      * J -> T WHEN NEXT DUE DATE IS PAST
      * T -> W WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3970-SET-CYCLE.
           EVALUATE ADLM-CYCLE-CD
               WHEN 'A'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3980-COUNT-RETRY.
           IF ADLM-RETRY-CNT < 9999999
               ADD 1 TO ADLM-RETRY-CNT
           END-IF
           IF ADLM-RETRY-CNT NOT < 12
               MOVE 'Y' TO ADLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3990-FLAG-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ADLM-AMOUNT-FIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4000-AGE-LAST-MAINT.
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
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4010-COUNT-SEQ.
           IF ADLT-SEQ < 9999999
               ADD 1 TO ADLT-SEQ
           END-IF
           IF ADLT-SEQ NOT < 10
               MOVE 'Y' TO ADLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4020-LIMIT-PAYMENT.
           IF ADLT-PAYMENT-AMT < 100.00
               MOVE 100.00 TO ADLT-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLT-PAYMENT-AMT > 500.00
               MOVE 500.00 TO ADLT-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4030-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF WS-WORK-AMT-2 NOT < WS-RESERVE-BAND-LOW(WS-TX)
                   MOVE WS-RESERVE-BAND-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESERVE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4040-SPLIT-RESERVE.
           COMPUTE WS-WORK-AMT = ADLT-RESERVE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ADLT-RESERVE-AMT - (WS-WORK-AMT * 3)
           COMPUTE ADLM-AMOUNT-FIN-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK YTD IS FLOORED AT $25.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4050-LIMIT-CHARGEBK.
           IF ADLM-CHARGEBK-YTD < 25.00
               MOVE 25.00 TO ADLM-CHARGEBK-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-CHARGEBK-YTD > 500.00
               MOVE 500.00 TO ADLM-CHARGEBK-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-30, 31-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4060-AGE-EFFECTIVE.
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
                   ADD ADLT-RESERVE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ADLT-RESERVE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 30
                   ADD ADLT-RESERVE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD ADLT-RESERVE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLT-RESERVE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4070-ACCRUE-RESERVE.
           IF ADLT-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-RESERVE-AMT * ADLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ADLM-AMOUNT-FIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-45, 46-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4080-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD ADLM-RESERVE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ADLM-RESERVE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-RESERVE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ADLM-RESERVE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-RESERVE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GAP YTD IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4090-LIMIT-GAP.
           IF ADLM-GAP-YTD < 25.00
               MOVE 25.00 TO ADLM-GAP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-GAP-YTD > 50000.00
               MOVE 50000.00 TO ADLM-GAP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4100-DESCRIBE-TRAN.
           EVALUATE ADLT-TRAN-CD
               WHEN 'BK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'PY'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CB'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'GP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E019 OPERATOR REQUIRED
      * E163 AMOUNT FIN AMT NEGATIVE
      * E673 AMOUNT FIN AMT OVER 99999
      * E704 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4110-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-OPERATOR = SPACES
                   MOVE 'E019' TO WS-REJECT-CODE
               WHEN ADLT-AMOUNT-FIN-AMT < 0
                   MOVE 'E163' TO WS-REJECT-CODE
               WHEN ADLT-AMOUNT-FIN-AMT > 99999.00
                   MOVE 'E673' TO WS-REJECT-CODE
               WHEN ADLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E704' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RESERVE YTD FEE ON THE WORK AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4120-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ADLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ADLM-RESERVE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ADLM-RESERVE-YTD ROUNDED = WS-WORK-AMT *
                       ADLM-MARGIN / 200
               WHEN ADLM-STATUS-CD = 'EUA'
                   MOVE 7.00 TO ADLM-RESERVE-YTD
               WHEN ADLM-STATUS-CD = 'HMW'
                   MOVE 15.00 TO ADLM-RESERVE-YTD
               WHEN OTHER
                   COMPUTE ADLM-RESERVE-YTD ROUNDED = WS-WORK-AMT *
                       ADLM-MARGIN / 100
           END-EVALUATE
           IF ADLM-RESERVE-YTD > 0 AND ADLM-RESERVE-YTD < 10.00
               MOVE 10.00 TO ADLM-RESERVE-YTD
           END-IF
           IF ADLM-RESERVE-YTD > 25.00
               MOVE 25.00 TO ADLM-RESERVE-YTD
           END-IF
           ADD ADLM-RESERVE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE GAP TIER TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4130-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-TIER-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ADLM-PAYMENT-YTD NOT < WS-GAP-TIER-LOW(WS-TX)
                   MOVE WS-GAP-TIER-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4140-CHECK-LAST-PAY.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO ADLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE AMOUNT FIN AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4150-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ADLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ADLM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-STOP-FLAG = 'Y'
                   COMPUTE ADLM-PAYMENT-AMT ROUNDED =
                       ADLT-AMOUNT-FIN-AMT * ADLM-MARGIN / 200
               WHEN ADLM-TYPE-CD = 'B'
                   MOVE 20.00 TO ADLM-PAYMENT-AMT
               WHEN ADLM-TYPE-CD = 'H'
                   MOVE 7.00 TO ADLM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE ADLM-PAYMENT-AMT ROUNDED =
                       ADLT-AMOUNT-FIN-AMT * ADLM-MARGIN / 100
           END-EVALUATE
           IF ADLM-PAYMENT-AMT > 0 AND ADLM-PAYMENT-AMT < 2.00
               MOVE 2.00 TO ADLM-PAYMENT-AMT
           END-IF
           IF ADLM-PAYMENT-AMT > 50.00
               MOVE 50.00 TO ADLM-PAYMENT-AMT
           END-IF
           ADD ADLM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4160-FLAG-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ADLM-AMOUNT-FIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EUA -> HMW WHEN LAST PAY DATE IS PAST
      * HMW -> RCB WHEN LAST PAY DATE IS PAST
      * RCB -> EUA WHEN LAST PAY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4170-SET-STATUS.
           EVALUATE ADLM-STATUS-CD
               WHEN 'EUA'
                   IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HMW' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HMW'
                   IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RCB' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RCB'
                   IF ADLM-LAST-PAY-DATE > 0 AND ADLM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EUA' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE RESERVE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       4180-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-PAYMENT-YTD ROUNDED =
                   ADLM-RESERVE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-PAYMENT-YTD
           END-IF
           ADD ADLM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EUA -> HMW WHEN EFF DATE IS PAST
      * HMW -> RCB WHEN EFF DATE IS PAST
      * RCB -> EUA WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4190-SET-STATUS.
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
      * REBATE AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4200-FLAG-REBATE.
           IF ADLT-REBATE-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ADLT-REBATE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4210-LIMIT-CHARGEBK.
           IF ADLM-CHARGEBK-AMT < 0.00
               MOVE 0.00 TO ADLM-CHARGEBK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-CHARGEBK-AMT > 1000.00
               MOVE 1000.00 TO ADLM-CHARGEBK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4220-FLAG-ITEM.
           IF WS-WORK-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT FIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT YTD.
      *----------------------------------------------------------------*
       4230-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-PAYMENT-YTD ROUNDED =
                   ADLM-AMOUNT-FIN-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-PAYMENT-YTD
           END-IF
           ADD ADLM-PAYMENT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4240-ACCRUE-RESERVE.
           IF ADLM-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-RESERVE-AMT * ADLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ADLM-GAP-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NEXT REVIEW DATE IS PAST
      * BAT -> MAL WHEN NEXT REVIEW DATE IS PAST
      * MAL -> TEL WHEN NEXT REVIEW DATE IS PAST
      * TEL -> ONL WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4250-SET-CHANNEL.
           EVALUATE ADLT-CHANNEL-CD
               WHEN 'BRN'
                   IF ADLM-NEXT-REVIEW-DATE > 0 AND
                       ADLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ADLM-NEXT-REVIEW-DATE > 0 AND
                       ADLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ADLM-NEXT-REVIEW-DATE > 0 AND
                       ADLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ADLM-NEXT-REVIEW-DATE > 0 AND
                       ADLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4260-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = ADLM-PAYMENT-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ADLM-PAYMENT-AMT - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4270-AGE-NEXT-REVIEW.
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
                   ADD ADLM-REBATE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ADLM-REBATE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ADLM-REBATE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-REBATE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-REBATE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4280-COUNT-SEQ.
           IF ADLT-SEQ < 9999999
               ADD 1 TO ADLT-SEQ
           END-IF
           IF ADLT-SEQ NOT < 5
               MOVE 'Y' TO ADLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RESERVE AMT IS FLOORED AT $10.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4290-LIMIT-RESERVE.
           IF ADLM-RESERVE-AMT < 10.00
               MOVE 10.00 TO ADLM-RESERVE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-RESERVE-AMT > 50000.00
               MOVE 50000.00 TO ADLM-RESERVE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4300-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E747 CUST ID REQUIRED
      * E151 PAYMENT YTD NEGATIVE
      * E028 PAYMENT YTD OVER 5000
      * E857 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       4310-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-CUST-ID = SPACES
                   MOVE 'E747' TO WS-REJECT-CODE
               WHEN ADLM-PAYMENT-YTD < 0
                   MOVE 'E151' TO WS-REJECT-CODE
               WHEN ADLM-PAYMENT-YTD > 5000.00
                   MOVE 'E028' TO WS-REJECT-CODE
               WHEN ADLM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E857' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE AMT BY TIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4320-ACCUM-TIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TIER-CD
                   ADD ADLT-RESERVE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLT-RESERVE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT 2 AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4330-ACCRUE-MARGIN.
           IF WS-WORK-AMT-2 > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * ADLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO ADLM-CHARGEBK-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN EXPIRY DATE IS PAST
      * BAT -> MAL WHEN EXPIRY DATE IS PAST
      * MAL -> TEL WHEN EXPIRY DATE IS PAST
      * TEL -> ONL WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4340-SET-CHANNEL.
           EVALUATE ADLT-CHANNEL-CD
               WHEN 'BRN'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4350-COUNT-EXCEPT.
           IF ADLM-EXCEPT-CNT < 9999999
               ADD 1 TO ADLM-EXCEPT-CNT
           END-IF
           IF ADLM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO ADLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GAP YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4360-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-STATUS-CD
                   ADD ADLM-GAP-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-GAP-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4370-COUNT-PAYMENT.
           IF ADLM-PAYMENT-CNT < 9999999
               ADD 1 TO ADLM-PAYMENT-CNT
           END-IF
           IF ADLM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4380-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF ADLT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO ADLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHARGEBK AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4390-LIMIT-CHARGEBK.
           IF ADLM-CHARGEBK-AMT < 10.00
               MOVE 10.00 TO ADLM-CHARGEBK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-CHARGEBK-AMT > 5000.00
               MOVE 5000.00 TO ADLM-CHARGEBK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REBATE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4400-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = ADLM-REBATE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ADLM-REBATE-AMT - (WS-WORK-AMT * 12)
           COMPUTE ADLM-RESERVE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN EFF DATE IS PAST
      * H -> S WHEN EFF DATE IS PAST
      * S -> V WHEN EFF DATE IS PAST
      * V -> W WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4410-SET-TYPE.
           EVALUATE ADLM-TYPE-CD
               WHEN 'B'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REBATE AMT IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4420-LIMIT-REBATE.
           IF ADLM-REBATE-AMT < 50.00
               MOVE 50.00 TO ADLM-REBATE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-REBATE-AMT > 500.00
               MOVE 500.00 TO ADLM-REBATE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4430-COUNT-RETRY.
           IF ADLM-RETRY-CNT < 9999999
               ADD 1 TO ADLM-RETRY-CNT
           END-IF
           IF ADLM-RETRY-CNT NOT < 10
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE GAP AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4440-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-CYCLE-CD
                   ADD ADLM-GAP-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-GAP-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E881 OPERATOR REQUIRED
      * E550 AMOUNT FIN YTD NEGATIVE
      * E442 AMOUNT FIN YTD OVER 25000
      * E625 NEXT REVIEW DATE IN FUTURE
      *----------------------------------------------------------------*
       4450-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-OPERATOR = SPACES
                   MOVE 'E881' TO WS-REJECT-CODE
               WHEN ADLM-AMOUNT-FIN-YTD < 0
                   MOVE 'E550' TO WS-REJECT-CODE
               WHEN ADLM-AMOUNT-FIN-YTD > 25000.00
                   MOVE 'E442' TO WS-REJECT-CODE
               WHEN ADLM-NEXT-REVIEW-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E625' TO WS-REJECT-CODE
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
       4460-DESCRIBE-CHANNEL.
           EVALUATE ADLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4470-AGE-EFFECTIVE.
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
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E873 MEMO REQUIRED
      * E852 RESERVE AMT NEGATIVE
      * E259 RESERVE AMT OVER 1000000
      * E041 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4480-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-MEMO = SPACES
                   MOVE 'E873' TO WS-REJECT-CODE
               WHEN ADLT-RESERVE-AMT < 0
                   MOVE 'E852' TO WS-REJECT-CODE
               WHEN ADLT-RESERVE-AMT > 1000000.00
                   MOVE 'E259' TO WS-REJECT-CODE
               WHEN ADLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E041' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BK -> PY WHEN NOTICE DATE IS PAST
      * PY -> CB WHEN NOTICE DATE IS PAST
      * CB -> RB WHEN NOTICE DATE IS PAST
      * RB -> GP WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4490-SET-TRAN.
           EVALUATE ADLT-TRAN-CD
               WHEN 'BK'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PY' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CB' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CB'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RB' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GP' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT FIN FEE ON THE GAP AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4500-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ADLM-AMOUNT-FIN-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   COMPUTE ADLM-AMOUNT-FIN-AMT ROUNDED = ADLM-GAP-AMT *
                       ADLM-FEE-PCT / 200
               WHEN ADLM-REASON-CD = 'FZV'
                   MOVE 12.00 TO ADLM-AMOUNT-FIN-AMT
               WHEN ADLM-REASON-CD = 'KPP'
                   MOVE 20.00 TO ADLM-AMOUNT-FIN-AMT
               WHEN OTHER
                   COMPUTE ADLM-AMOUNT-FIN-AMT ROUNDED = ADLM-GAP-AMT *
                       ADLM-FEE-PCT / 100
           END-EVALUATE
           IF ADLM-AMOUNT-FIN-AMT > 0 AND ADLM-AMOUNT-FIN-AMT < 2.00
               MOVE 2.00 TO ADLM-AMOUNT-FIN-AMT
           END-IF
           IF ADLM-AMOUNT-FIN-AMT > 100.00
               MOVE 100.00 TO ADLM-AMOUNT-FIN-AMT
           END-IF
           ADD ADLM-AMOUNT-FIN-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * RESERVE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4510-FLAG-RESERVE.
           IF ADLM-RESERVE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ADLM-RESERVE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4520-COUNT-EXCEPT.
           IF ADLM-EXCEPT-CNT < 9999999
               ADD 1 TO ADLM-EXCEPT-CNT
           END-IF
           IF ADLM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO ADLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EXPIRY DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4530-CHECK-EXPIRY.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO ADLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT REVIEW DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4540-AGE-NEXT-REVIEW.
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
                   ADD ADLM-CHARGEBK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ADLM-CHARGEBK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ADLM-CHARGEBK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-CHARGEBK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-CHARGEBK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * GAP YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4550-LIMIT-GAP.
           IF ADLM-GAP-YTD < 25.00
               MOVE 25.00 TO ADLM-GAP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-GAP-YTD > 5000.00
               MOVE 5000.00 TO ADLM-GAP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DEALER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4560-DESCRIBE-DEALER.
           EVALUATE ADLM-DEALER-CD
               WHEN 'DRU'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FNF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GBX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GTU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'JNE'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KTP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REBATE FEE ON THE REBATE AMT.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       4570-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ADLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ADLM-REBATE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   COMPUTE ADLM-REBATE-AMT ROUNDED = ADLT-REBATE-AMT *
                       ADLM-MARGIN / 200
               WHEN ADLM-CYCLE-CD = 'A'
                   MOVE 7.00 TO ADLM-REBATE-AMT
               WHEN ADLM-CYCLE-CD = 'H'
                   MOVE 7.00 TO ADLM-REBATE-AMT
               WHEN OTHER
                   COMPUTE ADLM-REBATE-AMT ROUNDED = ADLT-REBATE-AMT *
                       ADLM-MARGIN / 100
           END-EVALUATE
           IF ADLM-REBATE-AMT > 0 AND ADLM-REBATE-AMT < 10.00
               MOVE 10.00 TO ADLM-REBATE-AMT
           END-IF
           IF ADLM-REBATE-AMT > 50.00
               MOVE 50.00 TO ADLM-REBATE-AMT
           END-IF
           ADD ADLM-REBATE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EUA -> HMW WHEN NEXT DUE DATE IS PAST
      * HMW -> RCB WHEN NEXT DUE DATE IS PAST
      * RCB -> EUA WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4580-SET-STATUS.
           EVALUATE ADLM-STATUS-CD
               WHEN 'EUA'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HMW' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HMW'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RCB' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RCB'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EUA' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE BAND TIER FOR THE AMOUNT FIN YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4590-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 4
               IF ADLM-AMOUNT-FIN-YTD NOT < WS-RESERVE-BAND-LOW(WS-TX)
                   MOVE WS-RESERVE-BAND-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT GAP YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4600-SPLIT-GAP.
           COMPUTE WS-WORK-AMT = ADLM-GAP-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ADLM-GAP-YTD - (WS-WORK-AMT * 2)
           COMPUTE ADLM-AMOUNT-FIN-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4610-ACCRUE-RESERVE.
           IF ADLT-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-RESERVE-AMT * ADLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ADLT-AMOUNT-FIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REBATE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4620-FLAG-REBATE.
           IF ADLM-REBATE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ADLM-REBATE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4630-AGE-EFFECTIVE.
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
                   ADD ADLM-RESERVE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ADLM-RESERVE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-RESERVE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-RESERVE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-RESERVE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TIER CD:
      * EW -> JF WHEN LAST MAINT DATE IS PAST
      * JF -> KD WHEN LAST MAINT DATE IS PAST
      * KD -> UG WHEN LAST MAINT DATE IS PAST
      * UG -> WY WHEN LAST MAINT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4640-SET-TIER.
           EVALUATE ADLM-TIER-CD
               WHEN 'EW'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'JF' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'KD' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'UG' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'WY' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE REBATE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CHARGEBK AMT.
      *----------------------------------------------------------------*
       4650-PRORATE-CHARGEBK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-CHARGEBK-AMT ROUNDED =
                   ADLM-REBATE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-CHARGEBK-AMT
           END-IF
           ADD ADLM-CHARGEBK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4660-AGE-EFFECTIVE.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> H WHEN NEXT DUE DATE IS PAST
      * H -> J WHEN NEXT DUE DATE IS PAST
      * J -> T WHEN NEXT DUE DATE IS PAST
      * T -> W WHEN NEXT DUE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4670-SET-CYCLE.
           EVALUATE ADLM-CYCLE-CD
               WHEN 'A'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'J' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'J'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'T' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'T'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO ADLM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY TIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4680-ACCUM-TIER.
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
      * SPLIT REBATE AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4690-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = ADLT-REBATE-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ADLT-REBATE-AMT - (WS-WORK-AMT * 3)
           COMPUTE ADLM-CHARGEBK-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE GAP TIER TIER FOR THE REBATE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4700-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-GAP-TIER-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 10
               IF ADLM-REBATE-YTD NOT < WS-GAP-TIER-LOW(WS-TX)
                   MOVE WS-GAP-TIER-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4710-COUNT-EXCEPT.
           IF ADLM-EXCEPT-CNT < 9999999
               ADD 1 TO ADLM-EXCEPT-CNT
           END-IF
           IF ADLM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO ADLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT FIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESERVE YTD.
      *----------------------------------------------------------------*
       4720-PRORATE-RESERVE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-RESERVE-YTD ROUNDED =
                   ADLM-AMOUNT-FIN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-RESERVE-YTD
           END-IF
           ADD ADLM-RESERVE-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E657 ADDR REQUIRED
      * E868 REBATE AMT NEGATIVE
      * E350 REBATE AMT OVER 250000
      * E536 POST DATE IN FUTURE
      *----------------------------------------------------------------*
       4730-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-ADDR = SPACES
                   MOVE 'E657' TO WS-REJECT-CODE
               WHEN ADLT-REBATE-AMT < 0
                   MOVE 'E868' TO WS-REJECT-CODE
               WHEN ADLT-REBATE-AMT > 250000.00
                   MOVE 'E350' TO WS-REJECT-CODE
               WHEN ADLT-POST-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E536' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4740-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = ADLM-PAYMENT-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-PAYMENT-YTD - (WS-WORK-AMT * 3)
           COMPUTE ADLM-GAP-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4750-ACCRUE-PAYMENT.
           IF ADLM-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-PAYMENT-AMT * ADLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ADLT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       4760-COUNT-RETRY.
           IF ADLM-RETRY-CNT < 9999999
               ADD 1 TO ADLM-RETRY-CNT
           END-IF
           IF ADLM-RETRY-CNT NOT < 12
               MOVE 'Y' TO ADLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TIER CD:
      * EW -> JF WHEN EXPIRY DATE IS PAST
      * JF -> KD WHEN EXPIRY DATE IS PAST
      * KD -> UG WHEN EXPIRY DATE IS PAST
      * UG -> WY WHEN EXPIRY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4770-SET-TIER.
           EVALUATE ADLM-TIER-CD
               WHEN 'EW'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JF' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E132 CONTACT REQUIRED
      * E718 GAP AMT NEGATIVE
      * E366 GAP AMT OVER 250000
      * E248 NEXT DUE DATE IN FUTURE
      *----------------------------------------------------------------*
       4780-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-CONTACT = SPACES
                   MOVE 'E132' TO WS-REJECT-CODE
               WHEN ADLM-GAP-AMT < 0
                   MOVE 'E718' TO WS-REJECT-CODE
               WHEN ADLM-GAP-AMT > 250000.00
                   MOVE 'E366' TO WS-REJECT-CODE
               WHEN ADLM-NEXT-DUE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E248' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT FIN YTD FEE ON THE AMOUNT FIN AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       4790-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ADLM-AMOUNT-FIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-STOP-FLAG = 'Y'
                   COMPUTE ADLM-AMOUNT-FIN-YTD ROUNDED =
                       ADLM-AMOUNT-FIN-AMT * ADLM-RATE / 200
               WHEN ADLM-STATUS-CD = 'EUA'
                   MOVE 15.00 TO ADLM-AMOUNT-FIN-YTD
               WHEN ADLM-STATUS-CD = 'HMW'
                   MOVE 15.00 TO ADLM-AMOUNT-FIN-YTD
               WHEN OTHER
                   COMPUTE ADLM-AMOUNT-FIN-YTD ROUNDED =
                       ADLM-AMOUNT-FIN-AMT * ADLM-RATE / 100
           END-EVALUATE
           IF ADLM-AMOUNT-FIN-YTD > 0 AND ADLM-AMOUNT-FIN-YTD < 1.00
               MOVE 1.00 TO ADLM-AMOUNT-FIN-YTD
           END-IF
           IF ADLM-AMOUNT-FIN-YTD > 35.00
               MOVE 35.00 TO ADLM-AMOUNT-FIN-YTD
           END-IF
           ADD ADLM-AMOUNT-FIN-YTD TO WS-TOTAL-FEES.

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
                   MOVE AURISK-GAP-AMT
                     TO ADLM-GAP-AMT
                   MOVE AURISK-AMOUNT-FIN-AMT
                     TO ADLM-AMOUNT-FIN-AMT
                   MOVE AURISK-REBATE-AMT
                     TO ADLM-REBATE-AMT
                   MOVE AURISK-EXEMPT-FLAG
                     TO ADLM-EXEMPT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RISK' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7030-CALL-AURISK' TO ABND-PARAGRAPH
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
           DISPLAY 'AULRNEW READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'AULRNEW' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
