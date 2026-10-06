      *================================================================*
      * PROGRAM  : AULSETL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : DEALER AUTO FINANCE - SETTLEMENT. READS THE ADL     *
      *            TRANSACTIONS, UPDATES THE ADL MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. INDIRECT AUTO DEALER CONTRACTS, *
      *            RESERVE AND CHARGEBACK                              *
      * INPUT    : ADLMST (KSDS), ADLTRN, BANKCTL                      *
      * OUTPUT   : ADLOUT, AULSERPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, AULIM                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-11-20 BHS  ORIGINAL                                       *
      * 1994-07-17 CRP  PERFORMANCE - CHARGEBK READ AHEAD CHG5235      *
      * 1998-12-24 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-03-04 BHS  GAP LIMIT REVISED PRB1593                      *
      * 2000-04-20 JLM  CHARGEBK TABLE EXPANDED PRB5642                *
      * 2001-01-25 DPS  AMOUNT-FIN ROUNDING - HALF UP INC8914          *
      * 2003-01-16 RJT  PAYMENT CALCULATION CORRECTED PRB0753          *
      * 2004-02-17 JLM  CHARGEBK EXCEPTIONS TO OPS QUEUE INC0326       *
      * 2006-03-16 TWH  PAYMENT RERUN PROTECTION CHG7391               *
      * 2008-08-15 EWS  RESERVE TABLE EXPANDED CHG3738                 *
      * 2010-05-21 DPS  GAP ROUNDING - HALF UP INC7276                 *
      * 2010-04-12 CRP  PERFORMANCE - PAYMENT READ AHEAD INC8842       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AULSETL.
       AUTHOR. R J TRAVERS.

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
           SELECT REPORT-FILE ASSIGN TO "AULSERPT"
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
                                           VALUE 10.000000.
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
      * WS-RESERVE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-RESERVE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000404000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000500000000029000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000550000000051500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001550000000068000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00011550000000359000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012050000000777000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00017050000000892500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022050000000496000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00022100000000129000'.
       01  WS-RESERVE-BAND-TBL REDEFINES WS-RESERVE-BAND-VALUES.
           05  WS-RESERVE-BAND-ROW OCCURS 9 TIMES.
               10  WS-RESERVE-BAND-LOW     PIC 9(09)V99.
               10  WS-RESERVE-BAND-RATE    PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-REBATE-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REBATE-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000622500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000234500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005250000000606000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015250000000690000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00020250000000354500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00030250000000146500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00031250000000813000'.
       01  WS-REBATE-TIER-TBL REDEFINES WS-REBATE-TIER-VALUES.
           05  WS-REBATE-TIER-ROW OCCURS 7 TIMES.
               10  WS-REBATE-TIER-LOW      PIC 9(09)V99.
               10  WS-REBATE-TIER-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  AULSETL  DEALER AUTO FINANCE'.
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
       COPY AULIMP.

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
               PERFORM 3000-SET-TYPE
               PERFORM 3010-COUNT-CYCLE
               PERFORM 3020-DESCRIBE-TIER
               PERFORM 3030-EDIT-REJECT
               PERFORM 3040-PRORATE-PAYMENT
               PERFORM 3050-SET-REASON
               PERFORM 3060-SELECT-FEE-PCT
               PERFORM 3070-LIMIT-AMOUNT-FIN
               PERFORM 3080-ACCUM-TYPE
               PERFORM 3090-ACCRUE-GAP
               PERFORM 3100-SELECT-RATE
               PERFORM 3110-ACCRUE-PAYMENT
               PERFORM 3120-SET-DEALER
               PERFORM 3130-DESCRIBE-TIER
               PERFORM 3140-SPLIT-AMOUNT-FIN
               PERFORM 3150-SELECT-RATE
               PERFORM 3160-AGE-POST
               PERFORM 3170-PRORATE-REBATE
               PERFORM 3180-DESCRIBE-REASON
               PERFORM 3190-SET-CYCLE
               PERFORM 3200-CHECK-NEXT-DUE
               PERFORM 3210-ACCUM-CHANNEL
               PERFORM 3220-LIMIT-REBATE
               PERFORM 3230-SET-DEALER
               PERFORM 3240-CHECK-LAST-STMT
               PERFORM 3250-LIMIT-GAP
               PERFORM 3260-SET-REASON
               PERFORM 3270-CHECK-LAST-PAY
               PERFORM 3280-CHECK-LAST-STMT
               PERFORM 3290-SET-STATUS
               PERFORM 3300-COUNT-CYCLE
               PERFORM 3310-ASSESS-REVERSAL
               PERFORM 3320-ASSESS-OVERRIDE
               PERFORM 3330-PRORATE-PAYMENT
               PERFORM 3340-SPLIT-REBATE
               PERFORM 3350-SET-DEALER
               PERFORM 3360-ASSESS-NOTICE-SENT
               PERFORM 3370-LIMIT-AMOUNT-FIN
               PERFORM 3380-ASSESS-REVIEW
               PERFORM 3390-DESCRIBE-TIER
               PERFORM 3400-COUNT-CYCLE
               PERFORM 3410-ASSESS-PREFERRED
               PERFORM 3420-FLAG-AMOUNT-FIN
               PERFORM 3430-AGE-EXPIRY
               PERFORM 3440-FLAG-ITEM
               PERFORM 3450-CHECK-EFF
               PERFORM 3460-ACCUM-CYCLE
               PERFORM 3470-SELECT-FEE-PCT
               PERFORM 3480-DESCRIBE-CHANNEL
               PERFORM 3490-FLAG-ITEM
               PERFORM 3500-AGE-NOTICE
               PERFORM 3510-PRORATE-PAYMENT
               PERFORM 3520-LIMIT-PAYMENT
               PERFORM 3530-LIMIT-RESERVE
               PERFORM 3540-SELECT-RATE
               PERFORM 3550-SELECT-RATE
               PERFORM 3560-SPLIT-REBATE
               PERFORM 3570-SET-TRAN
               PERFORM 3580-AGE-LAST-ACT
               PERFORM 3590-FLAG-REBATE
               PERFORM 3600-PRORATE-RESERVE
               PERFORM 3610-AGE-POST
               PERFORM 3620-COUNT-RETRY
               PERFORM 3630-ACCUM-TRAN
               PERFORM 3640-ACCUM-TRAN
               PERFORM 3650-COUNT-SEQ
               PERFORM 3660-PRORATE-GAP
               PERFORM 3670-SELECT-MARGIN
               PERFORM 3680-SELECT-RATE
               PERFORM 3690-COUNT-RETRY
               PERFORM 3700-SELECT-MARGIN
               PERFORM 3710-FLAG-PAYMENT
               PERFORM 3720-LIMIT-RESERVE
               PERFORM 3730-CHECK-EFFECTIVE
               PERFORM 3740-PRORATE-AMOUNT-FIN
               PERFORM 3750-SET-CYCLE
               PERFORM 3760-SELECT-RATE
               PERFORM 3770-EDIT-REJECT
               PERFORM 3780-SPLIT-RESERVE
               PERFORM 3790-PRORATE-RESERVE
               PERFORM 3800-SELECT-MARGIN
               PERFORM 3810-DESCRIBE-DEALER
               PERFORM 3820-DESCRIBE-REASON
               PERFORM 3830-ACCUM-TYPE
               PERFORM 3840-SET-TYPE
               PERFORM 3850-ACCUM-TRAN
               PERFORM 3860-SET-REASON
               PERFORM 3870-DESCRIBE-STATUS
               PERFORM 3880-AGE-LAST-MAINT
               PERFORM 3890-ACCRUE-PAYMENT
               PERFORM 3900-FLAG-PAYMENT
               PERFORM 3910-LIMIT-RESERVE
               PERFORM 3920-FLAG-ITEM
               PERFORM 3930-SPLIT-AMOUNT-FIN
               PERFORM 3940-PRORATE-AMOUNT-FIN
               PERFORM 3950-ASSESS-PREFERRED
               PERFORM 3960-SELECT-FEE-PCT
               PERFORM 3970-ASSESS-NOTICE-SENT
               PERFORM 3980-ACCRUE-RATE
               PERFORM 3990-DESCRIBE-TIER
               PERFORM 4000-FLAG-CHARGEBK
               PERFORM 4010-EDIT-CUST-ID
               PERFORM 4020-AGE-EFFECTIVE
               PERFORM 4030-ACCRUE-FEE-PCT
               PERFORM 4040-COUNT-PAYMENT
               PERFORM 4050-AGE-NEXT-DUE
               PERFORM 4060-LIMIT-ITEM
               PERFORM 4070-ACCRUE-PAYMENT
               PERFORM 4080-PRORATE-REBATE
               PERFORM 4090-ACCRUE-RESERVE
               PERFORM 4100-COUNT-SEQ
               PERFORM 4110-AGE-NOTICE
               PERFORM 4120-ACCRUE-GAP
               PERFORM 4130-PRORATE-PAYMENT
               PERFORM 4140-LIMIT-GAP
               PERFORM 4150-SPLIT-AMOUNT-FIN
               PERFORM 4160-CHECK-NEXT-DUE
               PERFORM 4170-SET-TIER
               PERFORM 4180-ACCRUE-RESERVE
               PERFORM 4190-PRORATE-AMOUNT-FIN
               PERFORM 4200-ACCRUE-REBATE
               PERFORM 4210-COUNT-RETRY
               PERFORM 4220-ACCRUE-REBATE
               PERFORM 4230-FLAG-RESERVE
               PERFORM 4240-COUNT-EXCEPT
               PERFORM 4250-CHECK-POST
               PERFORM 4260-LIMIT-ITEM
               PERFORM 4270-ASSESS-EXEMPT
               PERFORM 4280-SPLIT-AMOUNT-FIN
               PERFORM 4290-PRORATE-CHARGEBK
               PERFORM 4300-DESCRIBE-TYPE
               PERFORM 4310-ACCUM-CYCLE
               PERFORM 4320-SET-CHANNEL
               PERFORM 4330-LIMIT-REBATE
               PERFORM 4340-COUNT-EXCEPT
               PERFORM 4350-EDIT-MEMO
               PERFORM 4360-LIMIT-REBATE
               PERFORM 4370-PRORATE-PAYMENT
               PERFORM 4380-COUNT-SEQ
               PERFORM 4390-PRORATE-RESERVE
               PERFORM 4400-SET-REASON
               PERFORM 4410-EDIT-REJECT
               PERFORM 4420-PRORATE-PAYMENT
               PERFORM 4430-LIMIT-AMOUNT-FIN
               PERFORM 4440-CHECK-NEXT-DUE
               PERFORM 4450-COUNT-RETRY
               PERFORM 4460-FLAG-AMOUNT-FIN
               PERFORM 4470-PRORATE-CHARGEBK
               PERFORM 4480-ACCRUE-REBATE
               PERFORM 4490-DESCRIBE-CHANNEL
               PERFORM 4500-FLAG-REBATE
               PERFORM 4510-LIMIT-RESERVE
               PERFORM 4520-COUNT-RETRY
               PERFORM 4530-ACCRUE-RATE
               PERFORM 4540-EDIT-REJECT
               PERFORM 4550-SET-REASON
               PERFORM 4560-EDIT-ADDR
               PERFORM 4570-ACCRUE-REBATE
               PERFORM 4580-ASSESS-REVERSAL
               PERFORM 4590-FLAG-PAYMENT
               PERFORM 4600-SELECT-RATE
               PERFORM 4610-SET-TYPE
               PERFORM 4620-ASSESS-EXEMPT
               PERFORM 4630-CHECK-NEXT-REVIEW
               PERFORM 4640-EDIT-REJECT
               PERFORM 4650-ACCUM-TIER
               PERFORM 4660-ACCRUE-RESERVE
               PERFORM 7000-CALL-AULIM
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
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN EXPIRY DATE IS PAST
      * H -> S WHEN EXPIRY DATE IS PAST
      * S -> V WHEN EXPIRY DATE IS PAST
      * V -> W WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3000-SET-TYPE.
           EVALUATE ADLM-TYPE-CD
               WHEN 'B'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-CYCLE.
           IF ADLM-CYCLE-CNT < 9999999
               ADD 1 TO ADLM-CYCLE-CNT
           END-IF
           IF ADLM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO ADLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3020-DESCRIBE-TIER.
           EVALUATE ADLM-TIER-CD
               WHEN 'EW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UG'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'WY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E444 REJECT CD REQUIRED
      * E134 CHARGEBK AMT NEGATIVE
      * E352 CHARGEBK AMT OVER 99999
      * E634 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       3030-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-REJECT-CD = SPACES
                   MOVE 'E444' TO WS-REJECT-CODE
               WHEN ADLM-CHARGEBK-AMT < 0
                   MOVE 'E134' TO WS-REJECT-CODE
               WHEN ADLM-CHARGEBK-AMT > 99999.00
                   MOVE 'E352' TO WS-REJECT-CODE
               WHEN ADLM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E634' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3040-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-PAYMENT-AMT ROUNDED =
                   WS-WORK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-PAYMENT-AMT
           END-IF
           ADD ADLM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * FZV -> KPP WHEN LAST STMT DATE IS PAST
      * KPP -> MVN WHEN LAST STMT DATE IS PAST
      * MVN -> UVY WHEN LAST STMT DATE IS PAST
      * UVY -> ZWZ WHEN LAST STMT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3050-SET-REASON.
           EVALUATE ADLM-REASON-CD
               WHEN 'FZV'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KPP' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KPP'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MVN' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MVN'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UVY' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UVY'
                   IF ADLM-LAST-STMT-DATE > 0 AND ADLM-LAST-STMT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWZ' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE REBATE TIER TIER FOR THE CHARGEBK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3060-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REBATE-TIER-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ADLM-CHARGEBK-AMT NOT < WS-REBATE-TIER-LOW(WS-TX)
                   MOVE WS-REBATE-TIER-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT FIN YTD IS FLOORED AT $10.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3070-LIMIT-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-YTD < 10.00
               MOVE 10.00 TO ADLM-AMOUNT-FIN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-AMOUNT-FIN-YTD > 10000.00
               MOVE 10000.00 TO ADLM-AMOUNT-FIN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3080-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TYPE-CD
                   ADD ADLT-RESERVE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLT-RESERVE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GAP YTD AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3090-ACCRUE-GAP.
           IF ADLM-GAP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-GAP-YTD * ADLM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO ADLT-AMOUNT-FIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE BAND TIER FOR THE GAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3100-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
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
      * DAILY INTEREST ON PAYMENT YTD AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3110-ACCRUE-PAYMENT.
           IF ADLM-PAYMENT-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-PAYMENT-YTD * ADLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEALER CD:
      * DRU -> FNF WHEN EXPIRY DATE IS PAST
      * FNF -> GBX WHEN EXPIRY DATE IS PAST
      * GBX -> GTU WHEN EXPIRY DATE IS PAST
      * GTU -> JNE WHEN EXPIRY DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3120-SET-DEALER.
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
      * TRANSLATE TIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3130-DESCRIBE-TIER.
           EVALUATE ADLM-TIER-CD
               WHEN 'EW'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JF'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'UG'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WY'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT FIN AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3140-SPLIT-AMOUNT-FIN.
           COMPUTE WS-WORK-AMT = ADLM-AMOUNT-FIN-AMT / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-AMOUNT-FIN-AMT - (WS-WORK-AMT *
               3)
           COMPUTE ADLT-REBATE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE BAND TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3150-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF WS-WORK-AMT-2 NOT < WS-RESERVE-BAND-LOW(WS-TX)
                   MOVE WS-RESERVE-BAND-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3160-AGE-POST.
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
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-REBATE-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT FIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REBATE AMT.
      *----------------------------------------------------------------*
       3170-PRORATE-REBATE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLT-REBATE-AMT ROUNDED =
                   ADLM-AMOUNT-FIN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLT-REBATE-AMT
           END-IF
           ADD ADLT-REBATE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3180-DESCRIBE-REASON.
           EVALUATE ADLM-REASON-CD
               WHEN 'FZV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KPP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'MVN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UVY'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ZWZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> H WHEN LAST STMT DATE IS PAST
      * H -> J WHEN LAST STMT DATE IS PAST
      * J -> T WHEN LAST STMT DATE IS PAST
      * T -> W WHEN LAST STMT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3190-SET-CYCLE.
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
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3200-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NEXT-DUE-DATE TO DTU-DATE-1
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
      * ACCUMULATE RESERVE AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3210-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLT-CHANNEL-CD
                   ADD ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * REBATE YTD IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3220-LIMIT-REBATE.
           IF ADLM-REBATE-YTD < 0.00
               MOVE 0.00 TO ADLM-REBATE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-REBATE-YTD > 500.00
               MOVE 500.00 TO ADLM-REBATE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEALER CD:
      * DRU -> FNF WHEN POST DATE IS PAST
      * FNF -> GBX WHEN POST DATE IS PAST
      * GBX -> GTU WHEN POST DATE IS PAST
      * GTU -> JNE WHEN POST DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-DEALER.
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
      * DAYS SINCE LAST STMT DATE. MORE THAN 10 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3240-CHECK-LAST-STMT.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * GAP YTD IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3250-LIMIT-GAP.
           IF ADLM-GAP-YTD < 100.00
               MOVE 100.00 TO ADLM-GAP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-GAP-YTD > 500.00
               MOVE 500.00 TO ADLM-GAP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * FZV -> KPP WHEN NEXT REVIEW DATE IS PAST
      * KPP -> MVN WHEN NEXT REVIEW DATE IS PAST
      * MVN -> UVY WHEN NEXT REVIEW DATE IS PAST
      * UVY -> ZWZ WHEN NEXT REVIEW DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3260-SET-REASON.
           EVALUATE ADLM-REASON-CD
               WHEN 'FZV'
                   IF ADLM-NEXT-REVIEW-DATE > 0 AND
                       ADLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'KPP' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KPP'
                   IF ADLM-NEXT-REVIEW-DATE > 0 AND
                       ADLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'MVN' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MVN'
                   IF ADLM-NEXT-REVIEW-DATE > 0 AND
                       ADLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'UVY' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UVY'
                   IF ADLM-NEXT-REVIEW-DATE > 0 AND
                       ADLM-NEXT-REVIEW-DATE < CTL-BUSINESS-DATE
                       MOVE 'ZWZ' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST PAY DATE. MORE THAN 180 CALENDAR DAYS SETS
      * STOP FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3270-CHECK-LAST-PAY.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO ADLM-STOP-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 60 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3280-CHECK-LAST-STMT.
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
           IF WS-DAYS > 60
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * EUA -> HMW WHEN EXPIRY DATE IS PAST
      * HMW -> RCB WHEN EXPIRY DATE IS PAST
      * RCB -> EUA WHEN EXPIRY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3290-SET-STATUS.
           EVALUATE ADLM-STATUS-CD
               WHEN 'EUA'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HMW' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HMW'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RCB' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RCB'
                   IF ADLM-EXPIRY-DATE > 0 AND ADLM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EUA' TO ADLM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-CYCLE.
           IF ADLM-CYCLE-CNT < 9999999
               ADD 1 TO ADLM-CYCLE-CNT
           END-IF
           IF ADLM-CYCLE-CNT NOT < 6
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REBATE FEE ON THE PAYMENT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3310-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ADLM-REBATE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   COMPUTE ADLM-REBATE-AMT ROUNDED = ADLM-PAYMENT-YTD *
                       ADLM-MARGIN / 200
               WHEN ADLM-CYCLE-CD = 'A'
                   MOVE 7.00 TO ADLM-REBATE-AMT
               WHEN ADLM-CYCLE-CD = 'H'
                   MOVE 20.00 TO ADLM-REBATE-AMT
               WHEN OTHER
                   COMPUTE ADLM-REBATE-AMT ROUNDED = ADLM-PAYMENT-YTD *
                       ADLM-MARGIN / 100
           END-EVALUATE
           IF ADLM-REBATE-AMT > 0 AND ADLM-REBATE-AMT < 5.00
               MOVE 5.00 TO ADLM-REBATE-AMT
           END-IF
           IF ADLM-REBATE-AMT > 25.00
               MOVE 25.00 TO ADLM-REBATE-AMT
           END-IF
           ADD ADLM-REBATE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE RESERVE AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3320-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN ADLM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO ADLT-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ADLT-PAYMENT-AMT ROUNDED = ADLM-RESERVE-AMT *
                       ADLM-RATE / 200
               WHEN ADLM-TIER-CD = 'EW'
                   MOVE 15.00 TO ADLT-PAYMENT-AMT
               WHEN ADLM-TIER-CD = 'JF'
                   MOVE 5.00 TO ADLT-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE ADLT-PAYMENT-AMT ROUNDED = ADLM-RESERVE-AMT *
                       ADLM-RATE / 100
           END-EVALUATE
           IF ADLT-PAYMENT-AMT > 0 AND ADLT-PAYMENT-AMT < 1.00
               MOVE 1.00 TO ADLT-PAYMENT-AMT
           END-IF
           IF ADLT-PAYMENT-AMT > 50.00
               MOVE 50.00 TO ADLT-PAYMENT-AMT
           END-IF
           ADD ADLT-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT FIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3330-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-PAYMENT-AMT ROUNDED =
                   ADLT-AMOUNT-FIN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-PAYMENT-AMT
           END-IF
           ADD ADLM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT REBATE YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3340-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = ADLM-REBATE-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-REBATE-YTD - (WS-WORK-AMT * 3)
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR DEALER CD:
      * DRU -> FNF WHEN LAST ACT DATE IS PAST
      * FNF -> GBX WHEN LAST ACT DATE IS PAST
      * GBX -> GTU WHEN LAST ACT DATE IS PAST
      * GTU -> JNE WHEN LAST ACT DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3350-SET-DEALER.
           EVALUATE ADLM-DEALER-CD
               WHEN 'DRU'
                   IF ADLM-LAST-ACT-DATE > 0 AND ADLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNF' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNF'
                   IF ADLM-LAST-ACT-DATE > 0 AND ADLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GBX' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GBX'
                   IF ADLM-LAST-ACT-DATE > 0 AND ADLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GTU' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GTU'
                   IF ADLM-LAST-ACT-DATE > 0 AND ADLM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JNE' TO ADLM-DEALER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE REBATE YTD FEE ON THE AMOUNT FIN YTD.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3360-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ADLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO ADLM-REBATE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ADLM-REBATE-YTD ROUNDED = ADLM-AMOUNT-FIN-YTD
                       * ADLM-FEE-PCT / 200
               WHEN ADLT-CHANNEL-CD = 'BRN'
                   MOVE 20.00 TO ADLM-REBATE-YTD
               WHEN ADLT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO ADLM-REBATE-YTD
               WHEN OTHER
                   COMPUTE ADLM-REBATE-YTD ROUNDED = ADLM-AMOUNT-FIN-YTD
                       * ADLM-FEE-PCT / 100
           END-EVALUATE
           IF ADLM-REBATE-YTD > 0 AND ADLM-REBATE-YTD < 10.00
               MOVE 10.00 TO ADLM-REBATE-YTD
           END-IF
           IF ADLM-REBATE-YTD > 35.00
               MOVE 35.00 TO ADLM-REBATE-YTD
           END-IF
           ADD ADLM-REBATE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT IS FLOORED AT $0.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       3370-LIMIT-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-AMT < 0.00
               MOVE 0.00 TO ADLM-AMOUNT-FIN-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-AMOUNT-FIN-AMT > 50000.00
               MOVE 50000.00 TO ADLM-AMOUNT-FIN-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REBATE FEE ON THE CHARGEBK YTD.
      * WAIVED WHEN REVIEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3380-ASSESS-REVIEW.
           EVALUATE TRUE
               WHEN ADLM-REVIEW-FLAG = 'Y'
                   MOVE ZERO TO ADLT-REBATE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   COMPUTE ADLT-REBATE-AMT ROUNDED = ADLM-CHARGEBK-YTD *
                       ADLM-MARGIN / 200
               WHEN ADLM-REASON-CD = 'FZV'
                   MOVE 5.00 TO ADLT-REBATE-AMT
               WHEN ADLM-REASON-CD = 'KPP'
                   MOVE 7.00 TO ADLT-REBATE-AMT
               WHEN OTHER
                   COMPUTE ADLT-REBATE-AMT ROUNDED = ADLM-CHARGEBK-YTD *
                       ADLM-MARGIN / 100
           END-EVALUATE
           IF ADLT-REBATE-AMT > 0 AND ADLT-REBATE-AMT < 10.00
               MOVE 10.00 TO ADLT-REBATE-AMT
           END-IF
           IF ADLT-REBATE-AMT > 75.00
               MOVE 75.00 TO ADLT-REBATE-AMT
           END-IF
           ADD ADLT-REBATE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE TIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3390-DESCRIBE-TIER.
           EVALUATE ADLM-TIER-CD
               WHEN 'EW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JF'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'KD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'UG'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'WY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3400-COUNT-CYCLE.
           IF ADLM-CYCLE-CNT < 9999999
               ADD 1 TO ADLM-CYCLE-CNT
           END-IF
           IF ADLM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO ADLM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT FEE ON THE REBATE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3410-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ADLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ADLM-PAYMENT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE ADLM-PAYMENT-AMT ROUNDED = ADLT-REBATE-AMT *
                       ADLM-MARGIN / 200
               WHEN ADLM-TIER-CD = 'EW'
                   MOVE 20.00 TO ADLM-PAYMENT-AMT
               WHEN ADLM-TIER-CD = 'JF'
                   MOVE 12.00 TO ADLM-PAYMENT-AMT
               WHEN OTHER
                   COMPUTE ADLM-PAYMENT-AMT ROUNDED = ADLT-REBATE-AMT *
                       ADLM-MARGIN / 100
           END-EVALUATE
           IF ADLM-PAYMENT-AMT > 0 AND ADLM-PAYMENT-AMT < 1.00
               MOVE 1.00 TO ADLM-PAYMENT-AMT
           END-IF
           IF ADLM-PAYMENT-AMT > 35.00
               MOVE 35.00 TO ADLM-PAYMENT-AMT
           END-IF
           ADD ADLM-PAYMENT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3420-FLAG-AMOUNT-FIN.
           IF ADLT-AMOUNT-FIN-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ADLT-AMOUNT-FIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3430-AGE-EXPIRY.
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
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-PAYMENT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3440-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * NOTICE SENT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-EFF.
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
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE YTD BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3460-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-CYCLE-CD
                   ADD ADLM-RESERVE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-RESERVE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REBATE TIER TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3470-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-REBATE-TIER-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ADLM-REBATE-AMT NOT < WS-REBATE-TIER-LOW(WS-TX)
                   MOVE WS-REBATE-TIER-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3480-DESCRIBE-CHANNEL.
           EVALUATE ADLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3490-FLAG-ITEM.
           IF WS-WORK-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3500-AGE-NOTICE.
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
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE GAP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3510-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-PAYMENT-AMT ROUNDED =
                   ADLM-GAP-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-PAYMENT-AMT
           END-IF
           ADD ADLM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3520-LIMIT-PAYMENT.
           IF ADLM-PAYMENT-AMT < 100.00
               MOVE 100.00 TO ADLM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-PAYMENT-AMT > 10000.00
               MOVE 10000.00 TO ADLM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESERVE YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3530-LIMIT-RESERVE.
           IF ADLM-RESERVE-YTD < 0.00
               MOVE 0.00 TO ADLM-RESERVE-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-RESERVE-YTD > 10000.00
               MOVE 10000.00 TO ADLM-RESERVE-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REBATE TIER TIER FOR THE RESERVE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3540-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REBATE-TIER-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ADLM-RESERVE-AMT NOT < WS-REBATE-TIER-LOW(WS-TX)
                   MOVE WS-REBATE-TIER-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REBATE TIER TIER FOR THE GAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3550-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REBATE-TIER-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ADLM-GAP-AMT NOT < WS-REBATE-TIER-LOW(WS-TX)
                   MOVE WS-REBATE-TIER-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REBATE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3560-SPLIT-REBATE.
           COMPUTE WS-WORK-AMT = ADLM-REBATE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = ADLM-REBATE-YTD - (WS-WORK-AMT * 2)
           COMPUTE ADLM-RESERVE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * BK -> PY WHEN LAST MAINT DATE IS PAST
      * PY -> CB WHEN LAST MAINT DATE IS PAST
      * CB -> RB WHEN LAST MAINT DATE IS PAST
      * RB -> GP WHEN LAST MAINT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3570-SET-TRAN.
           EVALUATE ADLT-TRAN-CD
               WHEN 'BK'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'PY' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PY'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'CB' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CB'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'RB' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RB'
                   IF ADLM-LAST-MAINT-DATE > 0 AND ADLM-LAST-MAINT-DATE
                       < CTL-BUSINESS-DATE
                       MOVE 'GP' TO ADLT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3580-AGE-LAST-ACT.
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
                   ADD ADLM-REBATE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-REBATE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD ADLM-REBATE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLM-REBATE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-REBATE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REBATE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3590-FLAG-REBATE.
           IF ADLM-REBATE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ADLM-REBATE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT FIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESERVE AMT.
      *----------------------------------------------------------------*
       3600-PRORATE-RESERVE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLT-RESERVE-AMT ROUNDED =
                   ADLT-AMOUNT-FIN-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLT-RESERVE-AMT
           END-IF
           ADD ADLT-RESERVE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM POST DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3610-AGE-POST.
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
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-AMOUNT-FIN-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3620-COUNT-RETRY.
           IF ADLM-RETRY-CNT < 9999999
               ADD 1 TO ADLM-RETRY-CNT
           END-IF
           IF ADLM-RETRY-CNT NOT < 12
               MOVE 'Y' TO ADLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE YTD BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3630-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLT-TRAN-CD
                   ADD ADLM-RESERVE-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-RESERVE-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3640-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLT-TRAN-CD
                   ADD ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3650-COUNT-SEQ.
           IF ADLT-SEQ < 9999999
               ADD 1 TO ADLT-SEQ
           END-IF
           IF ADLT-SEQ NOT < 6
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GAP AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       3660-PRORATE-GAP.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ADLM-GAP-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE REBATE TIER TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3670-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-REBATE-TIER-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ADLM-PAYMENT-YTD NOT < WS-REBATE-TIER-LOW(WS-TX)
                   MOVE WS-REBATE-TIER-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REBATE TIER TIER FOR THE PAYMENT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3680-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REBATE-TIER-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ADLM-PAYMENT-YTD NOT < WS-REBATE-TIER-LOW(WS-TX)
                   MOVE WS-REBATE-TIER-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-RETRY.
           IF ADLM-RETRY-CNT < 9999999
               ADD 1 TO ADLM-RETRY-CNT
           END-IF
           IF ADLM-RETRY-CNT NOT < 3
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REBATE TIER TIER FOR THE GAP YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3700-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-REBATE-TIER-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ADLM-GAP-YTD NOT < WS-REBATE-TIER-LOW(WS-TX)
                   MOVE WS-REBATE-TIER-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3710-FLAG-PAYMENT.
           IF ADLM-PAYMENT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE ADLM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RESERVE AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3720-LIMIT-RESERVE.
           IF ADLT-RESERVE-AMT < 0.00
               MOVE 0.00 TO ADLT-RESERVE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLT-RESERVE-AMT > 2500.00
               MOVE 2500.00 TO ADLT-RESERVE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3730-CHECK-EFFECTIVE.
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
           IF WS-DAYS > 10
               MOVE 'Y' TO ADLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHARGEBK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT FIN AMT.
      *----------------------------------------------------------------*
       3740-PRORATE-AMOUNT-FIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLT-AMOUNT-FIN-AMT ROUNDED =
                   ADLM-CHARGEBK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLT-AMOUNT-FIN-AMT
           END-IF
           ADD ADLT-AMOUNT-FIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * A -> H WHEN NEXT DUE DATE IS PAST
      * H -> J WHEN NEXT DUE DATE IS PAST
      * J -> T WHEN NEXT DUE DATE IS PAST
      * T -> W WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3750-SET-CYCLE.
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
      * SELECT THE RESERVE BAND TIER FOR THE REBATE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3760-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ADLM-REBATE-AMT NOT < WS-RESERVE-BAND-LOW(WS-TX)
                   MOVE WS-RESERVE-BAND-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E436 REJECT CD REQUIRED
      * E761 REBATE AMT NEGATIVE
      * E640 REBATE AMT OVER 99999
      * E451 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       3770-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-REJECT-CD = SPACES
                   MOVE 'E436' TO WS-REJECT-CODE
               WHEN ADLM-REBATE-AMT < 0
                   MOVE 'E761' TO WS-REJECT-CODE
               WHEN ADLM-REBATE-AMT > 99999.00
                   MOVE 'E640' TO WS-REJECT-CODE
               WHEN ADLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E451' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT RESERVE AMT INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3780-SPLIT-RESERVE.
           COMPUTE WS-WORK-AMT = ADLT-RESERVE-AMT / 12
           COMPUTE WS-WORK-AMT-2 = ADLT-RESERVE-AMT - (WS-WORK-AMT * 12)
           COMPUTE ADLM-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESERVE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3790-PRORATE-RESERVE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-RESERVE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE BAND TIER FOR THE CHARGEBK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3800-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ADLM-CHARGEBK-AMT NOT < WS-RESERVE-BAND-LOW(WS-TX)
                   MOVE WS-RESERVE-BAND-RATE(WS-TX) TO ADLM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE DEALER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-DEALER.
           EVALUATE ADLM-DEALER-CD
               WHEN 'DRU'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FNF'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'GBX'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'GTU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JNE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'KTP'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3820-DESCRIBE-REASON.
           EVALUATE ADLM-REASON-CD
               WHEN 'FZV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KPP'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MVN'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'UVY'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'ZWZ'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RESERVE AMT BY TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3830-ACCUM-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TYPE-CD
                   ADD ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLM-RESERVE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN NEXT DUE DATE IS PAST
      * H -> S WHEN NEXT DUE DATE IS PAST
      * S -> V WHEN NEXT DUE DATE IS PAST
      * V -> W WHEN NEXT DUE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-TYPE.
           EVALUATE ADLM-TYPE-CD
               WHEN 'B'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'S' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'S'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'V' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'V'
                   IF ADLM-NEXT-DUE-DATE > 0 AND ADLM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'W' TO ADLM-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE AMOUNT FIN AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3850-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLT-TRAN-CD
                   ADD ADLT-AMOUNT-FIN-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLT-AMOUNT-FIN-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * FZV -> KPP WHEN EFF DATE IS PAST
      * KPP -> MVN WHEN EFF DATE IS PAST
      * MVN -> UVY WHEN EFF DATE IS PAST
      * UVY -> ZWZ WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3860-SET-REASON.
           EVALUATE ADLM-REASON-CD
               WHEN 'FZV'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KPP' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KPP'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MVN' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MVN'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UVY' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UVY'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWZ' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3870-DESCRIBE-STATUS.
           EVALUATE ADLM-STATUS-CD
               WHEN 'EUA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'HMW'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RCB'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST MAINT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3880-AGE-LAST-MAINT.
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
                   ADD ADLM-REBATE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD ADLM-REBATE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ADLM-REBATE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD ADLM-REBATE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-REBATE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3890-ACCRUE-PAYMENT.
           IF ADLT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-PAYMENT-AMT * ADLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ADLM-RESERVE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3900-FLAG-PAYMENT.
           IF ADLM-PAYMENT-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ADLM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RESERVE AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3910-LIMIT-RESERVE.
           IF ADLT-RESERVE-AMT < 0.00
               MOVE 0.00 TO ADLT-RESERVE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLT-RESERVE-AMT > 500.00
               MOVE 500.00 TO ADLT-RESERVE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3920-FLAG-ITEM.
           IF WS-WORK-AMT-2 > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT-2 TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT FIN YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3930-SPLIT-AMOUNT-FIN.
           COMPUTE WS-WORK-AMT = ADLM-AMOUNT-FIN-YTD / 3
           COMPUTE WS-WORK-AMT-2 = ADLM-AMOUNT-FIN-YTD - (WS-WORK-AMT *
               3)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT FIN YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO AMOUNT FIN AMT.
      *----------------------------------------------------------------*
       3940-PRORATE-AMOUNT-FIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-AMOUNT-FIN-AMT ROUNDED =
                   ADLM-AMOUNT-FIN-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-AMOUNT-FIN-AMT
           END-IF
           ADD ADLM-AMOUNT-FIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT FIN YTD FEE ON THE GAP YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3950-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN ADLM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO ADLM-AMOUNT-FIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ADLM-AMOUNT-FIN-YTD ROUNDED = ADLM-GAP-YTD *
                       ADLM-RATE / 200
               WHEN ADLT-TRAN-CD = 'BK'
                   MOVE 3.00 TO ADLM-AMOUNT-FIN-YTD
               WHEN ADLT-TRAN-CD = 'PY'
                   MOVE 15.00 TO ADLM-AMOUNT-FIN-YTD
               WHEN OTHER
                   COMPUTE ADLM-AMOUNT-FIN-YTD ROUNDED = ADLM-GAP-YTD *
                       ADLM-RATE / 100
           END-EVALUATE
           IF ADLM-AMOUNT-FIN-YTD > 0 AND ADLM-AMOUNT-FIN-YTD < 2.00
               MOVE 2.00 TO ADLM-AMOUNT-FIN-YTD
           END-IF
           IF ADLM-AMOUNT-FIN-YTD > 25.00
               MOVE 25.00 TO ADLM-AMOUNT-FIN-YTD
           END-IF
           ADD ADLM-AMOUNT-FIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE RESERVE BAND TIER FOR THE CHARGEBK YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3960-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-RESERVE-BAND-RATE(1) TO ADLM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 9
               IF ADLM-CHARGEBK-YTD NOT < WS-RESERVE-BAND-LOW(WS-TX)
                   MOVE WS-RESERVE-BAND-RATE(WS-TX) TO ADLM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK 2 FEE ON THE PAYMENT AMT.
      * WAIVED WHEN NOTICE SENT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3970-ASSESS-NOTICE-SENT.
           EVALUATE TRUE
               WHEN ADLM-NOTICE-SENT-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT-2
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ADLT-PAYMENT-AMT *
                       ADLM-RATE / 200
               WHEN ADLM-REASON-CD = 'FZV'
                   MOVE 15.00 TO WS-WORK-AMT-2
               WHEN ADLM-REASON-CD = 'KPP'
                   MOVE 5.00 TO WS-WORK-AMT-2
               WHEN OTHER
                   COMPUTE WS-WORK-AMT-2 ROUNDED = ADLT-PAYMENT-AMT *
                       ADLM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT-2 > 0 AND WS-WORK-AMT-2 < 10.00
               MOVE 10.00 TO WS-WORK-AMT-2
           END-IF
           IF WS-WORK-AMT-2 > 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3980-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ADLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ADLM-AMOUNT-FIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TIER CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3990-DESCRIBE-TIER.
           EVALUATE ADLM-TIER-CD
               WHEN 'EW'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'JF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'KD'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'UG'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'WY'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * CHARGEBK AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4000-FLAG-CHARGEBK.
           IF ADLM-CHARGEBK-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ADLM-CHARGEBK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E815 CUST ID REQUIRED
      * E250 REBATE YTD NEGATIVE
      * E248 REBATE YTD OVER 10000
      * E509 LAST MAINT DATE IN FUTURE
      *----------------------------------------------------------------*
       4010-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-CUST-ID = SPACES
                   MOVE 'E815' TO WS-REJECT-CODE
               WHEN ADLM-REBATE-YTD < 0
                   MOVE 'E250' TO WS-REJECT-CODE
               WHEN ADLM-REBATE-YTD > 10000.00
                   MOVE 'E248' TO WS-REJECT-CODE
               WHEN ADLM-LAST-MAINT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E509' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4020-AGE-EFFECTIVE.
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
                   ADD ADLT-REBATE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD ADLT-REBATE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD ADLT-REBATE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD ADLT-REBATE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLT-REBATE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4030-ACCRUE-FEE-PCT.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ADLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO ADLT-REBATE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4040-COUNT-PAYMENT.
           IF ADLM-PAYMENT-CNT < 9999999
               ADD 1 TO ADLM-PAYMENT-CNT
           END-IF
           IF ADLM-PAYMENT-CNT NOT < 12
               MOVE 'Y' TO ADLM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-60 AND OVER 60 DAYS.
      *----------------------------------------------------------------*
       4050-AGE-NEXT-DUE.
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
      * WORK AMT IS FLOORED AT $0.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4060-LIMIT-ITEM.
           IF WS-WORK-AMT < 0.00
               MOVE 0.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON PAYMENT AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4070-ACCRUE-PAYMENT.
           IF ADLT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-PAYMENT-AMT * ADLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ADLM-PAYMENT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE GAP YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO REBATE AMT.
      *----------------------------------------------------------------*
       4080-PRORATE-REBATE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-REBATE-AMT ROUNDED =
                   ADLM-GAP-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-REBATE-AMT
           END-IF
           ADD ADLM-REBATE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4090-ACCRUE-RESERVE.
           IF ADLM-RESERVE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-RESERVE-AMT * ADLM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4100-COUNT-SEQ.
           IF ADLT-SEQ < 9999999
               ADD 1 TO ADLT-SEQ
           END-IF
           IF ADLT-SEQ NOT < 3
               MOVE 'Y' TO ADLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4110-AGE-NOTICE.
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
                   ADD ADLM-CHARGEBK-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD ADLM-CHARGEBK-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD ADLM-CHARGEBK-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD ADLM-CHARGEBK-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD ADLM-CHARGEBK-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON GAP YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4120-ACCRUE-GAP.
           IF ADLM-GAP-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-GAP-YTD * ADLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ADLT-REBATE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4130-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-PAYMENT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * GAP YTD IS FLOORED AT $0.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4140-LIMIT-GAP.
           IF ADLM-GAP-YTD < 0.00
               MOVE 0.00 TO ADLM-GAP-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-GAP-YTD > 10000.00
               MOVE 10000.00 TO ADLM-GAP-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT FIN YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4150-SPLIT-AMOUNT-FIN.
           COMPUTE WS-WORK-AMT = ADLM-AMOUNT-FIN-YTD / 4
           COMPUTE WS-WORK-AMT-2 = ADLM-AMOUNT-FIN-YTD - (WS-WORK-AMT *
               4)
           COMPUTE ADLM-REBATE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 10 CALENDAR DAYS SETS
      * REVIEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4160-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 10
               MOVE 'Y' TO ADLM-REVIEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TIER CD:
      * EW -> JF WHEN NOTICE DATE IS PAST
      * JF -> KD WHEN NOTICE DATE IS PAST
      * KD -> UG WHEN NOTICE DATE IS PAST
      * UG -> WY WHEN NOTICE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4170-SET-TIER.
           EVALUATE ADLM-TIER-CD
               WHEN 'EW'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JF' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JF'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UG' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UG'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'WY' TO ADLM-TIER-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4180-ACCRUE-RESERVE.
           IF ADLM-RESERVE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-RESERVE-YTD * ADLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE AMOUNT FIN AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO AMOUNT FIN AMT.
      *----------------------------------------------------------------*
       4190-PRORATE-AMOUNT-FIN.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-AMOUNT-FIN-AMT ROUNDED =
                   ADLT-AMOUNT-FIN-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO ADLM-AMOUNT-FIN-AMT
           END-IF
           ADD ADLM-AMOUNT-FIN-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REBATE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4200-ACCRUE-REBATE.
           IF ADLM-REBATE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-REBATE-YTD * ADLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ADLM-AMOUNT-FIN-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4210-COUNT-RETRY.
           IF ADLM-RETRY-CNT < 9999999
               ADD 1 TO ADLM-RETRY-CNT
           END-IF
           IF ADLM-RETRY-CNT NOT < 5
               MOVE 'Y' TO ADLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REBATE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4220-ACCRUE-REBATE.
           IF ADLT-REBATE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLT-REBATE-AMT * ADLM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO ADLM-RESERVE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RESERVE YTD OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4230-FLAG-RESERVE.
           IF ADLM-RESERVE-YTD > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ADLM-RESERVE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       4240-COUNT-EXCEPT.
           IF ADLM-EXCEPT-CNT < 9999999
               ADD 1 TO ADLM-EXCEPT-CNT
           END-IF
           IF ADLM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO ADLM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 45 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4250-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF ADLT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO ADLT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $100.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4260-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 100.00
               MOVE 100.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE AMOUNT FIN YTD FEE ON THE AMOUNT FIN AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       4270-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ADLM-AMOUNT-FIN-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   COMPUTE ADLM-AMOUNT-FIN-YTD ROUNDED =
                       ADLM-AMOUNT-FIN-AMT * ADLM-FEE-PCT / 200
               WHEN ADLM-CYCLE-CD = 'A'
                   MOVE 10.00 TO ADLM-AMOUNT-FIN-YTD
               WHEN ADLM-CYCLE-CD = 'H'
                   MOVE 10.00 TO ADLM-AMOUNT-FIN-YTD
               WHEN OTHER
                   COMPUTE ADLM-AMOUNT-FIN-YTD ROUNDED =
                       ADLM-AMOUNT-FIN-AMT * ADLM-FEE-PCT / 100
           END-EVALUATE
           IF ADLM-AMOUNT-FIN-YTD > 0 AND ADLM-AMOUNT-FIN-YTD < 10.00
               MOVE 10.00 TO ADLM-AMOUNT-FIN-YTD
           END-IF
           IF ADLM-AMOUNT-FIN-YTD > 100.00
               MOVE 100.00 TO ADLM-AMOUNT-FIN-YTD
           END-IF
           ADD ADLM-AMOUNT-FIN-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT AMOUNT FIN AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-AMOUNT-FIN.
           COMPUTE WS-WORK-AMT = ADLT-AMOUNT-FIN-AMT / 2
           COMPUTE WS-WORK-AMT-2 = ADLT-AMOUNT-FIN-AMT - (WS-WORK-AMT *
               2)
           COMPUTE ADLM-REBATE-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REBATE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CHARGEBK YTD.
      *----------------------------------------------------------------*
       4290-PRORATE-CHARGEBK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-CHARGEBK-YTD ROUNDED =
                   ADLM-REBATE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-CHARGEBK-YTD
           END-IF
           ADD ADLM-CHARGEBK-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4300-DESCRIBE-TYPE.
           EVALUATE ADLM-TYPE-CD
               WHEN 'B'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'H'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'S'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'V'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'W'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY CYCLE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4310-ACCUM-CYCLE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-CYCLE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-CYCLE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN NOTICE DATE IS PAST
      * BAT -> MAL WHEN NOTICE DATE IS PAST
      * MAL -> TEL WHEN NOTICE DATE IS PAST
      * TEL -> ONL WHEN NOTICE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4320-SET-CHANNEL.
           EVALUATE ADLT-CHANNEL-CD
               WHEN 'BRN'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF ADLM-NOTICE-DATE > 0 AND ADLM-NOTICE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO ADLT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REBATE AMT IS FLOORED AT $50.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       4330-LIMIT-REBATE.
           IF ADLT-REBATE-AMT < 50.00
               MOVE 50.00 TO ADLT-REBATE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLT-REBATE-AMT > 1000.00
               MOVE 1000.00 TO ADLT-REBATE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVIEW FLAG.
      *----------------------------------------------------------------*
       4340-COUNT-EXCEPT.
           IF ADLM-EXCEPT-CNT < 9999999
               ADD 1 TO ADLM-EXCEPT-CNT
           END-IF
           IF ADLM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO ADLM-REVIEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E926 MEMO REQUIRED
      * E456 RESERVE YTD NEGATIVE
      * E200 RESERVE YTD OVER 1000000
      * E940 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4350-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-MEMO = SPACES
                   MOVE 'E926' TO WS-REJECT-CODE
               WHEN ADLM-RESERVE-YTD < 0
                   MOVE 'E456' TO WS-REJECT-CODE
               WHEN ADLM-RESERVE-YTD > 1000000.00
                   MOVE 'E200' TO WS-REJECT-CODE
               WHEN ADLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E940' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REBATE AMT IS FLOORED AT $0.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4360-LIMIT-REBATE.
           IF ADLM-REBATE-AMT < 0.00
               MOVE 0.00 TO ADLM-REBATE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-REBATE-AMT > 2500.00
               MOVE 2500.00 TO ADLM-REBATE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4370-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   ADLT-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       4380-COUNT-SEQ.
           IF ADLT-SEQ < 9999999
               ADD 1 TO ADLT-SEQ
           END-IF
           IF ADLT-SEQ NOT < 3
               MOVE 'Y' TO ADLM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RESERVE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RESERVE AMT.
      *----------------------------------------------------------------*
       4390-PRORATE-RESERVE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-RESERVE-AMT ROUNDED =
                   ADLM-RESERVE-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-RESERVE-AMT
           END-IF
           ADD ADLM-RESERVE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * FZV -> KPP WHEN EFF DATE IS PAST
      * KPP -> MVN WHEN EFF DATE IS PAST
      * MVN -> UVY WHEN EFF DATE IS PAST
      * UVY -> ZWZ WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4400-SET-REASON.
           EVALUATE ADLM-REASON-CD
               WHEN 'FZV'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KPP' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KPP'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MVN' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MVN'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UVY' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UVY'
                   IF ADLT-EFF-DATE > 0 AND ADLT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWZ' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E159 REJECT CD REQUIRED
      * E913 PAYMENT YTD NEGATIVE
      * E429 PAYMENT YTD OVER 10000
      * E174 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4410-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-REJECT-CD = SPACES
                   MOVE 'E159' TO WS-REJECT-CODE
               WHEN ADLM-PAYMENT-YTD < 0
                   MOVE 'E913' TO WS-REJECT-CODE
               WHEN ADLM-PAYMENT-YTD > 10000.00
                   MOVE 'E429' TO WS-REJECT-CODE
               WHEN ADLM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E174' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       4420-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-PAYMENT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * AMOUNT FIN YTD IS FLOORED AT $50.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4430-LIMIT-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-YTD < 50.00
               MOVE 50.00 TO ADLM-AMOUNT-FIN-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-AMOUNT-FIN-YTD > 50000.00
               MOVE 50000.00 TO ADLM-AMOUNT-FIN-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4440-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF ADLM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE ADLM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO ADLM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4450-COUNT-RETRY.
           IF ADLM-RETRY-CNT < 9999999
               ADD 1 TO ADLM-RETRY-CNT
           END-IF
           IF ADLM-RETRY-CNT NOT < 12
               MOVE 'Y' TO ADLM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AMOUNT FIN AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4460-FLAG-AMOUNT-FIN.
           IF ADLM-AMOUNT-FIN-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ADLM-AMOUNT-FIN-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO CHARGEBK YTD.
      *----------------------------------------------------------------*
       4470-PRORATE-CHARGEBK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE ADLM-CHARGEBK-YTD ROUNDED =
                   ADLT-PAYMENT-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO ADLM-CHARGEBK-YTD
           END-IF
           ADD ADLM-CHARGEBK-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REBATE AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4480-ACCRUE-REBATE.
           IF ADLM-REBATE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-REBATE-AMT * ADLM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO ADLT-REBATE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4490-DESCRIBE-CHANNEL.
           EVALUATE ADLT-CHANNEL-CD
               WHEN 'BRN'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BAT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'MAL'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'TEL'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'ONL'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REBATE YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4500-FLAG-REBATE.
           IF ADLM-REBATE-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE ADLM-REBATE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RESERVE AMT IS FLOORED AT $25.00 AND CAPPED AT $50000.00.
      *----------------------------------------------------------------*
       4510-LIMIT-RESERVE.
           IF ADLM-RESERVE-AMT < 25.00
               MOVE 25.00 TO ADLM-RESERVE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF ADLM-RESERVE-AMT > 50000.00
               MOVE 50000.00 TO ADLM-RESERVE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETRY CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4520-COUNT-RETRY.
           IF ADLM-RETRY-CNT < 9999999
               ADD 1 TO ADLM-RETRY-CNT
           END-IF
           IF ADLM-RETRY-CNT NOT < 12
               MOVE 'Y' TO ADLT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4530-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * ADLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ADLM-RESERVE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E356 REJECT CD REQUIRED
      * E902 RESERVE AMT NEGATIVE
      * E560 RESERVE AMT OVER 99999
      * E444 LAST PAY DATE IN FUTURE
      *----------------------------------------------------------------*
       4540-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-REJECT-CD = SPACES
                   MOVE 'E356' TO WS-REJECT-CODE
               WHEN ADLM-RESERVE-AMT < 0
                   MOVE 'E902' TO WS-REJECT-CODE
               WHEN ADLM-RESERVE-AMT > 99999.00
                   MOVE 'E560' TO WS-REJECT-CODE
               WHEN ADLM-LAST-PAY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E444' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * FZV -> KPP WHEN EFFECTIVE DATE IS PAST
      * KPP -> MVN WHEN EFFECTIVE DATE IS PAST
      * MVN -> UVY WHEN EFFECTIVE DATE IS PAST
      * UVY -> ZWZ WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4550-SET-REASON.
           EVALUATE ADLM-REASON-CD
               WHEN 'FZV'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KPP' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KPP'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MVN' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MVN'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UVY' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UVY'
                   IF ADLM-EFFECTIVE-DATE > 0 AND ADLM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ZWZ' TO ADLM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E224 ADDR REQUIRED
      * E070 RESERVE YTD NEGATIVE
      * E763 RESERVE YTD OVER 99999
      * E104 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4560-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLM-ADDR = SPACES
                   MOVE 'E224' TO WS-REJECT-CODE
               WHEN ADLM-RESERVE-YTD < 0
                   MOVE 'E070' TO WS-REJECT-CODE
               WHEN ADLM-RESERVE-YTD > 99999.00
                   MOVE 'E763' TO WS-REJECT-CODE
               WHEN ADLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E104' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REBATE YTD AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4570-ACCRUE-REBATE.
           IF ADLM-REBATE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-REBATE-YTD * ADLM-RATE / 100 / 365
               ADD WS-WORK-AMT TO ADLT-AMOUNT-FIN-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REBATE YTD FEE ON THE AMOUNT FIN AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       4580-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN ADLT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO ADLM-REBATE-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-OVERRIDE-FLAG = 'Y'
                   COMPUTE ADLM-REBATE-YTD ROUNDED = ADLT-AMOUNT-FIN-AMT
                       * ADLM-RATE / 200
               WHEN ADLT-CHANNEL-CD = 'BRN'
                   MOVE 15.00 TO ADLM-REBATE-YTD
               WHEN ADLT-CHANNEL-CD = 'BAT'
                   MOVE 12.00 TO ADLM-REBATE-YTD
               WHEN OTHER
                   COMPUTE ADLM-REBATE-YTD ROUNDED = ADLT-AMOUNT-FIN-AMT
                       * ADLM-RATE / 100
           END-EVALUATE
           IF ADLM-REBATE-YTD > 0 AND ADLM-REBATE-YTD < 1.00
               MOVE 1.00 TO ADLM-REBATE-YTD
           END-IF
           IF ADLM-REBATE-YTD > 250.00
               MOVE 250.00 TO ADLM-REBATE-YTD
           END-IF
           ADD ADLM-REBATE-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PAYMENT YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4590-FLAG-PAYMENT.
           IF ADLM-PAYMENT-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE ADLM-PAYMENT-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REBATE TIER TIER FOR THE GAP AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4600-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REBATE-TIER-RATE(1) TO ADLM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF ADLM-GAP-AMT NOT < WS-REBATE-TIER-LOW(WS-TX)
                   MOVE WS-REBATE-TIER-RATE(WS-TX) TO ADLM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF ADLM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO ADLM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TYPE CD:
      * B -> H WHEN LAST ACT DATE IS PAST
      * H -> S WHEN LAST ACT DATE IS PAST
      * S -> V WHEN LAST ACT DATE IS PAST
      * V -> W WHEN LAST ACT DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4610-SET-TYPE.
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
      * ASSESS THE GAP YTD FEE ON THE WORK AMT 2.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       4620-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN ADLM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO ADLM-GAP-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN ADLM-PREFERRED-FLAG = 'Y'
                   COMPUTE ADLM-GAP-YTD ROUNDED = WS-WORK-AMT-2 *
                       ADLM-RATE / 200
               WHEN ADLM-STATUS-CD = 'EUA'
                   MOVE 15.00 TO ADLM-GAP-YTD
               WHEN ADLM-STATUS-CD = 'HMW'
                   MOVE 12.00 TO ADLM-GAP-YTD
               WHEN OTHER
                   COMPUTE ADLM-GAP-YTD ROUNDED = WS-WORK-AMT-2 *
                       ADLM-RATE / 100
           END-EVALUATE
           IF ADLM-GAP-YTD > 0 AND ADLM-GAP-YTD < 5.00
               MOVE 5.00 TO ADLM-GAP-YTD
           END-IF
           IF ADLM-GAP-YTD > 75.00
               MOVE 75.00 TO ADLM-GAP-YTD
           END-IF
           ADD ADLM-GAP-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT REVIEW DATE. MORE THAN 45 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4630-CHECK-NEXT-REVIEW.
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
           IF WS-DAYS > 45
               MOVE 'Y' TO ADLM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E744 REJECT CD REQUIRED
      * E450 WORK AMT NEGATIVE
      * E194 WORK AMT OVER 10000
      * E527 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       4640-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN ADLT-REJECT-CD = SPACES
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E450' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 10000.00
                   MOVE 'E194' TO WS-REJECT-CODE
               WHEN ADLM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E527' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT AMT BY TIER CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4650-ACCUM-TIER.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = ADLM-TIER-CD
                   ADD ADLT-PAYMENT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE ADLM-TIER-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE ADLT-PAYMENT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RESERVE YTD AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4660-ACCRUE-RESERVE.
           IF ADLM-RESERVE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   ADLM-RESERVE-YTD * ADLM-RATE / 100 / 360
               ADD WS-WORK-AMT TO ADLM-REBATE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PASS THE ADL FIELDS TO AULIM AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-AULIM.
           INITIALIZE AULIM-PARMS
           MOVE ADLM-REBATE-YTD
             TO AULIM-REBATE-YTD
           MOVE ADLM-CHARGEBK-YTD
             TO AULIM-CHARGEBK-YTD
           MOVE ADLM-REBATE-AMT
             TO AULIM-REBATE-AMT
           MOVE ADLM-PAYMENT-AMT
             TO AULIM-PAYMENT-AMT
           MOVE ADLM-CHARGEBK-AMT
             TO AULIM-CHARGEBK-AMT
           MOVE ADLM-AMOUNT-FIN-YTD
             TO AULIM-AMOUNT-FIN-YTD
           MOVE ADLM-LAST-MAINT-DATE
             TO AULIM-LAST-MAINT-DATE
           MOVE ADLM-LAST-STMT-DATE
             TO AULIM-LAST-STMT-DATE
           MOVE ADLM-NEXT-DUE-DATE
             TO AULIM-NEXT-DUE-DATE
           MOVE ADLM-LAST-PAY-DATE
             TO AULIM-LAST-PAY-DATE
           MOVE ADLM-TIER-CD
             TO AULIM-TIER-CD
           MOVE ADLM-TYPE-CD
             TO AULIM-TYPE-CD
           MOVE ADLM-STATUS-CD
             TO AULIM-STATUS-CD
           MOVE ADLM-DEALER-CD
             TO AULIM-DEALER-CD
           MOVE ADLM-EXEMPT-FLAG
             TO AULIM-EXEMPT-FLAG
           MOVE ADLM-STOP-FLAG
             TO AULIM-STOP-FLAG
           MOVE ADLM-OVERRIDE-FLAG
             TO AULIM-OVERRIDE-FLAG
           MOVE ADLM-CYCLE-CNT
             TO AULIM-CYCLE-CNT
           MOVE ADLM-PAYMENT-CNT
             TO AULIM-PAYMENT-CNT
           MOVE ADLM-MARGIN
             TO AULIM-MARGIN
           MOVE ADLM-FEE-PCT
             TO AULIM-FEE-PCT
           MOVE ADLM-ADDR
             TO AULIM-ADDR
           MOVE ADLM-NAME
             TO AULIM-NAME
           CALL 'AULIM' USING AULIM-PARMS
           EVALUATE AULIM-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE AULIM-REBATE-YTD
                     TO ADLM-REBATE-YTD
                   MOVE AULIM-REBATE-AMT
                     TO ADLM-REBATE-AMT
                   MOVE AULIM-PAYMENT-AMT
                     TO ADLM-PAYMENT-AMT
                   MOVE AULIM-CHARGEBK-AMT
                     TO ADLM-CHARGEBK-AMT
                   MOVE AULIM-AMOUNT-FIN-YTD
                     TO ADLM-AMOUNT-FIN-YTD
                   MOVE AULIM-STOP-FLAG
                     TO ADLM-STOP-FLAG
                   MOVE AULIM-OVERRIDE-FLAG
                     TO ADLM-OVERRIDE-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RLIM' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-AULIM' TO ABND-PARAGRAPH
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
           DISPLAY 'AULSETL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'AULSETL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
