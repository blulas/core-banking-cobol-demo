      *================================================================*
      * PROGRAM  : BPYRCON                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : BILL PAYMENT SERVICE - RECONCILIATION. READS THE BPY*
      *            TRANSACTIONS, UPDATES THE BPY MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. CONSUMER BILL PAYMENT PAYEE     *
      *            MAINTENANCE AND CHECK OR EFT DISBURSEMENT           *
      * INPUT    : BPYMST (KSDS), BPYTRN, BANKCTL                      *
      * OUTPUT   : BPYOUT, BPYRCRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, BPSTAT, BPPRC           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-07-09 PQR  ORIGINAL                                       *
      * 1992-11-19 CRP  FEE ROUNDING - HALF UP REQ8621                 *
      * 1992-02-13 GAF  PAYMENT TABLE EXPANDED INC1617                 *
      * 1994-01-04 RJT  PAYMENT RERUN PROTECTION INC3497               *
      * 1996-09-28 TWH  NEW REFUND REPORT COLUMNS INC4205              *
      * 1996-10-03 DPS  PAYMENT RERUN PROTECTION PRB2583               *
      * 1999-05-28 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-05-08 LMB  FEE CUTOFF MOVED PRB1753                       *
      * 2003-11-16 GAF  REFUND RERUN PROTECTION CHG0459                *
      * 2006-10-03 RJT  NEW RETURNED REPORT COLUMNS REQ0346            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BPYRCON.
       AUTHOR. D P SHAW.

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
           SELECT REPORT-FILE ASSIGN TO "BPYRCRPT"
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
                                           VALUE 14.000000.
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
      * WS-REFUND-TIER - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-REFUND-TIER-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000752500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000123000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006000000000600500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00006500000000771000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007000000000502000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008000000000614500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00009000000000584500'.
       01  WS-REFUND-TIER-TBL REDEFINES WS-REFUND-TIER-VALUES.
           05  WS-REFUND-TIER-ROW OCCURS 7 TIMES.
               10  WS-REFUND-TIER-LOW      PIC 9(09)V99.
               10  WS-REFUND-TIER-RATE     PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-FEE-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000238000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000100000000241000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000150000000853500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000400000000075500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00000900000000055000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00001000000000886500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002000000000110000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002250000000153000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002300000000412000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00002550000000897500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007550000000602000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00008550000000214500'.
       01  WS-FEE-BAND-TBL REDEFINES WS-FEE-BAND-VALUES.
           05  WS-FEE-BAND-ROW OCCURS 12 TIMES.
               10  WS-FEE-BAND-LOW         PIC 9(09)V99.
               10  WS-FEE-BAND-RATE        PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  BPYRCON  BILL PAYMENT SERVICE'.
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
       COPY BPSTATP.
       COPY BPPRCP.

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
               PERFORM 3000-SET-STATUS
               PERFORM 3010-COUNT-EXCEPT
               PERFORM 3020-COUNT-CYCLE
               PERFORM 3030-ACCRUE-RETURNED
               PERFORM 3040-CHECK-OPEN
               PERFORM 3050-DESCRIBE-STATUS
               PERFORM 3060-EDIT-CONTACT
               PERFORM 3070-EDIT-MEMO
               PERFORM 3080-LIMIT-PAYMENT
               PERFORM 3090-LIMIT-RETURNED
               PERFORM 3100-SELECT-RATE
               PERFORM 3110-FLAG-FEE
               PERFORM 3120-ACCRUE-REFUND
               PERFORM 3130-SELECT-RATE
               PERFORM 3140-EDIT-REJECT
               PERFORM 3150-DESCRIBE-REASON
               PERFORM 3160-CHECK-EFF
               PERFORM 3170-SET-SOURCE
               PERFORM 3180-ACCUM-CHANNEL
               PERFORM 3190-ASSESS-OVERRIDE
               PERFORM 3200-ACCUM-SOURCE
               PERFORM 3210-SELECT-RATE
               PERFORM 3220-ACCUM-SOURCE
               PERFORM 3230-SET-CHANNEL
               PERFORM 3240-ACCUM-STATUS
               PERFORM 3250-SET-REMIT-METHOD
               PERFORM 3260-DESCRIBE-SOURCE
               PERFORM 3270-FLAG-REFUND
               PERFORM 3280-PRORATE-REFUND
               PERFORM 3290-FLAG-RETURNED
               PERFORM 3300-DESCRIBE-PAYEE-TYPE
               PERFORM 3310-AGE-CLOSE
               PERFORM 3320-SELECT-RATE
               PERFORM 3330-AGE-LAST-STMT
               PERFORM 3340-ASSESS-HOLD
               PERFORM 3350-DESCRIBE-SOURCE
               PERFORM 3360-SET-CYCLE
               PERFORM 3370-ACCUM-REASON
               PERFORM 3380-CHECK-LAST-STMT
               PERFORM 3390-ACCUM-SOURCE
               PERFORM 3400-DESCRIBE-TRAN
               PERFORM 3410-SET-CYCLE
               PERFORM 3420-SELECT-PENALTY-PCT
               PERFORM 3430-SET-REASON
               PERFORM 3440-PRORATE-ITEM
               PERFORM 3450-CHECK-EFF
               PERFORM 3460-FLAG-PAYMENT
               PERFORM 3470-AGE-LAST-ACT
               PERFORM 3480-FLAG-ITEM
               PERFORM 3490-EDIT-ADDR
               PERFORM 3500-ASSESS-EXEMPT
               PERFORM 3510-FLAG-FEE
               PERFORM 3520-AGE-EXPIRY
               PERFORM 3530-ACCRUE-RATE
               PERFORM 3540-SPLIT-PAYMENT
               PERFORM 3550-LIMIT-FEE
               PERFORM 3560-DESCRIBE-STATUS
               PERFORM 3570-ASSESS-PREFERRED
               PERFORM 3580-SELECT-RATE
               PERFORM 3590-SET-CHANNEL
               PERFORM 3600-CHECK-POST
               PERFORM 3610-ACCRUE-PAYMENT
               PERFORM 3620-PRORATE-PAYMENT
               PERFORM 3630-SET-REASON
               PERFORM 3640-LIMIT-RETURNED
               PERFORM 3650-CHECK-CLOSE
               PERFORM 3660-COUNT-ITEM
               PERFORM 3670-ASSESS-EXEMPT
               PERFORM 3680-EDIT-REF-NO
               PERFORM 3690-COUNT-ITEM
               PERFORM 3700-ASSESS-AUTO-RENEW
               PERFORM 3710-SELECT-RATE
               PERFORM 3720-ASSESS-REVERSAL
               PERFORM 3730-SPLIT-FEE
               PERFORM 3740-FLAG-RETURNED
               PERFORM 3750-ACCUM-PAYEE-TYPE
               PERFORM 3760-CHECK-OPEN
               PERFORM 3770-PRORATE-RETURNED
               PERFORM 3780-DESCRIBE-CYCLE
               PERFORM 3790-SET-CYCLE
               PERFORM 3800-LIMIT-RETURNED
               PERFORM 3810-DESCRIBE-CYCLE
               PERFORM 3820-COUNT-EXCEPT
               PERFORM 3830-DESCRIBE-TRAN
               PERFORM 3840-SET-TRAN
               PERFORM 3850-EDIT-ADDR
               PERFORM 3860-ACCRUE-RETURNED
               PERFORM 3870-SELECT-RATE
               PERFORM 3880-ACCRUE-FEE
               PERFORM 3890-ASSESS-REVERSAL
               PERFORM 3900-EDIT-REF-NO
               PERFORM 3910-LIMIT-REFUND
               PERFORM 3920-ASSESS-OVERRIDE
               PERFORM 3930-COUNT-ITEM
               PERFORM 3940-ASSESS-HOLD
               PERFORM 3950-ASSESS-SUSPEND
               PERFORM 3960-PRORATE-RETURNED
               PERFORM 3970-EDIT-OPERATOR
               PERFORM 3980-SET-STATUS
               PERFORM 3990-COUNT-SEQ
               PERFORM 4000-AGE-OPEN
               PERFORM 4010-ACCRUE-RETURNED
               PERFORM 4020-EDIT-OPERATOR
               PERFORM 4030-DESCRIBE-TRAN
               PERFORM 4040-SELECT-RATE
               PERFORM 4050-FLAG-FEE
               PERFORM 4060-FLAG-RETURNED
               PERFORM 4070-SELECT-RATE
               PERFORM 4080-ACCRUE-RETURNED
               PERFORM 4090-ACCUM-CHANNEL
               PERFORM 4100-SET-PAYEE-TYPE
               PERFORM 4110-COUNT-EXCEPT
               PERFORM 4120-AGE-EXPIRY
               PERFORM 4130-CHECK-EFF
               PERFORM 4140-ACCUM-SOURCE
               PERFORM 4150-CHECK-MATURITY
               PERFORM 4160-SELECT-MARGIN
               PERFORM 4170-AGE-NOTICE
               PERFORM 4180-PRORATE-RETURNED
               PERFORM 4190-FLAG-RETURNED
               PERFORM 4200-EDIT-CONTACT
               PERFORM 4210-LIMIT-RETURNED
               PERFORM 4220-COUNT-SEQ
               PERFORM 4230-LIMIT-REFUND
               PERFORM 4240-CHECK-NEXT-DUE
               PERFORM 4250-LIMIT-ITEM
               PERFORM 4260-AGE-OPEN
               PERFORM 4270-SELECT-PENALTY-PCT
               PERFORM 4280-SPLIT-FEE
               PERFORM 4290-SPLIT-PAYMENT
               PERFORM 4300-AGE-LAST-ACT
               PERFORM 4310-FLAG-RETURNED
               PERFORM 4320-PRORATE-RETURNED
               PERFORM 4330-ASSESS-EXEMPT
               PERFORM 4340-SPLIT-PAYMENT
               PERFORM 4350-COUNT-ITEM
               PERFORM 4360-ACCUM-TRAN
               PERFORM 4370-EDIT-ADDR
               PERFORM 4380-ACCRUE-REFUND
               PERFORM 4390-AGE-CLOSE
               PERFORM 4400-ACCUM-STATUS
               PERFORM 4410-SET-CHANNEL
               PERFORM 4420-DESCRIBE-REASON
               PERFORM 4430-CHECK-CLOSE
               PERFORM 4440-CHECK-MATURITY
               PERFORM 4450-DESCRIBE-REMIT-METHOD
               PERFORM 4460-LIMIT-PAYMENT
               PERFORM 4470-COUNT-SEQ
               PERFORM 4480-PRORATE-RETURNED
               PERFORM 7020-CALL-BPSTAT
               PERFORM 7010-CALL-BPPRC
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
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN CLOSE DATE IS PAST
      * LR -> NZ WHEN CLOSE DATE IS PAST
      * NZ -> RV WHEN CLOSE DATE IS PAST
      * RV -> TM WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3000-SET-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LR' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LR'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS OVERRIDE FLAG.
      *----------------------------------------------------------------*
       3010-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 3
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * CYCLE CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-CYCLE.
           IF BPYM-CYCLE-CNT < 9999999
               ADD 1 TO BPYM-CYCLE-CNT
           END-IF
           IF BPYM-CYCLE-CNT NOT < 12
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETURNED AMT AT MARGIN,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3030-ACCRUE-RETURNED.
           IF BPYM-RETURNED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-RETURNED-AMT * BPYM-MARGIN / 100 / 360
               ADD WS-WORK-AMT TO BPYM-REFUND-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 180 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3040-CHECK-OPEN.
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
           IF WS-DAYS > 180
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3050-DESCRIBE-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'TM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E619 CONTACT REQUIRED
      * E022 REFUND AMT NEGATIVE
      * E007 REFUND AMT OVER 250000
      * E482 LAST STMT DATE IN FUTURE
      *----------------------------------------------------------------*
       3060-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-CONTACT = SPACES
                   MOVE 'E619' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-AMT < 0
                   MOVE 'E022' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-AMT > 250000.00
                   MOVE 'E007' TO WS-REJECT-CODE
               WHEN BPYM-LAST-STMT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E482' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E694 MEMO REQUIRED
      * E150 RETURNED YTD NEGATIVE
      * E194 RETURNED YTD OVER 1000000
      * E876 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3070-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-MEMO = SPACES
                   MOVE 'E694' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-YTD < 0
                   MOVE 'E150' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-YTD > 1000000.00
                   MOVE 'E194' TO WS-REJECT-CODE
               WHEN BPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E876' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3080-LIMIT-PAYMENT.
           IF BPYM-PAYMENT-AMT < 0.00
               MOVE 0.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-PAYMENT-AMT > 1000.00
               MOVE 1000.00 TO BPYM-PAYMENT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED AMT IS FLOORED AT $10.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3090-LIMIT-RETURNED.
           IF BPYM-RETURNED-AMT < 10.00
               MOVE 10.00 TO BPYM-RETURNED-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-RETURNED-AMT > 1000.00
               MOVE 1000.00 TO BPYM-RETURNED-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3100-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPYM-PAYMENT-AMT NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3110-FLAG-FEE.
           IF BPYT-FEE-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYT-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3120-ACCRUE-REFUND.
           IF BPYT-REFUND-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-REFUND-AMT * BPYM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO BPYM-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3130-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPYT-PAYMENT-AMT NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E686 REJECT CD REQUIRED
      * E447 REFUND YTD NEGATIVE
      * E319 REFUND YTD OVER 50000
      * E191 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       3140-EDIT-REJECT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-REJECT-CD = SPACES
                   MOVE 'E686' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-YTD < 0
                   MOVE 'E447' TO WS-REJECT-CODE
               WHEN BPYM-REFUND-YTD > 50000.00
                   MOVE 'E319' TO WS-REJECT-CODE
               WHEN BPYT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E191' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3150-DESCRIBE-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'BX'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'GM'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'KU'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'RR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WJ'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 365 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3160-CHECK-EFF.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR SOURCE CD:
      * CT -> FV WHEN EFFECTIVE DATE IS PAST
      * FV -> HD WHEN EFFECTIVE DATE IS PAST
      * HD -> KD WHEN EFFECTIVE DATE IS PAST
      * KD -> NE WHEN EFFECTIVE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3170-SET-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FV' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FV'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'HD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'HD'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KD' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KD'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NE' TO BPYM-SOURCE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3180-ACCUM-CHANNEL.
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
      * ASSESS THE RETURNED FEE ON THE RETURNED AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3190-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BPYT-RETURNED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-HOLD-FLAG = 'Y'
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = BPYM-RETURNED-AMT
                       * BPYM-RATE / 200
               WHEN BPYM-CYCLE-CD = 'DA'
                   MOVE 20.00 TO BPYT-RETURNED-AMT
               WHEN BPYM-CYCLE-CD = 'FP'
                   MOVE 3.00 TO BPYT-RETURNED-AMT
               WHEN OTHER
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = BPYM-RETURNED-AMT
                       * BPYM-RATE / 100
           END-EVALUATE
           IF BPYT-RETURNED-AMT > 0 AND BPYT-RETURNED-AMT < 5.00
               MOVE 5.00 TO BPYT-RETURNED-AMT
           END-IF
           IF BPYT-RETURNED-AMT > 250.00
               MOVE 250.00 TO BPYT-RETURNED-AMT
           END-IF
           ADD BPYT-RETURNED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3200-ACCUM-SOURCE.
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
      * SELECT THE FEE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3210-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3220-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-SOURCE-CD
                   ADD BPYM-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN LAST ACT DATE IS PAST
      * BAT -> MAL WHEN LAST ACT DATE IS PAST
      * MAL -> TEL WHEN LAST ACT DATE IS PAST
      * TEL -> ONL WHEN LAST ACT DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3230-SET-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF BPYM-LAST-ACT-DATE > 0 AND BPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BPYM-LAST-ACT-DATE > 0 AND BPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BPYM-LAST-ACT-DATE > 0 AND BPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BPYM-LAST-ACT-DATE > 0 AND BPYM-LAST-ACT-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT 2 BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3240-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-STATUS-CD
                   ADD WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT-2 TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REMIT METHOD CD:
      * CGV -> FNS WHEN OPEN DATE IS PAST
      * FNS -> NMS WHEN OPEN DATE IS PAST
      * NMS -> TAF WHEN OPEN DATE IS PAST
      * TAF -> VNK WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3250-SET-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FNS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FNS'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NMS' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NMS'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TAF' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TAF'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VNK' TO BPYM-REMIT-METHOD-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3260-DESCRIBE-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FV'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'HD'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'KD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'NE'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NJ'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'PS'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * REFUND YTD OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3270-FLAG-REFUND.
           IF BPYM-REFUND-YTD > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-REFUND-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE REFUND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REFUND AMT.
      *----------------------------------------------------------------*
       3280-PRORATE-REFUND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-REFUND-AMT ROUNDED =
                   BPYT-REFUND-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-REFUND-AMT
           END-IF
           ADD BPYM-REFUND-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETURNED AMT OVER $10000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3290-FLAG-RETURNED.
           IF BPYM-RETURNED-AMT > 10000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-RETURNED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE PAYEE TYPE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3300-DESCRIBE-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'FR'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LL'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'US'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'XM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YE'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3310-AGE-CLOSE.
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
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3320-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPYM-FEE-YTD NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3330-AGE-LAST-STMT.
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
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE PAYMENT YTD FEE ON THE REFUND AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3340-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPYM-PAYMENT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   COMPUTE BPYM-PAYMENT-YTD ROUNDED = BPYM-REFUND-AMT *
                       BPYM-RATE / 200
               WHEN BPYT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO BPYM-PAYMENT-YTD
               WHEN BPYT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO BPYM-PAYMENT-YTD
               WHEN OTHER
                   COMPUTE BPYM-PAYMENT-YTD ROUNDED = BPYM-REFUND-AMT *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-PAYMENT-YTD > 0 AND BPYM-PAYMENT-YTD < 10.00
               MOVE 10.00 TO BPYM-PAYMENT-YTD
           END-IF
           IF BPYM-PAYMENT-YTD > 35.00
               MOVE 35.00 TO BPYM-PAYMENT-YTD
           END-IF
           ADD BPYM-PAYMENT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * TRANSLATE SOURCE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3350-DESCRIBE-SOURCE.
           EVALUATE BPYM-SOURCE-CD
               WHEN 'CT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FV'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'HD'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KD'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'NE'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NJ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN OPEN DATE IS PAST
      * FP -> JH WHEN OPEN DATE IS PAST
      * JH -> JV WHEN OPEN DATE IS PAST
      * JV -> RM WHEN OPEN DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3360-SET-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FP' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FP'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JH' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JH'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JV' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JV'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE RETURNED AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3370-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-REASON-CD
                   ADD BPYM-RETURNED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-RETURNED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE LAST STMT DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3380-CHECK-LAST-STMT.
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
           IF WS-DAYS > 365
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND AMT BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3390-ACCUM-SOURCE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-SOURCE-CD
                   ADD BPYM-REFUND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-SOURCE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-REFUND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3400-DESCRIBE-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'PD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RT'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'CN'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'RF'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN CLOSE DATE IS PAST
      * FP -> JH WHEN CLOSE DATE IS PAST
      * JH -> JV WHEN CLOSE DATE IS PAST
      * JV -> RM WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3410-SET-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FP' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FP'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JH' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JH'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JV' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JV'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE RETURNED AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3420-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO BPYM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPYT-RETURNED-AMT NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO BPYM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> BX WHEN EFF DATE IS PAST
      * BX -> GM WHEN EFF DATE IS PAST
      * GM -> KU WHEN EFF DATE IS PAST
      * KU -> LT WHEN EFF DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3430-SET-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BX' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BX'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KU' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KU'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT.
      *----------------------------------------------------------------*
       3440-PRORATE-ITEM.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 120 CALENDAR DAYS SETS
      * OVERRIDE FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3450-CHECK-EFF.
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
           IF WS-DAYS > 120
               MOVE 'Y' TO BPYM-OVERRIDE-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3460-FLAG-PAYMENT.
           IF BPYT-PAYMENT-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYT-PAYMENT-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3470-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF BPYM-LAST-ACT-DATE > 0 AND BPYM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * WORK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3480-FLAG-ITEM.
           IF WS-WORK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E090 ADDR REQUIRED
      * E280 WORK AMT NEGATIVE
      * E180 WORK AMT OVER 50000
      * E675 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3490-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-ADDR = SPACES
                   MOVE 'E090' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E280' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 50000.00
                   MOVE 'E180' TO WS-REJECT-CODE
               WHEN BPYM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E675' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED YTD FEE ON THE FEE AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3500-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BPYM-RETURNED-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = BPYT-FEE-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYT-CHANNEL-CD = 'BRN'
                   MOVE 12.00 TO BPYM-RETURNED-YTD
               WHEN BPYT-CHANNEL-CD = 'BAT'
                   MOVE 20.00 TO BPYM-RETURNED-YTD
               WHEN OTHER
                   COMPUTE BPYM-RETURNED-YTD ROUNDED = BPYT-FEE-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYM-RETURNED-YTD > 0 AND BPYM-RETURNED-YTD < 10.00
               MOVE 10.00 TO BPYM-RETURNED-YTD
           END-IF
           IF BPYM-RETURNED-YTD > 100.00
               MOVE 100.00 TO BPYM-RETURNED-YTD
           END-IF
           ADD BPYM-RETURNED-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FEE AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3510-FLAG-FEE.
           IF BPYM-FEE-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3520-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EXPIRY-DATE TO DTU-DATE-1
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
      * DAILY INTEREST ON WORK AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3530-ACCRUE-RATE.
           IF WS-WORK-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   WS-WORK-AMT * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO BPYM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3540-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPYM-PAYMENT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = BPYM-PAYMENT-AMT - (WS-WORK-AMT * 3)
           COMPUTE BPYT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3550-LIMIT-FEE.
           IF BPYT-FEE-AMT < 50.00
               MOVE 50.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-FEE-AMT > 5000.00
               MOVE 5000.00 TO BPYT-FEE-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE STATUS CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3560-DESCRIBE-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'LR'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NZ'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'RV'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'TM'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'TZ'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED FEE ON THE RETURNED YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3570-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO BPYM-RETURNED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE BPYM-RETURNED-AMT ROUNDED = BPYM-RETURNED-YTD
                       * BPYM-PENALTY-PCT / 200
               WHEN BPYM-CYCLE-CD = 'DA'
                   MOVE 5.00 TO BPYM-RETURNED-AMT
               WHEN BPYM-CYCLE-CD = 'FP'
                   MOVE 15.00 TO BPYM-RETURNED-AMT
               WHEN OTHER
                   COMPUTE BPYM-RETURNED-AMT ROUNDED = BPYM-RETURNED-YTD
                       * BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYM-RETURNED-AMT > 0 AND BPYM-RETURNED-AMT < 2.00
               MOVE 2.00 TO BPYM-RETURNED-AMT
           END-IF
           IF BPYM-RETURNED-AMT > 100.00
               MOVE 100.00 TO BPYM-RETURNED-AMT
           END-IF
           ADD BPYM-RETURNED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3580-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPYM-FEE-YTD NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN OPEN DATE IS PAST
      * BAT -> MAL WHEN OPEN DATE IS PAST
      * MAL -> TEL WHEN OPEN DATE IS PAST
      * TEL -> ONL WHEN OPEN DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3590-SET-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BPYM-OPEN-DATE > 0 AND BPYM-OPEN-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3600-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF BPYT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYT-POST-DATE TO DTU-DATE-1
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
      * DAILY INTEREST ON PAYMENT AMT AT RATE,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3610-ACCRUE-PAYMENT.
           IF BPYT-PAYMENT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-PAYMENT-AMT * BPYM-RATE / 100 / 365
               ADD WS-WORK-AMT TO BPYT-RETURNED-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE FEE YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO PAYMENT AMT.
      *----------------------------------------------------------------*
       3620-PRORATE-PAYMENT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-PAYMENT-AMT ROUNDED =
                   BPYM-FEE-YTD * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYM-PAYMENT-AMT
           END-IF
           ADD BPYM-PAYMENT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * AX -> BX WHEN CLOSE DATE IS PAST
      * BX -> GM WHEN CLOSE DATE IS PAST
      * GM -> KU WHEN CLOSE DATE IS PAST
      * KU -> LT WHEN CLOSE DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3630-SET-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BX' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BX'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GM' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'GM'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KU' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KU'
                   IF BPYM-CLOSE-DATE > 0 AND BPYM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LT' TO BPYM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETURNED YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3640-LIMIT-RETURNED.
           IF BPYM-RETURNED-YTD < 25.00
               MOVE 25.00 TO BPYM-RETURNED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-RETURNED-YTD > 5000.00
               MOVE 5000.00 TO BPYM-RETURNED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3650-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-ITEM.
           IF BPYM-ITEM-CNT < 9999999
               ADD 1 TO BPYM-ITEM-CNT
           END-IF
           IF BPYM-ITEM-CNT NOT < 5
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED FEE ON THE REFUND AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3670-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BPYM-RETURNED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE BPYM-RETURNED-AMT ROUNDED = BPYT-REFUND-AMT *
                       BPYM-RATE / 200
               WHEN BPYM-REASON-CD = 'AX'
                   MOVE 12.00 TO BPYM-RETURNED-AMT
               WHEN BPYM-REASON-CD = 'BX'
                   MOVE 20.00 TO BPYM-RETURNED-AMT
               WHEN OTHER
                   COMPUTE BPYM-RETURNED-AMT ROUNDED = BPYT-REFUND-AMT *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-RETURNED-AMT > 0 AND BPYM-RETURNED-AMT < 2.00
               MOVE 2.00 TO BPYM-RETURNED-AMT
           END-IF
           IF BPYM-RETURNED-AMT > 35.00
               MOVE 35.00 TO BPYM-RETURNED-AMT
           END-IF
           ADD BPYM-RETURNED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E974 REF NO REQUIRED
      * E894 FEE AMT NEGATIVE
      * E388 FEE AMT OVER 1000000
      * E327 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3680-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-REF-NO = SPACES
                   MOVE 'E974' TO WS-REJECT-CODE
               WHEN BPYT-FEE-AMT < 0
                   MOVE 'E894' TO WS-REJECT-CODE
               WHEN BPYT-FEE-AMT > 1000000.00
                   MOVE 'E388' TO WS-REJECT-CODE
               WHEN BPYM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E327' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3690-COUNT-ITEM.
           IF BPYM-ITEM-CNT < 9999999
               ADD 1 TO BPYM-ITEM-CNT
           END-IF
           IF BPYM-ITEM-CNT NOT < 5
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED FEE ON THE REFUND YTD.
      * WAIVED WHEN AUTO RENEW FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3700-ASSESS-AUTO-RENEW.
           EVALUATE TRUE
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   MOVE ZERO TO BPYT-RETURNED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = BPYM-REFUND-YTD *
                       BPYM-RATE / 200
               WHEN BPYT-TRAN-CD = 'SC'
                   MOVE 7.00 TO BPYT-RETURNED-AMT
               WHEN BPYT-TRAN-CD = 'PD'
                   MOVE 3.00 TO BPYT-RETURNED-AMT
               WHEN OTHER
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = BPYM-REFUND-YTD *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYT-RETURNED-AMT > 0 AND BPYT-RETURNED-AMT < 10.00
               MOVE 10.00 TO BPYT-RETURNED-AMT
           END-IF
           IF BPYT-RETURNED-AMT > 35.00
               MOVE 35.00 TO BPYT-RETURNED-AMT
           END-IF
           ADD BPYT-RETURNED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3710-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPYM-FEE-YTD NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE REFUND AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $250.00.
      *----------------------------------------------------------------*
       3720-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = BPYT-REFUND-AMT *
                       BPYM-RATE / 200
               WHEN BPYM-CYCLE-CD = 'DA'
                   MOVE 5.00 TO WS-WORK-AMT
               WHEN BPYM-CYCLE-CD = 'FP'
                   MOVE 12.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = BPYT-REFUND-AMT *
                       BPYM-RATE / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 250.00
               MOVE 250.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3730-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYM-FEE-YTD / 4
           COMPUTE WS-WORK-AMT-2 = BPYM-FEE-YTD - (WS-WORK-AMT * 4)
           COMPUTE BPYT-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3740-FLAG-RETURNED.
           IF BPYM-RETURNED-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-RETURNED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY PAYEE TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3750-ACCUM-PAYEE-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-PAYEE-TYPE-CD
                   ADD BPYM-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-PAYEE-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 30 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3760-CHECK-OPEN.
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
           IF WS-DAYS > 30
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       3770-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-RETURNED-AMT ROUNDED =
                   BPYT-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYT-RETURNED-AMT
           END-IF
           ADD BPYT-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3780-DESCRIBE-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'FP'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'JH'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'JV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RM'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'YR'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * DA -> FP WHEN EXPIRY DATE IS PAST
      * FP -> JH WHEN EXPIRY DATE IS PAST
      * JH -> JV WHEN EXPIRY DATE IS PAST
      * JV -> RM WHEN EXPIRY DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3790-SET-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FP' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FP'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JH' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JH'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'JV' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'JV'
                   IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RM' TO BPYM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETURNED YTD IS FLOORED AT $25.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3800-LIMIT-RETURNED.
           IF BPYM-RETURNED-YTD < 25.00
               MOVE 25.00 TO BPYM-RETURNED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-RETURNED-YTD > 5000.00
               MOVE 5000.00 TO BPYM-RETURNED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3810-DESCRIBE-CYCLE.
           EVALUATE BPYM-CYCLE-CD
               WHEN 'DA'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'FP'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'JH'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'JV'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'RM'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'YR'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'ZW'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3820-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO BPYM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3830-DESCRIBE-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'PD'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RT'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'CN'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'RF'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR TRAN CD:
      * SC -> PD WHEN EFF DATE IS PAST
      * PD -> RT WHEN EFF DATE IS PAST
      * RT -> CN WHEN EFF DATE IS PAST
      * CN -> RF WHEN EFF DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3840-SET-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'PD' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'PD'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RT' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RT'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'CN' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'CN'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RF' TO BPYT-TRAN-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E744 ADDR REQUIRED
      * E394 RETURNED YTD NEGATIVE
      * E298 RETURNED YTD OVER 10000
      * E956 EFFECTIVE DATE IN FUTURE
      *----------------------------------------------------------------*
       3850-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-ADDR = SPACES
                   MOVE 'E744' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-YTD < 0
                   MOVE 'E394' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-YTD > 10000.00
                   MOVE 'E298' TO WS-REJECT-CODE
               WHEN BPYM-EFFECTIVE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E956' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETURNED AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3860-ACCRUE-RETURNED.
           IF BPYM-RETURNED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-RETURNED-AMT * BPYM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BPYM-FEE-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3870-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPYM-FEE-AMT NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3880-ACCRUE-FEE.
           IF BPYM-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-FEE-AMT * BPYM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BPYM-REFUND-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE WORK FEE ON THE PAYMENT YTD.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       3890-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN BPYT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO WS-WORK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE WS-WORK-AMT ROUNDED = BPYM-PAYMENT-YTD *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYM-CYCLE-CD = 'DA'
                   MOVE 20.00 TO WS-WORK-AMT
               WHEN BPYM-CYCLE-CD = 'FP'
                   MOVE 15.00 TO WS-WORK-AMT
               WHEN OTHER
                   COMPUTE WS-WORK-AMT ROUNDED = BPYM-PAYMENT-YTD *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF WS-WORK-AMT > 0 AND WS-WORK-AMT < 2.00
               MOVE 2.00 TO WS-WORK-AMT
           END-IF
           IF WS-WORK-AMT > 25.00
               MOVE 25.00 TO WS-WORK-AMT
           END-IF
           ADD WS-WORK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E537 REF NO REQUIRED
      * E167 FEE AMT NEGATIVE
      * E192 FEE AMT OVER 50000
      * E237 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       3900-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-REF-NO = SPACES
                   MOVE 'E537' TO WS-REJECT-CODE
               WHEN BPYM-FEE-AMT < 0
                   MOVE 'E167' TO WS-REJECT-CODE
               WHEN BPYM-FEE-AMT > 50000.00
                   MOVE 'E192' TO WS-REJECT-CODE
               WHEN BPYM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E237' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REFUND YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3910-LIMIT-REFUND.
           IF BPYM-REFUND-YTD < 50.00
               MOVE 50.00 TO BPYM-REFUND-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-REFUND-YTD > 2500.00
               MOVE 2500.00 TO BPYM-REFUND-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED FEE ON THE PAYMENT AMT.
      * WAIVED WHEN OVERRIDE FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3920-ASSESS-OVERRIDE.
           EVALUATE TRUE
               WHEN BPYM-OVERRIDE-FLAG = 'Y'
                   MOVE ZERO TO BPYT-RETURNED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = BPYM-PAYMENT-AMT
                       * BPYM-PENALTY-PCT / 200
               WHEN BPYM-PAYEE-TYPE-CD = 'BT'
                   MOVE 7.00 TO BPYT-RETURNED-AMT
               WHEN BPYM-PAYEE-TYPE-CD = 'FR'
                   MOVE 20.00 TO BPYT-RETURNED-AMT
               WHEN OTHER
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = BPYM-PAYMENT-AMT
                       * BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYT-RETURNED-AMT > 0 AND BPYT-RETURNED-AMT < 2.00
               MOVE 2.00 TO BPYT-RETURNED-AMT
           END-IF
           IF BPYT-RETURNED-AMT > 75.00
               MOVE 75.00 TO BPYT-RETURNED-AMT
           END-IF
           ADD BPYT-RETURNED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 3
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3930-COUNT-ITEM.
           IF BPYM-ITEM-CNT < 9999999
               ADD 1 TO BPYM-ITEM-CNT
           END-IF
           IF BPYM-ITEM-CNT NOT < 3
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE FEE FEE ON THE RETURNED AMT.
      * WAIVED WHEN HOLD FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3940-ASSESS-HOLD.
           EVALUATE TRUE
               WHEN BPYM-HOLD-FLAG = 'Y'
                   MOVE ZERO TO BPYM-FEE-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   COMPUTE BPYM-FEE-AMT ROUNDED = BPYM-RETURNED-AMT *
                       BPYM-PENALTY-PCT / 200
               WHEN BPYT-CHANNEL-CD = 'BRN'
                   MOVE 5.00 TO BPYM-FEE-AMT
               WHEN BPYT-CHANNEL-CD = 'BAT'
                   MOVE 10.00 TO BPYM-FEE-AMT
               WHEN OTHER
                   COMPUTE BPYM-FEE-AMT ROUNDED = BPYM-RETURNED-AMT *
                       BPYM-PENALTY-PCT / 100
           END-EVALUATE
           IF BPYM-FEE-AMT > 0 AND BPYM-FEE-AMT < 5.00
               MOVE 5.00 TO BPYM-FEE-AMT
           END-IF
           IF BPYM-FEE-AMT > 50.00
               MOVE 50.00 TO BPYM-FEE-AMT
           END-IF
           ADD BPYM-FEE-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE REFUND YTD FEE ON THE PAYMENT AMT.
      * WAIVED WHEN SUSPEND FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3950-ASSESS-SUSPEND.
           EVALUATE TRUE
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   MOVE ZERO TO BPYM-REFUND-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-PREFERRED-FLAG = 'Y'
                   COMPUTE BPYM-REFUND-YTD ROUNDED = BPYT-PAYMENT-AMT *
                       BPYM-RATE / 200
               WHEN BPYM-REMIT-METHOD-CD = 'CGV'
                   MOVE 7.00 TO BPYM-REFUND-YTD
               WHEN BPYM-REMIT-METHOD-CD = 'FNS'
                   MOVE 15.00 TO BPYM-REFUND-YTD
               WHEN OTHER
                   COMPUTE BPYM-REFUND-YTD ROUNDED = BPYT-PAYMENT-AMT *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYM-REFUND-YTD > 0 AND BPYM-REFUND-YTD < 2.00
               MOVE 2.00 TO BPYM-REFUND-YTD
           END-IF
           IF BPYM-REFUND-YTD > 50.00
               MOVE 50.00 TO BPYM-REFUND-YTD
           END-IF
           ADD BPYM-REFUND-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       3960-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYT-RETURNED-AMT ROUNDED =
                   BPYM-PAYMENT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO BPYT-RETURNED-AMT
           END-IF
           ADD BPYT-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E441 OPERATOR REQUIRED
      * E836 RETURNED YTD NEGATIVE
      * E879 RETURNED YTD OVER 5000
      * E104 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3970-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-OPERATOR = SPACES
                   MOVE 'E441' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-YTD < 0
                   MOVE 'E836' TO WS-REJECT-CODE
               WHEN BPYM-RETURNED-YTD > 5000.00
                   MOVE 'E879' TO WS-REJECT-CODE
               WHEN BPYM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E104' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR STATUS CD:
      * JX -> LR WHEN EFF DATE IS PAST
      * LR -> NZ WHEN EFF DATE IS PAST
      * NZ -> RV WHEN EFF DATE IS PAST
      * RV -> TM WHEN EFF DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3980-SET-STATUS.
           EVALUATE BPYM-STATUS-CD
               WHEN 'JX'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LR' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LR'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NZ' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NZ'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'RV' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'RV'
                   IF BPYT-EFF-DATE > 0 AND BPYT-EFF-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TM' TO BPYM-STATUS-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       3990-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 6
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4000-AGE-OPEN.
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
      * DAILY INTEREST ON RETURNED AMT AT RATE,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4010-ACCRUE-RETURNED.
           IF BPYT-RETURNED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYT-RETURNED-AMT * BPYM-RATE / 100 / 360
               ADD WS-WORK-AMT TO WS-WORK-AMT-2
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E259 OPERATOR REQUIRED
      * E202 PAYMENT AMT NEGATIVE
      * E713 PAYMENT AMT OVER 5000
      * E922 NOTICE DATE IN FUTURE
      *----------------------------------------------------------------*
       4020-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYT-OPERATOR = SPACES
                   MOVE 'E259' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT < 0
                   MOVE 'E202' TO WS-REJECT-CODE
               WHEN BPYT-PAYMENT-AMT > 5000.00
                   MOVE 'E713' TO WS-REJECT-CODE
               WHEN BPYM-NOTICE-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E922' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4030-DESCRIBE-TRAN.
           EVALUATE BPYT-TRAN-CD
               WHEN 'SC'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'PD'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RT'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'CN'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'RF'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4040-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPYM-FEE-YTD NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * FEE YTD OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4050-FLAG-FEE.
           IF BPYM-FEE-YTD > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-FEE-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED AMT OVER $2500.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       4060-FLAG-RETURNED.
           IF BPYM-RETURNED-AMT > 2500.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE BPYM-RETURNED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE REFUND TIER TIER FOR THE RETURNED YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4070-SELECT-RATE.
           MOVE 1 TO WS-TX
           MOVE WS-REFUND-TIER-RATE(1) TO BPYM-RATE
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF BPYM-RETURNED-YTD NOT < WS-REFUND-TIER-LOW(WS-TX)
                   MOVE WS-REFUND-TIER-RATE(WS-TX) TO BPYM-RATE
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-RATE > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-RATE
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON RETURNED AMT AT MARGIN,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4080-ACCRUE-RETURNED.
           IF BPYM-RETURNED-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-RETURNED-AMT * BPYM-MARGIN / 100 / 365
               ADD WS-WORK-AMT TO WS-WORK-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REFUND YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4090-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-CHANNEL-CD
                   ADD BPYM-REFUND-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-REFUND-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR PAYEE TYPE CD:
      * BT -> FR WHEN EFFECTIVE DATE IS PAST
      * FR -> LL WHEN EFFECTIVE DATE IS PAST
      * LL -> US WHEN EFFECTIVE DATE IS PAST
      * US -> XM WHEN EFFECTIVE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4100-SET-PAYEE-TYPE.
           EVALUATE BPYM-PAYEE-TYPE-CD
               WHEN 'BT'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FR' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FR'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'LL' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'LL'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'US' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'US'
                   IF BPYM-EFFECTIVE-DATE > 0 AND BPYM-EFFECTIVE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XM' TO BPYM-PAYEE-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS PREFERRED FLAG.
      *----------------------------------------------------------------*
       4110-COUNT-EXCEPT.
           IF BPYM-EXCEPT-CNT < 9999999
               ADD 1 TO BPYM-EXCEPT-CNT
           END-IF
           IF BPYM-EXCEPT-CNT NOT < 5
               MOVE 'Y' TO BPYM-PREFERRED-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4120-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-EXPIRY-DATE > 0 AND BPYM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYM-RETURNED-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-RETURNED-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYM-RETURNED-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-RETURNED-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-RETURNED-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4130-CHECK-EFF.
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
      * ACCUMULATE FEE YTD BY SOURCE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4140-ACCUM-SOURCE.
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
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4150-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO BPYM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE PAYMENT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4160-SELECT-MARGIN.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO BPYM-MARGIN
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF BPYT-PAYMENT-AMT NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO BPYM-MARGIN
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-MARGIN > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-MARGIN
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NOTICE DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4170-AGE-NOTICE.
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
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE PAYMENT YTD FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO RETURNED AMT.
      *----------------------------------------------------------------*
       4180-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE BPYM-RETURNED-AMT ROUNDED =
                   BPYM-PAYMENT-YTD * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO BPYM-RETURNED-AMT
           END-IF
           ADD BPYM-RETURNED-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * RETURNED YTD OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       4190-FLAG-RETURNED.
           IF BPYM-RETURNED-YTD > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE BPYM-RETURNED-YTD TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E638 CONTACT REQUIRED
      * E176 REFUND AMT NEGATIVE
      * E561 REFUND AMT OVER 99999
      * E161 EFF DATE IN FUTURE
      *----------------------------------------------------------------*
       4200-EDIT-CONTACT.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-CONTACT = SPACES
                   MOVE 'E638' TO WS-REJECT-CODE
               WHEN BPYT-REFUND-AMT < 0
                   MOVE 'E176' TO WS-REJECT-CODE
               WHEN BPYT-REFUND-AMT > 99999.00
                   MOVE 'E561' TO WS-REJECT-CODE
               WHEN BPYT-EFF-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E161' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * RETURNED YTD IS FLOORED AT $50.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4210-LIMIT-RETURNED.
           IF BPYM-RETURNED-YTD < 50.00
               MOVE 50.00 TO BPYM-RETURNED-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-RETURNED-YTD > 10000.00
               MOVE 10000.00 TO BPYM-RETURNED-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       4220-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 6
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * REFUND AMT IS FLOORED AT $10.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       4230-LIMIT-REFUND.
           IF BPYT-REFUND-AMT < 10.00
               MOVE 10.00 TO BPYT-REFUND-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYT-REFUND-AMT > 5000.00
               MOVE 5000.00 TO BPYT-REFUND-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 365 CALENDAR DAYS SETS
      * SUSPEND FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4240-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $50.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4250-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 50.00
               MOVE 50.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-30, 31-60, 61-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       4260-AGE-OPEN.
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
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYM-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE BAND TIER FOR THE WORK AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4270-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-BAND-RATE(1) TO BPYM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 12
               IF WS-WORK-AMT NOT < WS-FEE-BAND-LOW(WS-TX)
                   MOVE WS-FEE-BAND-RATE(WS-TX) TO BPYM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF BPYM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO BPYM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4280-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = BPYM-FEE-YTD / 12
           COMPUTE WS-WORK-AMT-2 = BPYM-FEE-YTD - (WS-WORK-AMT * 12)
           COMPUTE WS-WORK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT PAYMENT YTD INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4290-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = BPYM-PAYMENT-YTD / 4
           COMPUTE WS-WORK-AMT-2 = BPYM-PAYMENT-YTD - (WS-WORK-AMT * 4)
           COMPUTE BPYT-PAYMENT-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST ACT DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-90 AND OVER 90 DAYS.
      *----------------------------------------------------------------*
       4300-AGE-LAST-ACT.
           MOVE ZERO TO WS-DAYS
           IF BPYM-LAST-ACT-DATE > 0 AND BPYM-LAST-ACT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-LAST-ACT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 90
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-REFUND-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * RETURNED AMT OVER $25000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       4310-FLAG-RETURNED.
           IF BPYT-RETURNED-AMT > 25000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE BPYT-RETURNED-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETURNED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4320-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   BPYM-RETURNED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ASSESS THE RETURNED FEE ON THE WORK AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4330-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN BPYM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO BPYT-RETURNED-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN BPYM-SUSPEND-FLAG = 'Y'
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = WS-WORK-AMT *
                       BPYM-RATE / 200
               WHEN BPYM-SOURCE-CD = 'CT'
                   MOVE 3.00 TO BPYT-RETURNED-AMT
               WHEN BPYM-SOURCE-CD = 'FV'
                   MOVE 20.00 TO BPYT-RETURNED-AMT
               WHEN OTHER
                   COMPUTE BPYT-RETURNED-AMT ROUNDED = WS-WORK-AMT *
                       BPYM-RATE / 100
           END-EVALUATE
           IF BPYT-RETURNED-AMT > 0 AND BPYT-RETURNED-AMT < 10.00
               MOVE 10.00 TO BPYT-RETURNED-AMT
           END-IF
           IF BPYT-RETURNED-AMT > 25.00
               MOVE 25.00 TO BPYT-RETURNED-AMT
           END-IF
           ADD BPYT-RETURNED-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT WORK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4340-SPLIT-PAYMENT.
           COMPUTE WS-WORK-AMT = WS-WORK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = WS-WORK-AMT - (WS-WORK-AMT * 2)
           COMPUTE BPYM-PAYMENT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4350-COUNT-ITEM.
           IF BPYM-ITEM-CNT < 9999999
               ADD 1 TO BPYM-ITEM-CNT
           END-IF
           IF BPYM-ITEM-CNT NOT < 6
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE RETURNED AMT BY TRAN CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4360-ACCUM-TRAN.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYT-TRAN-CD
                   ADD BPYT-RETURNED-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYT-TRAN-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYT-RETURNED-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E521 ADDR REQUIRED
      * E196 WORK AMT NEGATIVE
      * E592 WORK AMT OVER 25000
      * E584 LAST ACT DATE IN FUTURE
      *----------------------------------------------------------------*
       4370-EDIT-ADDR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN BPYM-ADDR = SPACES
                   MOVE 'E521' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT < 0
                   MOVE 'E196' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT > 25000.00
                   MOVE 'E592' TO WS-REJECT-CODE
               WHEN BPYM-LAST-ACT-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E584' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REFUND YTD AT PENALTY PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4380-ACCRUE-REFUND.
           IF BPYM-REFUND-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   BPYM-REFUND-YTD * BPYM-PENALTY-PCT / 100 / 365
               ADD WS-WORK-AMT TO BPYT-PAYMENT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM CLOSE DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       4390-AGE-CLOSE.
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
                   ADD BPYT-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD BPYT-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD BPYT-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD BPYT-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD BPYT-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE PAYMENT YTD BY STATUS CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4400-ACCUM-STATUS.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = BPYM-STATUS-CD
                   ADD BPYM-PAYMENT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE BPYM-STATUS-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE BPYM-PAYMENT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * BRN -> BAT WHEN MATURITY DATE IS PAST
      * BAT -> MAL WHEN MATURITY DATE IS PAST
      * MAL -> TEL WHEN MATURITY DATE IS PAST
      * TEL -> ONL WHEN MATURITY DATE IS PAST
      * WITHIN 60 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4410-SET-CHANNEL.
           EVALUATE BPYT-CHANNEL-CD
               WHEN 'BRN'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'BAT' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'BAT'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'MAL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'MAL'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'TEL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'TEL'
                   IF BPYM-MATURITY-DATE > 0 AND BPYM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'ONL' TO BPYT-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE REASON CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4420-DESCRIBE-REASON.
           EVALUATE BPYM-REASON-CD
               WHEN 'AX'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'BX'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'GM'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'KU'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'LT'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'RR'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'WJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE CLOSE DATE. MORE THAN 45 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4430-CHECK-CLOSE.
           MOVE ZERO TO WS-DAYS
           IF BPYM-CLOSE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-CLOSE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 45
               MOVE 'Y' TO BPYM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 15 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       4440-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF BPYM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE BPYM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO BPYT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REMIT METHOD CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       4450-DESCRIBE-REMIT-METHOD.
           EVALUATE BPYM-REMIT-METHOD-CD
               WHEN 'CGV'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'FNS'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'NMS'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'TAF'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN 'VNK'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'WUA'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'XCT'
                   MOVE 'MATURED' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PAYMENT YTD IS FLOORED AT $50.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       4460-LIMIT-PAYMENT.
           IF BPYM-PAYMENT-YTD < 50.00
               MOVE 50.00 TO BPYM-PAYMENT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF BPYM-PAYMENT-YTD > 2500.00
               MOVE 2500.00 TO BPYM-PAYMENT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS SUSPEND FLAG.
      *----------------------------------------------------------------*
       4470-COUNT-SEQ.
           IF BPYT-SEQ < 9999999
               ADD 1 TO BPYT-SEQ
           END-IF
           IF BPYT-SEQ NOT < 12
               MOVE 'Y' TO BPYM-SUSPEND-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE RETURNED AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO WORK AMT 2.
      *----------------------------------------------------------------*
       4480-PRORATE-RETURNED.
           IF WS-PERIOD-DAYS > 0
               COMPUTE WS-WORK-AMT-2 ROUNDED =
                   BPYT-RETURNED-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO WS-WORK-AMT-2
           END-IF
           ADD WS-WORK-AMT-2 TO WS-TOTAL-PRORATED.

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
                   MOVE BPSTAT-REFUND-AMT
                     TO BPYM-REFUND-AMT
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
                   MOVE BPPRC-FEE-AMT
                     TO BPYM-FEE-AMT
                   MOVE BPPRC-PAYMENT-AMT
                     TO BPYM-PAYMENT-AMT
                   MOVE BPPRC-PAYMENT-YTD
                     TO BPYM-PAYMENT-YTD
                   MOVE BPPRC-REFUND-YTD
                     TO BPYM-REFUND-YTD
                   MOVE BPPRC-SUSPEND-FLAG
                     TO BPYM-SUSPEND-FLAG
                   MOVE BPPRC-OVERRIDE-FLAG
                     TO BPYM-OVERRIDE-FLAG
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
           DISPLAY 'BPYRCON READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'BPYRCON' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
