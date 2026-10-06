      *================================================================*
      * PROGRAM  : LBXBILL                                             *
      * TYPE     : BATCH                                               *
      * PURPOSE  : LOCKBOX REMITTANCE - CYCLE BILLING. READS THE LBX   *
      *            TRANSACTIONS, UPDATES THE LBX MASTER AND PRODUCES   *
      *            THE CONTROL REPORT. WHOLESALE LOCKBOX REMITTANCE    *
      *            CAPTURE AND CREDIT TO CLIENTS                       *
      * INPUT    : LBXMST (KSDS), LBXTRN, BANKCTL                      *
      * OUTPUT   : LBXOUT, LBXBIRPT                                    *
      * CALLS    : GETCTL, DATEUTIL, ABENDRTN, LBWAIV, LBFEE, LBCAL    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-11-14 LMB  ORIGINAL                                       *
      * 2000-08-10 GAF  EXCEPT CUTOFF MOVED PRB6820                    *
      * 2003-05-09 JLM  DEPOSIT RERUN PROTECTION PRB4453               *
      * 2007-05-01 JLM  FLOAT EXCEPTIONS TO OPS QUEUE CHG6936          *
      * 2011-01-18 GAF  FLOAT CALCULATION CORRECTED CHG2902            *
      * 2011-05-16 PQR  FEE CUTOFF MOVED REQ3232                       *
      * 2011-06-05 JLM  REMIT CUTOFF MOVED CHG1707                     *
      * 2012-10-07 GAF  FEE LIMIT REVISED CHG8262                      *
      * 2013-10-19 JLM  CHECK EXCEPTIONS TO OPS QUEUE INC9591          *
      * 2013-03-06 GAF  EXCEPT EXCEPTIONS TO OPS QUEUE REQ0230         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBXBILL.
       AUTHOR. J L MARSH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO "LBXMST"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LBXM-KEY
               FILE STATUS IS WS-MST-STATUS.
           SELECT OPTIONAL TRAN-FILE ASSIGN TO "LBXTRN"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-TRN-STATUS.
           SELECT OUT-FILE ASSIGN TO "LBXOUT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
           SELECT REPORT-FILE ASSIGN TO "LBXBIRPT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE.
       COPY LBXMST.
       FD  TRAN-FILE.
       COPY LBXTRN.
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
                                           VALUE 16.000000.
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
      * WS-FEE-SCHED - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-FEE-SCHED-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000479000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005000000000900000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005050000000117000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00005300000000734500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00007800000000588500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010300000000856500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00012800000000300000'.
       01  WS-FEE-SCHED-TBL REDEFINES WS-FEE-SCHED-VALUES.
           05  WS-FEE-SCHED-ROW OCCURS 7 TIMES.
               10  WS-FEE-SCHED-LOW        PIC 9(09)V99.
               10  WS-FEE-SCHED-RATE       PIC 9(03)V9(06).
      *----------------------------------------------------------------*
      * WS-EXCEPT-BAND - LOWER BOUND 9(9)V99, RATE 9(3)V9(6)
      *----------------------------------------------------------------*
       01  WS-EXCEPT-BAND-VALUES.
           05  FILLER                      PIC X(20)
                                           VALUE '00000000000000238000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00010000000000401000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015000000000659000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00015500000000053000'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016500000000213500'.
           05  FILLER                      PIC X(20)
                                           VALUE '00016600000000073000'.
       01  WS-EXCEPT-BAND-TBL REDEFINES WS-EXCEPT-BAND-VALUES.
           05  WS-EXCEPT-BAND-ROW OCCURS 6 TIMES.
               10  WS-EXCEPT-BAND-LOW      PIC 9(09)V99.
               10  WS-EXCEPT-BAND-RATE     PIC 9(03)V9(06).
       01  WS-RPT-HEAD.
           05  FILLER                      PIC X(40)
               VALUE 'CBS  LBXBILL  LOCKBOX REMITTANCE'.
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
       COPY LBWAIVP.
       COPY LBFEEP.
       COPY LBCALP.

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
               MOVE 'LBXMST' TO ABND-FILE-NAME
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
           MOVE LBXT-KEY TO LBXM-KEY
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'NOMS' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
           END-READ
           IF WS-REJECT-CODE = SPACES
               PERFORM 3000-LIMIT-FLOAT
               PERFORM 3010-LIMIT-ITEM
               PERFORM 3020-COUNT-ITEM
               PERFORM 3030-PRORATE-CASH-ON-HAND
               PERFORM 3040-SPLIT-REMIT
               PERFORM 3050-COUNT-SEQ
               PERFORM 3060-ACCRUE-FEE
               PERFORM 3070-ACCRUE-FLOAT
               PERFORM 3080-DESCRIBE-REGION
               PERFORM 3090-PRORATE-DEPOSIT
               PERFORM 3100-EDIT-MEMO
               PERFORM 3110-AGE-LAST-STMT
               PERFORM 3120-FLAG-FEE
               PERFORM 3130-ACCUM-REASON
               PERFORM 3140-AGE-EXPIRY
               PERFORM 3150-ASSESS-EXEMPT
               PERFORM 3160-ACCUM-REGION
               PERFORM 3170-COUNT-SEQ
               PERFORM 3180-ACCRUE-REMIT
               PERFORM 3190-ACCUM-CHANNEL
               PERFORM 3200-COUNT-ITEM
               PERFORM 3210-ASSESS-EXEMPT
               PERFORM 3220-PRORATE-EXCEPT
               PERFORM 3230-ACCUM-REASON
               PERFORM 3240-EDIT-OPERATOR
               PERFORM 3250-EDIT-CUST-ID
               PERFORM 3260-SET-REASON
               PERFORM 3270-ACCUM-BATCH-TYPE
               PERFORM 3280-SPLIT-DEPOSIT
               PERFORM 3290-ACCUM-BATCH-TYPE
               PERFORM 3300-COUNT-PAYMENT
               PERFORM 3310-ACCUM-REGION
               PERFORM 3320-SPLIT-CHECK
               PERFORM 3330-SET-CYCLE
               PERFORM 3340-EDIT-REF-NO
               PERFORM 3350-SELECT-DISCOUNT
               PERFORM 3360-SELECT-PENALTY-PCT
               PERFORM 3370-LIMIT-ITEM
               PERFORM 3380-LIMIT-CHECK
               PERFORM 3390-ACCRUE-FEE
               PERFORM 3400-DESCRIBE-CHANNEL
               PERFORM 3410-AGE-EFF
               PERFORM 3420-AGE-EFFECTIVE
               PERFORM 3430-SELECT-PENALTY-PCT
               PERFORM 3440-CHECK-NEXT-DUE
               PERFORM 3450-ACCUM-REASON
               PERFORM 3460-AGE-EFFECTIVE
               PERFORM 3470-CHECK-POST
               PERFORM 3480-ASSESS-PREFERRED
               PERFORM 3490-SET-CHANNEL
               PERFORM 3500-EDIT-NAME
               PERFORM 3510-SELECT-DISCOUNT
               PERFORM 3520-FLAG-ITEM
               PERFORM 3530-ASSESS-EXEMPT
               PERFORM 3540-FLAG-CASH-ON-HAND
               PERFORM 3550-COUNT-ITEM
               PERFORM 3560-SELECT-PENALTY-PCT
               PERFORM 3570-CHECK-EFFECTIVE
               PERFORM 3580-ACCUM-REASON
               PERFORM 3590-SELECT-DISCOUNT
               PERFORM 3600-LIMIT-CHECK
               PERFORM 3610-ACCUM-CHANNEL
               PERFORM 3620-ACCRUE-FEE
               PERFORM 3630-DESCRIBE-CYCLE
               PERFORM 3640-ASSESS-REVERSAL
               PERFORM 3650-ASSESS-EXEMPT
               PERFORM 3660-COUNT-SEQ
               PERFORM 3670-ASSESS-STOP
               PERFORM 3680-SPLIT-CASH-ON-HAND
               PERFORM 3690-SPLIT-REMIT
               PERFORM 3700-AGE-LAST-STMT
               PERFORM 3710-DESCRIBE-TRAN
               PERFORM 3720-AGE-NEXT-DUE
               PERFORM 3730-SPLIT-CHECK
               PERFORM 3740-AGE-EFF
               PERFORM 3750-AGE-EFF
               PERFORM 3760-SET-REASON
               PERFORM 3770-CHECK-OPEN
               PERFORM 3780-SET-REGION
               PERFORM 3790-PRORATE-REMIT
               PERFORM 3800-CHECK-EFF
               PERFORM 3810-PRORATE-FEE
               PERFORM 3820-CHECK-POST
               PERFORM 3830-DESCRIBE-CHANNEL
               PERFORM 3840-SELECT-PENALTY-PCT
               PERFORM 3850-CHECK-POST
               PERFORM 3860-FLAG-CHECK
               PERFORM 3870-CHECK-MATURITY
               PERFORM 3880-CHECK-EFF
               PERFORM 3890-SELECT-DISCOUNT
               PERFORM 3900-LIMIT-DEPOSIT
               PERFORM 3910-COUNT-ITEM
               PERFORM 3920-DESCRIBE-REGION
               PERFORM 3930-PRORATE-CHECK
               PERFORM 3940-SELECT-DISCOUNT
               PERFORM 3950-COUNT-EXCEPT
               PERFORM 3960-SET-CHANNEL
               PERFORM 3970-SELECT-PENALTY-PCT
               PERFORM 3980-SPLIT-FEE
               PERFORM 3990-SPLIT-CHECK
               PERFORM 4000-COUNT-PAYMENT
               PERFORM 4010-SPLIT-FEE
               PERFORM 4020-ACCRUE-CHECK
               PERFORM 4030-ASSESS-PREFERRED
               PERFORM 4040-SET-REASON
               PERFORM 4050-SELECT-FEE-PCT
               PERFORM 4060-LIMIT-REMIT
               PERFORM 4070-AGE-OPEN
               PERFORM 4080-ACCUM-LOCKBOX
               PERFORM 4090-AGE-LAST-PAY
               PERFORM 4100-LIMIT-DEPOSIT
               PERFORM 4110-PRORATE-FLOAT
               PERFORM 4120-LIMIT-EXCEPT
               PERFORM 4130-SET-BATCH-TYPE
               PERFORM 7000-CALL-LBWAIV
               PERFORM 7020-CALL-LBFEE
               PERFORM 7010-CALL-LBCAL
               REWRITE LBX-MASTER-REC
               IF WS-MST-STATUS NOT = '00'
                   MOVE 'LBXMST' TO ABND-FILE-NAME
                   MOVE WS-MST-STATUS TO ABND-FILE-STATUS
                   PERFORM 9900-ABEND
               END-IF
               WRITE OUT-REC FROM LBX-TRAN-REC
               ADD 1 TO WS-WRITE-COUNT
           END-IF
           PERFORM 8000-DETAIL-LINE.

      *----------------------------------------------------------------*
      * FLOAT YTD IS FLOORED AT $100.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3000-LIMIT-FLOAT.
           IF LBXM-FLOAT-YTD < 100.00
               MOVE 100.00 TO LBXM-FLOAT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-FLOAT-YTD > 2500.00
               MOVE 2500.00 TO LBXM-FLOAT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT IS FLOORED AT $50.00 AND CAPPED AT $5000.00.
      *----------------------------------------------------------------*
       3010-LIMIT-ITEM.
           IF WS-WORK-AMT < 50.00
               MOVE 50.00 TO WS-WORK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT > 5000.00
               MOVE 5000.00 TO WS-WORK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS STOP FLAG.
      *----------------------------------------------------------------*
       3020-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 6
               MOVE 'Y' TO LBXM-STOP-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CHECK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CASH ON HAND YTD.
      *----------------------------------------------------------------*
       3030-PRORATE-CASH-ON-HAND.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CASH-ON-HAND-YTD ROUNDED =
                   LBXM-CHECK-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-CASH-ON-HAND-YTD
           END-IF
           ADD LBXM-CASH-ON-HAND-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SPLIT REMIT AMT INTO 4 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3040-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = LBXM-REMIT-AMT / 4
           COMPUTE WS-WORK-AMT-2 = LBXM-REMIT-AMT - (WS-WORK-AMT * 4)
           COMPUTE LBXM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 5
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3050-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 5
               MOVE 'Y' TO LBXM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE YTD AT DISCOUNT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3060-ACCRUE-FEE.
           IF LBXM-FEE-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-FEE-YTD * LBXM-DISCOUNT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-EXCEPT-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FLOAT AMT AT FEE PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3070-ACCRUE-FLOAT.
           IF LBXT-FLOAT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-FLOAT-AMT * LBXM-FEE-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-FEE-YTD
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3080-DESCRIBE-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'FMZ'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN 'KSL'
                   MOVE 'MATURED' TO WS-DESC
               WHEN 'NYK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE CHECK AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO DEPOSIT AMT.
      *----------------------------------------------------------------*
       3090-PRORATE-DEPOSIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-DEPOSIT-AMT ROUNDED =
                   LBXT-CHECK-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-DEPOSIT-AMT
           END-IF
           ADD LBXM-DEPOSIT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E605 MEMO REQUIRED
      * E891 CHECK AMT NEGATIVE
      * E662 CHECK AMT OVER 1000000
      * E997 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3100-EDIT-MEMO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-MEMO = SPACES
                   MOVE 'E605' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-AMT < 0
                   MOVE 'E891' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-AMT > 1000000.00
                   MOVE 'E662' TO WS-REJECT-CODE
               WHEN LBXM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E997' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-90, 91-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3110-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 90
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FEE AMT OVER $100000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY H.
      *----------------------------------------------------------------*
       3120-FLAG-FEE.
           IF LBXM-FEE-AMT > 100000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'H' TO WS-EXC-SEVERITY
               MOVE LBXM-FEE-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT YTD BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3130-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REASON-CD
                   ADD LBXM-REMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-REMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EXPIRY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3140-AGE-EXPIRY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-EXPIRY-DATE > 0 AND LBXM-EXPIRY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-EXPIRY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE CHECK YTD FEE ON THE EXCEPT YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $50.00.
      *----------------------------------------------------------------*
       3150-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO LBXM-CHECK-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-STOP-FLAG = 'Y'
                   COMPUTE LBXM-CHECK-YTD ROUNDED = LBXM-EXCEPT-YTD *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXM-CHANNEL-CD = 'B'
                   MOVE 7.00 TO LBXM-CHECK-YTD
               WHEN LBXM-CHANNEL-CD = 'H'
                   MOVE 12.00 TO LBXM-CHECK-YTD
               WHEN OTHER
                   COMPUTE LBXM-CHECK-YTD ROUNDED = LBXM-EXCEPT-YTD *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-CHECK-YTD > 0 AND LBXM-CHECK-YTD < 10.00
               MOVE 10.00 TO LBXM-CHECK-YTD
           END-IF
           IF LBXM-CHECK-YTD > 50.00
               MOVE 50.00 TO LBXM-CHECK-YTD
           END-IF
           ADD LBXM-CHECK-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND AMT BY REGION CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3160-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REGION-CD
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS NOTICE SENT FLAG.
      *----------------------------------------------------------------*
       3170-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 6
               MOVE 'Y' TO LBXM-NOTICE-SENT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON REMIT AMT AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3180-ACCRUE-REMIT.
           IF LBXM-REMIT-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-REMIT-AMT * LBXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT AMT BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3190-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXT-CHANNEL-CD
                   ADD LBXM-EXCEPT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXT-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-EXCEPT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3200-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 12
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT YTD FEE ON THE DEPOSIT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3210-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO LBXM-EXCEPT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-STOP-FLAG = 'Y'
                   COMPUTE LBXM-EXCEPT-YTD ROUNDED = LBXM-DEPOSIT-AMT *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXT-CHANNEL-CD = 'BRN'
                   MOVE 7.00 TO LBXM-EXCEPT-YTD
               WHEN LBXT-CHANNEL-CD = 'BAT'
                   MOVE 3.00 TO LBXM-EXCEPT-YTD
               WHEN OTHER
                   COMPUTE LBXM-EXCEPT-YTD ROUNDED = LBXM-DEPOSIT-AMT *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-EXCEPT-YTD > 0 AND LBXM-EXCEPT-YTD < 1.00
               MOVE 1.00 TO LBXM-EXCEPT-YTD
           END-IF
           IF LBXM-EXCEPT-YTD > 35.00
               MOVE 35.00 TO LBXM-EXCEPT-YTD
           END-IF
           ADD LBXM-EXCEPT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * PRORATE WORK AMT 2 FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO EXCEPT AMT.
      *----------------------------------------------------------------*
       3220-PRORATE-EXCEPT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-EXCEPT-AMT ROUNDED =
                   WS-WORK-AMT-2 * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-EXCEPT-AMT
           END-IF
           ADD LBXM-EXCEPT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * ACCUMULATE FEE AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3230-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REASON-CD
                   ADD LBXT-FEE-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXT-FEE-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E232 OPERATOR REQUIRED
      * E773 WORK AMT 2 NEGATIVE
      * E048 WORK AMT 2 OVER 50000
      * E427 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3240-EDIT-OPERATOR.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXT-OPERATOR = SPACES
                   MOVE 'E232' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 < 0
                   MOVE 'E773' TO WS-REJECT-CODE
               WHEN WS-WORK-AMT-2 > 50000.00
                   MOVE 'E048' TO WS-REJECT-CODE
               WHEN LBXM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E427' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E101 CUST ID REQUIRED
      * E841 CHECK YTD NEGATIVE
      * E909 CHECK YTD OVER 10000
      * E728 OPEN DATE IN FUTURE
      *----------------------------------------------------------------*
       3250-EDIT-CUST-ID.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-CUST-ID = SPACES
                   MOVE 'E101' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-YTD < 0
                   MOVE 'E841' TO WS-REJECT-CODE
               WHEN LBXM-CHECK-YTD > 10000.00
                   MOVE 'E909' TO WS-REJECT-CODE
               WHEN LBXM-OPEN-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E728' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GF -> UK WHEN NEXT DUE DATE IS PAST
      * UK -> VR WHEN NEXT DUE DATE IS PAST
      * VR -> XG WHEN NEXT DUE DATE IS PAST
      * XG -> GF WHEN NEXT DUE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3260-SET-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XG' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XG'
                   IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GF' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE CASH ON HAND AMT BY BATCH TYPE CD FOR THE CONTROL
      * REPORT.
      *----------------------------------------------------------------*
       3270-ACCUM-BATCH-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-BATCH-TYPE-CD
                   ADD LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-BATCH-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-CASH-ON-HAND-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT DEPOSIT AMT INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3280-SPLIT-DEPOSIT.
           COMPUTE WS-WORK-AMT = LBXM-DEPOSIT-AMT / 3
           COMPUTE WS-WORK-AMT-2 = LBXM-DEPOSIT-AMT - (WS-WORK-AMT * 3)
           COMPUTE LBXT-CHECK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE WORK AMT BY BATCH TYPE CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3290-ACCUM-BATCH-TYPE.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-BATCH-TYPE-CD
                   ADD WS-WORK-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-BATCH-TYPE-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE WS-WORK-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 6
      * SETS HOLD FLAG.
      *----------------------------------------------------------------*
       3300-COUNT-PAYMENT.
           IF LBXM-PAYMENT-CNT < 9999999
               ADD 1 TO LBXM-PAYMENT-CNT
           END-IF
           IF LBXM-PAYMENT-CNT NOT < 6
               MOVE 'Y' TO LBXM-HOLD-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT AMT BY REGION CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3310-ACCUM-REGION.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REGION-CD
                   ADD LBXM-EXCEPT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REGION-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-EXCEPT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHECK YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3320-SPLIT-CHECK.
           COMPUTE WS-WORK-AMT = LBXM-CHECK-YTD / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-CHECK-YTD - (WS-WORK-AMT * 2)
           COMPUTE LBXM-EXCEPT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CYCLE CD:
      * EDV -> EPM WHEN LAST PAY DATE IS PAST
      * EPM -> FZY WHEN LAST PAY DATE IS PAST
      * FZY -> XUU WHEN LAST PAY DATE IS PAST
      * XUU -> YBH WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3330-SET-CYCLE.
           EVALUATE LBXM-CYCLE-CD
               WHEN 'EDV'
                   IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'EPM' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'EPM'
                   IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FZY' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FZY'
                   IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XUU' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XUU'
                   IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'YBH' TO LBXM-CYCLE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E964 REF NO REQUIRED
      * E576 REMIT AMT NEGATIVE
      * E645 REMIT AMT OVER 99999
      * E601 MATURITY DATE IN FUTURE
      *----------------------------------------------------------------*
       3340-EDIT-REF-NO.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-REF-NO = SPACES
                   MOVE 'E964' TO WS-REJECT-CODE
               WHEN LBXM-REMIT-AMT < 0
                   MOVE 'E576' TO WS-REJECT-CODE
               WHEN LBXM-REMIT-AMT > 99999.00
                   MOVE 'E645' TO WS-REJECT-CODE
               WHEN LBXM-MATURITY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E601' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT BAND TIER FOR THE FEE YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3350-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-BAND-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXM-FEE-YTD NOT < WS-EXCEPT-BAND-LOW(WS-TX)
                   MOVE WS-EXCEPT-BAND-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE WORK AMT 2.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3360-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF WS-WORK-AMT-2 NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT 2 IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       3370-LIMIT-ITEM.
           IF WS-WORK-AMT-2 < 0.00
               MOVE 0.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF WS-WORK-AMT-2 > 500.00
               MOVE 500.00 TO WS-WORK-AMT-2
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHECK AMT IS FLOORED AT $100.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       3380-LIMIT-CHECK.
           IF LBXT-CHECK-AMT < 100.00
               MOVE 100.00 TO LBXT-CHECK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-CHECK-AMT > 10000.00
               MOVE 10000.00 TO LBXT-CHECK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT PENALTY PCT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3390-ACCRUE-FEE.
           IF LBXT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-FEE-AMT * LBXM-PENALTY-PCT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-REMIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3400-DESCRIBE-CHANNEL.
           EVALUATE LBXM-CHANNEL-CD
               WHEN 'B'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'H'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'Z'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-30, 31-90, 91-120 AND OVER 120 DAYS.
      *----------------------------------------------------------------*
       3410-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-DEPOSIT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-DEPOSIT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD LBXM-DEPOSIT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-DEPOSIT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-DEPOSIT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-15, 16-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3420-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-CASH-ON-HAND-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE REMIT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3430-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBXM-REMIT-YTD NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE NEXT DUE DATE. MORE THAN 60 CALENDAR DAYS SETS
      * REVERSAL FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3440-CHECK-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NEXT-DUE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE DEPOSIT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3450-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REASON-CD
                   ADD LBXM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-DEPOSIT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFFECTIVE DATE INTO BUCKETS
      * CURRENT, 1-30, 31-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3460-AGE-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-EFFECTIVE-DATE > 0 AND LBXM-EFFECTIVE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-FLOAT-YTD TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 30
                   ADD LBXM-FLOAT-YTD TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-FLOAT-YTD TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXM-FLOAT-YTD TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-FLOAT-YTD TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 30 CALENDAR DAYS SETS
      * PREFERRED FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3470-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF LBXT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO LBXM-PREFERRED-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CHECK FEE ON THE DEPOSIT YTD.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $1.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3480-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO LBXT-CHECK-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-AUTO-RENEW-FLAG = 'Y'
                   COMPUTE LBXT-CHECK-AMT ROUNDED = LBXM-DEPOSIT-YTD *
                       LBXM-DISCOUNT / 200
               WHEN LBXM-CYCLE-CD = 'EDV'
                   MOVE 3.00 TO LBXT-CHECK-AMT
               WHEN LBXM-CYCLE-CD = 'EPM'
                   MOVE 3.00 TO LBXT-CHECK-AMT
               WHEN OTHER
                   COMPUTE LBXT-CHECK-AMT ROUNDED = LBXM-DEPOSIT-YTD *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXT-CHECK-AMT > 0 AND LBXT-CHECK-AMT < 1.00
               MOVE 1.00 TO LBXT-CHECK-AMT
           END-IF
           IF LBXT-CHECK-AMT > 100.00
               MOVE 100.00 TO LBXT-CHECK-AMT
           END-IF
           ADD LBXT-CHECK-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> H WHEN CLOSE DATE IS PAST
      * H -> Z WHEN CLOSE DATE IS PAST
      * Z -> B WHEN CLOSE DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3490-SET-CHANNEL.
           EVALUATE LBXM-CHANNEL-CD
               WHEN 'B'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * FIELD EDITS - FIRST FAILURE SETS THE REJECT CODE:
      * E557 NAME REQUIRED
      * E163 FLOAT YTD NEGATIVE
      * E874 FLOAT YTD OVER 25000
      * E680 EXPIRY DATE IN FUTURE
      *----------------------------------------------------------------*
       3500-EDIT-NAME.
           MOVE SPACES TO WS-REJECT-CODE
           EVALUATE TRUE
               WHEN LBXM-NAME = SPACES
                   MOVE 'E557' TO WS-REJECT-CODE
               WHEN LBXM-FLOAT-YTD < 0
                   MOVE 'E163' TO WS-REJECT-CODE
               WHEN LBXM-FLOAT-YTD > 25000.00
                   MOVE 'E874' TO WS-REJECT-CODE
               WHEN LBXM-EXPIRY-DATE > CTL-NEXT-BUS-DATE
                   MOVE 'E680' TO WS-REJECT-CODE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           IF WS-REJECT-CODE NOT = SPACES
               ADD 1 TO WS-REJECT-COUNT
               MOVE 'Y' TO WS-REJECT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE REMIT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3510-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBXM-REMIT-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * WORK AMT OVER $9999.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY L.
      *----------------------------------------------------------------*
       3520-FLAG-ITEM.
           IF WS-WORK-AMT > 9999.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'L' TO WS-EXC-SEVERITY
               MOVE WS-WORK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT FEE ON THE CHECK YTD.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3530-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO LBXM-DEPOSIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-STOP-FLAG = 'Y'
                   COMPUTE LBXM-DEPOSIT-AMT ROUNDED = LBXM-CHECK-YTD *
                       LBXM-DISCOUNT / 200
               WHEN LBXM-BATCH-TYPE-CD = 'C'
                   MOVE 15.00 TO LBXM-DEPOSIT-AMT
               WHEN LBXM-BATCH-TYPE-CD = 'D'
                   MOVE 10.00 TO LBXM-DEPOSIT-AMT
               WHEN OTHER
                   COMPUTE LBXM-DEPOSIT-AMT ROUNDED = LBXM-CHECK-YTD *
                       LBXM-DISCOUNT / 100
           END-EVALUATE
           IF LBXM-DEPOSIT-AMT > 0 AND LBXM-DEPOSIT-AMT < 10.00
               MOVE 10.00 TO LBXM-DEPOSIT-AMT
           END-IF
           IF LBXM-DEPOSIT-AMT > 75.00
               MOVE 75.00 TO LBXM-DEPOSIT-AMT
           END-IF
           ADD LBXM-DEPOSIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * CASH ON HAND AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3540-FLAG-CASH-ON-HAND.
           IF LBXM-CASH-ON-HAND-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBXM-CASH-ON-HAND-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3550-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 12
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE EXCEPT YTD.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3560-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBXM-EXCEPT-YTD NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFFECTIVE DATE. MORE THAN 180 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3570-CHECK-EFFECTIVE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-EFFECTIVE-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-EFFECTIVE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 180
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE EXCEPT AMT BY REASON CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3580-ACCUM-REASON.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-REASON-CD
                   ADD LBXM-EXCEPT-AMT TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-REASON-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-EXCEPT-AMT TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT BAND TIER FOR THE FLOAT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3590-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-BAND-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXT-FLOAT-AMT NOT < WS-EXCEPT-BAND-LOW(WS-TX)
                   MOVE WS-EXCEPT-BAND-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHECK AMT IS FLOORED AT $25.00 AND CAPPED AT $2500.00.
      *----------------------------------------------------------------*
       3600-LIMIT-CHECK.
           IF LBXT-CHECK-AMT < 25.00
               MOVE 25.00 TO LBXT-CHECK-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXT-CHECK-AMT > 2500.00
               MOVE 2500.00 TO LBXT-CHECK-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT YTD BY CHANNEL CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       3610-ACCUM-CHANNEL.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-CHANNEL-CD
                   ADD LBXM-REMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-CHANNEL-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-REMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON FEE AMT AT FEE PCT,
      * ACTUAL/365. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       3620-ACCRUE-FEE.
           IF LBXT-FEE-AMT > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXT-FEE-AMT * LBXM-FEE-PCT / 100 / 365
               ADD WS-WORK-AMT TO LBXM-EXCEPT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CYCLE CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3630-DESCRIBE-CYCLE.
           EVALUATE LBXM-CYCLE-CD
               WHEN 'EDV'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'EPM'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'FZY'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'XUU'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'YBH'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ASSESS THE EXCEPT YTD FEE ON THE WORK AMT.
      * WAIVED WHEN REVERSAL FLAG IS SET.
      * MINIMUM $10.00, CAPPED AT $75.00.
      *----------------------------------------------------------------*
       3640-ASSESS-REVERSAL.
           EVALUATE TRUE
               WHEN LBXT-REVERSAL-FLAG = 'Y'
                   MOVE ZERO TO LBXM-EXCEPT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   COMPUTE LBXM-EXCEPT-YTD ROUNDED = WS-WORK-AMT *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXM-CHANNEL-CD = 'B'
                   MOVE 15.00 TO LBXM-EXCEPT-YTD
               WHEN LBXM-CHANNEL-CD = 'H'
                   MOVE 20.00 TO LBXM-EXCEPT-YTD
               WHEN OTHER
                   COMPUTE LBXM-EXCEPT-YTD ROUNDED = WS-WORK-AMT *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-EXCEPT-YTD > 0 AND LBXM-EXCEPT-YTD < 10.00
               MOVE 10.00 TO LBXM-EXCEPT-YTD
           END-IF
           IF LBXM-EXCEPT-YTD > 75.00
               MOVE 75.00 TO LBXM-EXCEPT-YTD
           END-IF
           ADD LBXM-EXCEPT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * ASSESS THE DEPOSIT YTD FEE ON THE FLOAT AMT.
      * WAIVED WHEN EXEMPT FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $35.00.
      *----------------------------------------------------------------*
       3650-ASSESS-EXEMPT.
           EVALUATE TRUE
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   MOVE ZERO TO LBXM-DEPOSIT-YTD
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-NOTICE-SENT-FLAG = 'Y'
                   COMPUTE LBXM-DEPOSIT-YTD ROUNDED = LBXT-FLOAT-AMT *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXM-REASON-CD = 'GF'
                   MOVE 12.00 TO LBXM-DEPOSIT-YTD
               WHEN LBXM-REASON-CD = 'UK'
                   MOVE 20.00 TO LBXM-DEPOSIT-YTD
               WHEN OTHER
                   COMPUTE LBXM-DEPOSIT-YTD ROUNDED = LBXT-FLOAT-AMT *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-DEPOSIT-YTD > 0 AND LBXM-DEPOSIT-YTD < 5.00
               MOVE 5.00 TO LBXM-DEPOSIT-YTD
           END-IF
           IF LBXM-DEPOSIT-YTD > 35.00
               MOVE 35.00 TO LBXM-DEPOSIT-YTD
           END-IF
           ADD LBXM-DEPOSIT-YTD TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SEQ IS INCREMENTED FOR THE ITEM. REACHING 12
      * SETS EXEMPT FLAG.
      *----------------------------------------------------------------*
       3660-COUNT-SEQ.
           IF LBXT-SEQ < 9999999
               ADD 1 TO LBXT-SEQ
           END-IF
           IF LBXT-SEQ NOT < 12
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE REMIT FEE ON THE CHECK YTD.
      * WAIVED WHEN STOP FLAG IS SET.
      * MINIMUM $5.00, CAPPED AT $100.00.
      *----------------------------------------------------------------*
       3670-ASSESS-STOP.
           EVALUATE TRUE
               WHEN LBXM-STOP-FLAG = 'Y'
                   MOVE ZERO TO LBXM-REMIT-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   COMPUTE LBXM-REMIT-AMT ROUNDED = LBXM-CHECK-YTD *
                       LBXM-PENALTY-PCT / 200
               WHEN LBXT-TRAN-CD = 'RC'
                   MOVE 5.00 TO LBXM-REMIT-AMT
               WHEN LBXT-TRAN-CD = 'DP'
                   MOVE 20.00 TO LBXM-REMIT-AMT
               WHEN OTHER
                   COMPUTE LBXM-REMIT-AMT ROUNDED = LBXM-CHECK-YTD *
                       LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-REMIT-AMT > 0 AND LBXM-REMIT-AMT < 5.00
               MOVE 5.00 TO LBXM-REMIT-AMT
           END-IF
           IF LBXM-REMIT-AMT > 100.00
               MOVE 100.00 TO LBXM-REMIT-AMT
           END-IF
           ADD LBXM-REMIT-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * SPLIT CASH ON HAND YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3680-SPLIT-CASH-ON-HAND.
           COMPUTE WS-WORK-AMT = LBXM-CASH-ON-HAND-YTD / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-CASH-ON-HAND-YTD - (WS-WORK-AMT
               * 2)
           COMPUTE LBXM-FLOAT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT REMIT YTD INTO 12 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3690-SPLIT-REMIT.
           COMPUTE WS-WORK-AMT = LBXM-REMIT-YTD / 12
           COMPUTE WS-WORK-AMT-2 = LBXM-REMIT-YTD - (WS-WORK-AMT * 12)
           COMPUTE LBXT-CHECK-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST STMT DATE INTO BUCKETS
      * CURRENT, 1-15, 16-45, 46-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3700-AGE-LAST-STMT.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-STMT-DATE > 0 AND LBXM-LAST-STMT-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-STMT-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 45
                   ADD WS-WORK-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD WS-WORK-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * TRANSLATE TRAN CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3710-DESCRIBE-TRAN.
           EVALUATE LBXT-TRAN-CD
               WHEN 'RC'
                   MOVE 'REVIEW' TO WS-DESC
               WHEN 'DP'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'EX'
                   MOVE 'REVERSED' TO WS-DESC
               WHEN 'RJ'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'AD'
                   MOVE 'DEFAULT' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM NEXT DUE DATE INTO BUCKETS
      * CURRENT, 1-120, 121-150, 151-180 AND OVER 180 DAYS.
      *----------------------------------------------------------------*
       3720-AGE-NEXT-DUE.
           MOVE ZERO TO WS-DAYS
           IF LBXM-NEXT-DUE-DATE > 0 AND LBXM-NEXT-DUE-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-NEXT-DUE-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-DEPOSIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-DEPOSIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-DEPOSIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 180
                   ADD LBXM-DEPOSIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-DEPOSIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SPLIT CHECK AMT INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3730-SPLIT-CHECK.
           COMPUTE WS-WORK-AMT = LBXT-CHECK-AMT / 2
           COMPUTE WS-WORK-AMT-2 = LBXT-CHECK-AMT - (WS-WORK-AMT * 2)
           COMPUTE LBXM-FEE-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-45, 46-90, 91-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3740-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-REMIT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD LBXM-REMIT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 90
                   ADD LBXM-REMIT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-REMIT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-REMIT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM EFF DATE INTO BUCKETS
      * CURRENT, 1-15, 16-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       3750-AGE-EFF.
           MOVE ZERO TO WS-DAYS
           IF LBXT-EFF-DATE > 0 AND LBXT-EFF-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 15
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD WS-WORK-AMT-2 TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GF -> UK WHEN CLOSE DATE IS PAST
      * UK -> VR WHEN CLOSE DATE IS PAST
      * VR -> XG WHEN CLOSE DATE IS PAST
      * XG -> GF WHEN CLOSE DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3760-SET-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XG' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XG'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GF' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DAYS SINCE OPEN DATE. MORE THAN 120 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3770-CHECK-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBXM-OPEN-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 120
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REGION CD:
      * DVP -> FMZ WHEN MATURITY DATE IS PAST
      * FMZ -> KSL WHEN MATURITY DATE IS PAST
      * KSL -> NYK WHEN MATURITY DATE IS PAST
      * NYK -> DVP WHEN MATURITY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3780-SET-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'FMZ' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'FMZ'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'KSL' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'KSL'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'NYK' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'NYK'
                   IF LBXM-MATURITY-DATE > 0 AND LBXM-MATURITY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'DVP' TO LBXM-REGION-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/360, ROUNDED HALF UP. RESULT TO REMIT YTD.
      *----------------------------------------------------------------*
       3790-PRORATE-REMIT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-REMIT-YTD ROUNDED =
                   LBXM-FEE-AMT * WS-PERIOD-DAYS / 360
           ELSE
               MOVE ZERO TO LBXM-REMIT-YTD
           END-IF
           ADD LBXM-REMIT-YTD TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 15 CALENDAR DAYS SETS
      * HOLD FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3800-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF LBXT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 15
               MOVE 'Y' TO LBXM-HOLD-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE CASH ON HAND AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FEE AMT.
      *----------------------------------------------------------------*
       3810-PRORATE-FEE.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXT-FEE-AMT ROUNDED =
                   LBXT-CASH-ON-HAND-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXT-FEE-AMT
           END-IF
           ADD LBXT-FEE-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 90 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3820-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF LBXT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 90
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE CHANNEL CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3830-DESCRIBE-CHANNEL.
           EVALUATE LBXM-CHANNEL-CD
               WHEN 'B'
                   MOVE 'PENDING' TO WS-DESC
               WHEN 'H'
                   MOVE 'ACTIVE' TO WS-DESC
               WHEN 'Z'
                   MOVE 'PENDING' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT BAND TIER FOR THE CASH ON HAND AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3840-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-BAND-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXT-CASH-ON-HAND-AMT NOT < WS-EXCEPT-BAND-LOW(WS-TX)
                   MOVE WS-EXCEPT-BAND-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE POST DATE. MORE THAN 365 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3850-CHECK-POST.
           MOVE ZERO TO WS-DAYS
           IF LBXT-POST-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-POST-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 365
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * CHECK AMT OVER $5000.00 IS WRITTEN TO THE EXCEPTION
      * REPORT WITH SEVERITY M.
      *----------------------------------------------------------------*
       3860-FLAG-CHECK.
           IF LBXT-CHECK-AMT > 5000.00
               ADD 1 TO WS-EXCEPT-COUNT
               MOVE 'M' TO WS-EXC-SEVERITY
               MOVE LBXT-CHECK-AMT TO WS-EXC-AMOUNT
               PERFORM 8100-WRITE-EXCEPTION
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE MATURITY DATE. MORE THAN 30 CALENDAR DAYS SETS
      * EXEMPT FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3870-CHECK-MATURITY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-MATURITY-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-MATURITY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 30
               MOVE 'Y' TO LBXM-EXEMPT-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAYS SINCE EFF DATE. MORE THAN 60 CALENDAR DAYS SETS
      * AUTO RENEW FLAG AND COUNTS THE ITEM FOR REVIEW.
      *----------------------------------------------------------------*
       3880-CHECK-EFF.
           MOVE ZERO TO WS-DAYS
           IF LBXT-EFF-DATE > 0
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXT-EFF-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               IF DTU-OK
                   MOVE DTU-RESULT-NUMBER TO WS-DAYS
               END-IF
           END-IF
           IF WS-DAYS > 60
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
               ADD 1 TO WS-REVIEW-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SELECT THE FEE SCHED TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3890-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-FEE-SCHED-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 7
               IF LBXM-FEE-AMT NOT < WS-FEE-SCHED-LOW(WS-TX)
                   MOVE WS-FEE-SCHED-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $0.00 AND CAPPED AT $1000.00.
      *----------------------------------------------------------------*
       3900-LIMIT-DEPOSIT.
           IF LBXM-DEPOSIT-YTD < 0.00
               MOVE 0.00 TO LBXM-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-DEPOSIT-YTD > 1000.00
               MOVE 1000.00 TO LBXM-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ITEM CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS AUTO RENEW FLAG.
      *----------------------------------------------------------------*
       3910-COUNT-ITEM.
           IF LBXM-ITEM-CNT < 9999999
               ADD 1 TO LBXM-ITEM-CNT
           END-IF
           IF LBXM-ITEM-CNT NOT < 10
               MOVE 'Y' TO LBXM-AUTO-RENEW-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * TRANSLATE REGION CD TO ITS REPORT DESCRIPTION.
      *----------------------------------------------------------------*
       3920-DESCRIBE-REGION.
           EVALUATE LBXM-REGION-CD
               WHEN 'DVP'
                   MOVE 'COMPLETE' TO WS-DESC
               WHEN 'FMZ'
                   MOVE 'SUSPENDED' TO WS-DESC
               WHEN 'KSL'
                   MOVE 'CLOSED' TO WS-DESC
               WHEN 'NYK'
                   MOVE 'INACTIVE' TO WS-DESC
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-DESC
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PRORATE FEE AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO CHECK AMT.
      *----------------------------------------------------------------*
       3930-PRORATE-CHECK.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXM-CHECK-AMT ROUNDED =
                   LBXM-FEE-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXM-CHECK-AMT
           END-IF
           ADD LBXM-CHECK-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT BAND TIER FOR THE FLOAT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3940-SELECT-DISCOUNT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-BAND-RATE(1) TO LBXM-DISCOUNT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXT-FLOAT-AMT NOT < WS-EXCEPT-BAND-LOW(WS-TX)
                   MOVE WS-EXCEPT-BAND-RATE(WS-TX) TO LBXM-DISCOUNT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-DISCOUNT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-DISCOUNT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * EXCEPT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       3950-COUNT-EXCEPT.
           IF LBXM-EXCEPT-CNT < 9999999
               ADD 1 TO LBXM-EXCEPT-CNT
           END-IF
           IF LBXM-EXCEPT-CNT NOT < 10
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR CHANNEL CD:
      * B -> H WHEN LAST PAY DATE IS PAST
      * H -> Z WHEN LAST PAY DATE IS PAST
      * Z -> B WHEN LAST PAY DATE IS PAST
      * WITHIN 30 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       3960-SET-CHANNEL.
           EVALUATE LBXM-CHANNEL-CD
               WHEN 'B'
                   IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'H' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'H'
                   IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'Z' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'Z'
                   IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'B' TO LBXM-CHANNEL-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT BAND TIER FOR THE FEE AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       3970-SELECT-PENALTY-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-BAND-RATE(1) TO LBXM-PENALTY-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXT-FEE-AMT NOT < WS-EXCEPT-BAND-LOW(WS-TX)
                   MOVE WS-EXCEPT-BAND-RATE(WS-TX) TO LBXM-PENALTY-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-PENALTY-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-PENALTY-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3980-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = LBXM-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE LBXT-CASH-ON-HAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT CHECK YTD INTO 3 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       3990-SPLIT-CHECK.
           COMPUTE WS-WORK-AMT = LBXM-CHECK-YTD / 3
           COMPUTE WS-WORK-AMT-2 = LBXM-CHECK-YTD - (WS-WORK-AMT * 3)
           COMPUTE LBXT-CASH-ON-HAND-AMT = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PAYMENT CNT IS INCREMENTED FOR THE ITEM. REACHING 10
      * SETS REVERSAL FLAG.
      *----------------------------------------------------------------*
       4000-COUNT-PAYMENT.
           IF LBXM-PAYMENT-CNT < 9999999
               ADD 1 TO LBXM-PAYMENT-CNT
           END-IF
           IF LBXM-PAYMENT-CNT NOT < 10
               MOVE 'Y' TO LBXT-REVERSAL-FLAG
           END-IF.

      *----------------------------------------------------------------*
      * SPLIT FEE YTD INTO 2 EQUAL PARTS. THE ROUNDING
      * REMAINDER IS ADDED TO THE FIRST PART.
      *----------------------------------------------------------------*
       4010-SPLIT-FEE.
           COMPUTE WS-WORK-AMT = LBXM-FEE-YTD / 2
           COMPUTE WS-WORK-AMT-2 = LBXM-FEE-YTD - (WS-WORK-AMT * 2)
           COMPUTE LBXM-FLOAT-YTD = WS-WORK-AMT + WS-WORK-AMT-2
           IF WS-WORK-AMT-2 NOT = 0
               ADD 1 TO WS-ROUNDED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * DAILY INTEREST ON CHECK YTD AT DISCOUNT,
      * ACTUAL/360. NEGATIVE BALANCES EARN NOTHING.
      *----------------------------------------------------------------*
       4020-ACCRUE-CHECK.
           IF LBXM-CHECK-YTD > 0
               COMPUTE WS-WORK-AMT ROUNDED =
                   LBXM-CHECK-YTD * LBXM-DISCOUNT / 100 / 360
               ADD WS-WORK-AMT TO LBXM-DEPOSIT-AMT
               ADD WS-WORK-AMT TO WS-TOTAL-INTEREST
           ELSE
               ADD 1 TO WS-NEGATIVE-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * ASSESS THE CASH ON HAND FEE ON THE FEE AMT.
      * WAIVED WHEN PREFERRED FLAG IS SET.
      * MINIMUM $2.00, CAPPED AT $25.00.
      *----------------------------------------------------------------*
       4030-ASSESS-PREFERRED.
           EVALUATE TRUE
               WHEN LBXM-PREFERRED-FLAG = 'Y'
                   MOVE ZERO TO LBXM-CASH-ON-HAND-AMT
                   ADD 1 TO WS-WAIVED-COUNT
               WHEN LBXM-EXEMPT-FLAG = 'Y'
                   COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED = LBXT-FEE-AMT
                       * LBXM-PENALTY-PCT / 200
               WHEN LBXM-CHANNEL-CD = 'B'
                   MOVE 5.00 TO LBXM-CASH-ON-HAND-AMT
               WHEN LBXM-CHANNEL-CD = 'H'
                   MOVE 5.00 TO LBXM-CASH-ON-HAND-AMT
               WHEN OTHER
                   COMPUTE LBXM-CASH-ON-HAND-AMT ROUNDED = LBXT-FEE-AMT
                       * LBXM-PENALTY-PCT / 100
           END-EVALUATE
           IF LBXM-CASH-ON-HAND-AMT > 0 AND LBXM-CASH-ON-HAND-AMT < 2.00
               MOVE 2.00 TO LBXM-CASH-ON-HAND-AMT
           END-IF
           IF LBXM-CASH-ON-HAND-AMT > 25.00
               MOVE 25.00 TO LBXM-CASH-ON-HAND-AMT
           END-IF
           ADD LBXM-CASH-ON-HAND-AMT TO WS-TOTAL-FEES.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR REASON CD:
      * GF -> UK WHEN CLOSE DATE IS PAST
      * UK -> VR WHEN CLOSE DATE IS PAST
      * VR -> XG WHEN CLOSE DATE IS PAST
      * XG -> GF WHEN CLOSE DATE IS PAST
      * WITHIN 180 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4040-SET-REASON.
           EVALUATE LBXM-REASON-CD
               WHEN 'GF'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'UK' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'UK'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'VR' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'VR'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'XG' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'XG'
                   IF LBXM-CLOSE-DATE > 0 AND LBXM-CLOSE-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'GF' TO LBXM-REASON-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * SELECT THE EXCEPT BAND TIER FOR THE EXCEPT AMT.
      * THE LAST TIER WHOSE LOWER BOUND IS AT OR BELOW THE AMOUNT
      * WINS. AMOUNTS BELOW THE FIRST TIER TAKE THE FIRST TIER.
      *----------------------------------------------------------------*
       4050-SELECT-FEE-PCT.
           MOVE 1 TO WS-TX
           MOVE WS-EXCEPT-BAND-RATE(1) TO LBXM-FEE-PCT
           PERFORM VARYING WS-TX FROM 1 BY 1 UNTIL WS-TX > 6
               IF LBXM-EXCEPT-AMT NOT < WS-EXCEPT-BAND-LOW(WS-TX)
                   MOVE WS-EXCEPT-BAND-RATE(WS-TX) TO LBXM-FEE-PCT
                   MOVE WS-TX TO WS-TIER-USED
               END-IF
           END-PERFORM
           IF LBXM-FEE-PCT > WS-MAX-RATE
               MOVE WS-MAX-RATE TO LBXM-FEE-PCT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * REMIT AMT IS FLOORED AT $0.00 AND CAPPED AT $500.00.
      *----------------------------------------------------------------*
       4060-LIMIT-REMIT.
           IF LBXM-REMIT-AMT < 0.00
               MOVE 0.00 TO LBXM-REMIT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-REMIT-AMT > 500.00
               MOVE 500.00 TO LBXM-REMIT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM OPEN DATE INTO BUCKETS
      * CURRENT, 1-45, 46-60, 61-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4070-AGE-OPEN.
           MOVE ZERO TO WS-DAYS
           IF LBXM-OPEN-DATE > 0 AND LBXM-OPEN-DATE < CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-OPEN-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXT-FLOAT-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 45
                   ADD LBXT-FLOAT-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 60
                   ADD LBXT-FLOAT-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXT-FLOAT-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXT-FLOAT-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * ACCUMULATE REMIT YTD BY LOCKBOX CD FOR THE CONTROL REPORT.
      *----------------------------------------------------------------*
       4080-ACCUM-LOCKBOX.
           MOVE 'N' TO WS-FOUND
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CAT-USED OR WS-FOUND = 'Y'
               IF WS-CAT-CODE(WS-CX) = LBXM-LOCKBOX-CD
                   ADD LBXM-REMIT-YTD TO WS-CAT-AMT(WS-CX)
                   ADD 1 TO WS-CAT-CNT(WS-CX)
                   MOVE 'Y' TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = 'N' AND WS-CAT-USED < 50
               ADD 1 TO WS-CAT-USED
               MOVE LBXM-LOCKBOX-CD TO WS-CAT-CODE(WS-CAT-USED)
               MOVE LBXM-REMIT-YTD TO WS-CAT-AMT(WS-CAT-USED)
               MOVE 1 TO WS-CAT-CNT(WS-CAT-USED)
           END-IF.

      *----------------------------------------------------------------*
      * AGE THE ITEM FROM LAST PAY DATE INTO BUCKETS
      * CURRENT, 1-60, 61-120, 121-150 AND OVER 150 DAYS.
      *----------------------------------------------------------------*
       4090-AGE-LAST-PAY.
           MOVE ZERO TO WS-DAYS
           IF LBXM-LAST-PAY-DATE > 0 AND LBXM-LAST-PAY-DATE <
               CTL-BUSINESS-DATE
               INITIALIZE DATE-PARMS
               SET DTU-DAYS-BETWEEN TO TRUE
               MOVE LBXM-LAST-PAY-DATE TO DTU-DATE-1
               MOVE CTL-BUSINESS-DATE TO DTU-DATE-2
               CALL 'DATEUTIL' USING DATE-PARMS
               MOVE DTU-RESULT-NUMBER TO WS-DAYS
           END-IF
           EVALUATE TRUE
               WHEN WS-DAYS = 0
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(1)
               WHEN WS-DAYS NOT > 60
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(2)
               WHEN WS-DAYS NOT > 120
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(3)
               WHEN WS-DAYS NOT > 150
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(4)
               WHEN OTHER
                   ADD LBXM-FEE-AMT TO WS-AGE-AMT(5)
           END-EVALUATE.

      *----------------------------------------------------------------*
      * DEPOSIT YTD IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4100-LIMIT-DEPOSIT.
           IF LBXM-DEPOSIT-YTD < 25.00
               MOVE 25.00 TO LBXM-DEPOSIT-YTD
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-DEPOSIT-YTD > 10000.00
               MOVE 10000.00 TO LBXM-DEPOSIT-YTD
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * PRORATE DEPOSIT AMT FOR THE DAYS IN THE PERIOD,
      * ACTUAL/365, ROUNDED HALF UP. RESULT TO FLOAT AMT.
      *----------------------------------------------------------------*
       4110-PRORATE-FLOAT.
           IF WS-PERIOD-DAYS > 0
               COMPUTE LBXT-FLOAT-AMT ROUNDED =
                   LBXM-DEPOSIT-AMT * WS-PERIOD-DAYS / 365
           ELSE
               MOVE ZERO TO LBXT-FLOAT-AMT
           END-IF
           ADD LBXT-FLOAT-AMT TO WS-TOTAL-PRORATED.

      *----------------------------------------------------------------*
      * EXCEPT AMT IS FLOORED AT $25.00 AND CAPPED AT $10000.00.
      *----------------------------------------------------------------*
       4120-LIMIT-EXCEPT.
           IF LBXM-EXCEPT-AMT < 25.00
               MOVE 25.00 TO LBXM-EXCEPT-AMT
               ADD 1 TO WS-FLOORED-COUNT
           END-IF
           IF LBXM-EXCEPT-AMT > 10000.00
               MOVE 10000.00 TO LBXM-EXCEPT-AMT
               ADD 1 TO WS-CAPPED-COUNT
           END-IF.

      *----------------------------------------------------------------*
      * STATUS TRANSITIONS FOR BATCH TYPE CD:
      * C -> D WHEN POST DATE IS PAST
      * D -> F WHEN POST DATE IS PAST
      * F -> K WHEN POST DATE IS PAST
      * K -> U WHEN POST DATE IS PAST
      * WITHIN 90 DAYS OF THE DATE THE STATUS IS LEFT ALONE.
      *----------------------------------------------------------------*
       4130-SET-BATCH-TYPE.
           EVALUATE LBXM-BATCH-TYPE-CD
               WHEN 'C'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'D' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'D'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'F' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'F'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'K' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN 'K'
                   IF LBXT-POST-DATE > 0 AND LBXT-POST-DATE <
                       CTL-BUSINESS-DATE
                       MOVE 'U' TO LBXM-BATCH-TYPE-CD
                       ADD 1 TO WS-STATUS-CHANGES
                   END-IF
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-STATUS
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE LBX FIELDS TO LBWAIV AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7000-CALL-LBWAIV.
           INITIALIZE LBWAIV-PARMS
           MOVE LBXM-EXCEPT-AMT
             TO LBWAIV-EXCEPT-AMT
           MOVE LBXM-FLOAT-AMT
             TO LBWAIV-FLOAT-AMT
           MOVE LBXM-CHECK-YTD
             TO LBWAIV-CHECK-YTD
           MOVE LBXM-CASH-ON-HAND-YTD
             TO LBWAIV-CASH-ON-HAND-YTD
           MOVE LBXM-FLOAT-YTD
             TO LBWAIV-FLOAT-YTD
           MOVE LBXM-REMIT-YTD
             TO LBWAIV-REMIT-YTD
           MOVE LBXM-MATURITY-DATE
             TO LBWAIV-MATURITY-DATE
           MOVE LBXM-EXPIRY-DATE
             TO LBWAIV-EXPIRY-DATE
           MOVE LBXM-LAST-PAY-DATE
             TO LBWAIV-LAST-PAY-DATE
           MOVE LBXM-LAST-STMT-DATE
             TO LBWAIV-LAST-STMT-DATE
           MOVE LBXM-REASON-CD
             TO LBWAIV-REASON-CD
           MOVE LBXM-LOCKBOX-CD
             TO LBWAIV-LOCKBOX-CD
           MOVE LBXM-REGION-CD
             TO LBWAIV-REGION-CD
           MOVE LBXM-CHANNEL-CD
             TO LBWAIV-CHANNEL-CD
           MOVE LBXM-PREFERRED-FLAG
             TO LBWAIV-PREFERRED-FLAG
           MOVE LBXM-EXEMPT-FLAG
             TO LBWAIV-EXEMPT-FLAG
           MOVE LBXM-STOP-FLAG
             TO LBWAIV-STOP-FLAG
           MOVE LBXM-PAYMENT-CNT
             TO LBWAIV-PAYMENT-CNT
           MOVE LBXM-EXCEPT-CNT
             TO LBWAIV-EXCEPT-CNT
           MOVE LBXM-PENALTY-PCT
             TO LBWAIV-PENALTY-PCT
           MOVE LBXM-DISCOUNT
             TO LBWAIV-DISCOUNT
           MOVE LBXM-CONTACT
             TO LBWAIV-CONTACT
           MOVE LBXM-CUST-ID
             TO LBWAIV-CUST-ID
           CALL 'LBWAIV' USING LBWAIV-PARMS
           EVALUATE LBWAIV-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE LBWAIV-CHECK-YTD
                     TO LBXM-CHECK-YTD
                   MOVE LBWAIV-CASH-ON-HAND-YTD
                     TO LBXM-CASH-ON-HAND-YTD
                   MOVE LBWAIV-REMIT-YTD
                     TO LBXM-REMIT-YTD
                   MOVE LBWAIV-PREFERRED-FLAG
                     TO LBXM-PREFERRED-FLAG
                   MOVE LBWAIV-EXEMPT-FLAG
                     TO LBXM-EXEMPT-FLAG
                   MOVE LBWAIV-STOP-FLAG
                     TO LBXM-STOP-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RAIV' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7000-CALL-LBWAIV' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE LBX FIELDS TO LBFEE AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7020-CALL-LBFEE.
           INITIALIZE LBFEE-PARMS
           MOVE LBXM-EXCEPT-YTD
             TO LBFEE-EXCEPT-YTD
           MOVE LBXM-DEPOSIT-YTD
             TO LBFEE-DEPOSIT-YTD
           MOVE LBXM-REMIT-YTD
             TO LBFEE-REMIT-YTD
           MOVE LBXM-EXCEPT-AMT
             TO LBFEE-EXCEPT-AMT
           MOVE LBXM-FLOAT-YTD
             TO LBFEE-FLOAT-YTD
           MOVE LBXM-FLOAT-AMT
             TO LBFEE-FLOAT-AMT
           MOVE LBXM-CLOSE-DATE
             TO LBFEE-CLOSE-DATE
           MOVE LBXM-NEXT-DUE-DATE
             TO LBFEE-NEXT-DUE-DATE
           MOVE LBXM-LAST-PAY-DATE
             TO LBFEE-LAST-PAY-DATE
           MOVE LBXM-OPEN-DATE
             TO LBFEE-OPEN-DATE
           MOVE LBXM-LOCKBOX-CD
             TO LBFEE-LOCKBOX-CD
           MOVE LBXM-REASON-CD
             TO LBFEE-REASON-CD
           MOVE LBXM-BATCH-TYPE-CD
             TO LBFEE-BATCH-TYPE-CD
           MOVE LBXM-CHANNEL-CD
             TO LBFEE-CHANNEL-CD
           MOVE LBXM-STOP-FLAG
             TO LBFEE-STOP-FLAG
           MOVE LBXM-EXEMPT-FLAG
             TO LBFEE-EXEMPT-FLAG
           MOVE LBXM-NOTICE-SENT-FLAG
             TO LBFEE-NOTICE-SENT-FLAG
           MOVE LBXM-PAYMENT-CNT
             TO LBFEE-PAYMENT-CNT
           MOVE LBXM-ITEM-CNT
             TO LBFEE-ITEM-CNT
           MOVE LBXM-DISCOUNT
             TO LBFEE-DISCOUNT
           MOVE LBXM-PENALTY-PCT
             TO LBFEE-PENALTY-PCT
           MOVE LBXM-NAME
             TO LBFEE-NAME
           MOVE LBXM-ADDR
             TO LBFEE-ADDR
           CALL 'LBFEE' USING LBFEE-PARMS
           EVALUATE LBFEE-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE LBFEE-EXCEPT-YTD
                     TO LBXM-EXCEPT-YTD
                   MOVE LBFEE-EXCEPT-AMT
                     TO LBXM-EXCEPT-AMT
                   MOVE LBFEE-STOP-FLAG
                     TO LBXM-STOP-FLAG
                   MOVE LBFEE-NOTICE-SENT-FLAG
                     TO LBXM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RFEE' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7020-CALL-LBFEE' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

      *----------------------------------------------------------------*
      * PASS THE LBX FIELDS TO LBCAL AND APPLY THE RESULT.
      *----------------------------------------------------------------*
       7010-CALL-LBCAL.
           INITIALIZE LBCAL-PARMS
           MOVE LBXM-DEPOSIT-AMT
             TO LBCAL-DEPOSIT-AMT
           MOVE LBXM-DEPOSIT-YTD
             TO LBCAL-DEPOSIT-YTD
           MOVE LBXM-CHECK-AMT
             TO LBCAL-CHECK-AMT
           MOVE LBXM-CASH-ON-HAND-YTD
             TO LBCAL-CASH-ON-HAND-YTD
           MOVE LBXM-FLOAT-YTD
             TO LBCAL-FLOAT-YTD
           MOVE LBXM-REMIT-YTD
             TO LBCAL-REMIT-YTD
           MOVE LBXM-NOTICE-DATE
             TO LBCAL-NOTICE-DATE
           MOVE LBXM-NEXT-DUE-DATE
             TO LBCAL-NEXT-DUE-DATE
           MOVE LBXM-OPEN-DATE
             TO LBCAL-OPEN-DATE
           MOVE LBXM-EFFECTIVE-DATE
             TO LBCAL-EFFECTIVE-DATE
           MOVE LBXM-REGION-CD
             TO LBCAL-REGION-CD
           MOVE LBXM-BATCH-TYPE-CD
             TO LBCAL-BATCH-TYPE-CD
           MOVE LBXM-REASON-CD
             TO LBCAL-REASON-CD
           MOVE LBXM-LOCKBOX-CD
             TO LBCAL-LOCKBOX-CD
           MOVE LBXM-HOLD-FLAG
             TO LBCAL-HOLD-FLAG
           MOVE LBXM-PREFERRED-FLAG
             TO LBCAL-PREFERRED-FLAG
           MOVE LBXM-NOTICE-SENT-FLAG
             TO LBCAL-NOTICE-SENT-FLAG
           MOVE LBXM-EXCEPT-CNT
             TO LBCAL-EXCEPT-CNT
           MOVE LBXM-RETRY-CNT
             TO LBCAL-RETRY-CNT
           MOVE LBXM-PENALTY-PCT
             TO LBCAL-PENALTY-PCT
           MOVE LBXM-DISCOUNT
             TO LBCAL-DISCOUNT
           MOVE LBXM-DESC
             TO LBCAL-DESC
           MOVE LBXM-CONTACT
             TO LBCAL-CONTACT
           CALL 'LBCAL' USING LBCAL-PARMS
           EVALUATE LBCAL-RETURN-CODE
               WHEN 00
               WHEN 04
                   MOVE LBCAL-DEPOSIT-AMT
                     TO LBXM-DEPOSIT-AMT
                   MOVE LBCAL-FLOAT-YTD
                     TO LBXM-FLOAT-YTD
                   MOVE LBCAL-REMIT-YTD
                     TO LBXM-REMIT-YTD
                   MOVE LBCAL-HOLD-FLAG
                     TO LBXM-HOLD-FLAG
                   MOVE LBCAL-PREFERRED-FLAG
                     TO LBXM-PREFERRED-FLAG
                   MOVE LBCAL-NOTICE-SENT-FLAG
                     TO LBXM-NOTICE-SENT-FLAG
                   CONTINUE
               WHEN 08
                   MOVE 'RCAL' TO WS-REJECT-CODE
                   ADD 1 TO WS-REJECT-COUNT
               WHEN OTHER
                   MOVE '7010-CALL-LBCAL' TO ABND-PARAGRAPH
                   PERFORM 9900-ABEND
           END-EVALUATE.

       8000-DETAIL-LINE.
           MOVE LBXT-KEY TO RD-KEY
           MOVE LBXT-TRAN-CD TO RD-CODE
           MOVE LBXT-FLOAT-AMT TO RD-AMT
           MOVE WS-REJECT-CODE TO RD-REJECT
           MOVE WS-DESC TO RD-DESC
           WRITE REPORT-REC FROM WS-RPT-DETAIL.

       8100-WRITE-EXCEPTION.
           MOVE SPACES TO WS-RPT-DETAIL
           MOVE LBXT-KEY TO RD-KEY
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
           DISPLAY 'LBXBILL READ=' WS-READ-COUNT ' REJECTED='
               WS-REJECT-COUNT
           IF WS-REJECT-COUNT > 0
               MOVE 4 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       9900-ABEND.
           MOVE 'LBXBILL' TO ABND-PROGRAM
           MOVE 16 TO ABND-CODE
           CALL 'ABENDRTN' USING ABEND-PARMS.
